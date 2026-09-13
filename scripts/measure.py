"""Measure each supplied stream once, retaining failures and raw evidence.

Run inside `nix shell nixpkgs#time -c ...` if GNU time is not on PATH.
Hardware instructions count only the checker; GNU time wraps perf for RSS.
"""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import resource
import shutil
import signal
import subprocess
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--checker', choices=['soa', 'aos', 'rust', 'official'], required=True)
p.add_argument('--binary', type=Path, required=True)
p.add_argument('--config', type=Path, help='Rust JSON config')
p.add_argument('--jobs', type=int, default=4)
p.add_argument('--exports', type=Path, nargs='+', required=True)
p.add_argument('--output', type=Path, required=True)
p.add_argument('--timeout', type=int, default=3600)
p.add_argument('--memory-kib', type=int, default=22000000)
p.add_argument('--run-pending', action='store_true', help='run a previously reserved cell')
p.add_argument('--only', action='append', default=[], help='relative file name, e.g. good/init.ndjson')
a = p.parse_args()
a.binary = a.binary.resolve()
a.output.mkdir(parents=True, exist_ok=True)
gnu_time = shutil.which('time')
if not gnu_time:
    raise SystemExit('GNU time is missing; run under nix shell nixpkgs#time')

def sha(path):
    with path.open('rb') as f:
        return hashlib.file_digest(f, 'sha256').hexdigest()

binary_hash = sha(a.binary)
env = {**os.environ, 'LC_ALL': 'C', 'OMP_STACKSIZE': '1G', 'LEAN_NUM_THREADS': str(a.jobs)}
for k in list(env):
    if k.startswith('NANOCLO_'):
        env.pop(k)
config = None
if a.checker == 'rust':
    if not a.config:
        raise SystemExit('--config is required for Rust')
    config = json.loads(a.config.read_text())
    if config['num_threads'] != a.jobs:
        raise SystemExit('Rust config thread count differs from --jobs')

def limits():
    _, hard = resource.getrlimit(resource.RLIMIT_STACK)
    resource.setrlimit(resource.RLIMIT_STACK, (hard, hard))
    if a.memory_kib:
        n = a.memory_kib * 1024
        resource.setrlimit(resource.RLIMIT_AS, (n, n))

files = []
for root in a.exports:
    for path in [root] if root.is_file() else sorted(root.rglob('*.ndjson')):
        name = path.name if root.is_file() else str(path.relative_to(root))
        if not a.only or name in a.only:
            files.append((name, path.resolve()))
results = []
for name, path in files:
    prefix = a.output / name.removesuffix('.ndjson')
    prefix.parent.mkdir(parents=True, exist_ok=True)
    result_file = prefix.with_suffix('.measurement.json')
    identity = {'file': name, 'input_sha256': sha(path), 'checker': a.checker,
                'binary_sha256': binary_hash, 'jobs': a.jobs, 'config': config,
                'memory_limit_kib': a.memory_kib, 'timeout_seconds': a.timeout}
    if result_file.exists():
        saved = json.loads(result_file.read_text())
        if any(saved.get(k) != v for k, v in identity.items()):
            raise SystemExit(f'existing measurement has different inputs: {result_file}')
        if not saved.get('pending', False):
            results.append(saved)
            print('retained', name, flush=True)
            continue
        if not a.run_pending:
            raise SystemExit(f'measurement is reserved: {result_file}')
    perf_log = prefix.with_suffix('.perf')
    time_log = prefix.with_suffix('.time')
    stdout_log = prefix.with_suffix('.stdout')
    stderr_log = prefix.with_suffix('.stderr')
    command = [str(a.binary)]
    if a.checker in ('soa', 'aos'):
        command += [f'--jobs={a.jobs}', str(path)]
    elif a.checker == 'rust':
        command += [str(a.config.resolve())]
    else:
        command += [str(path)]
    wrapped = [gnu_time, '-v', '-o', str(time_log), 'perf', 'stat', '-x', ';',
               '-e', 'instructions:u', '-o', str(perf_log), '--', *command]
    t0 = time.monotonic()
    timed_out = False
    with path.open('rb') as source, stdout_log.open('wb') as out, stderr_log.open('wb') as err:
        proc = subprocess.Popen(wrapped, stdin=source if a.checker == 'rust' else subprocess.DEVNULL,
                                stdout=out, stderr=err, env=env, preexec_fn=limits, start_new_session=True)
        try:
            status = proc.wait(timeout=a.timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            os.killpg(proc.pid, signal.SIGTERM)
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait()
            status = 124
    elapsed = time.monotonic() - t0
    instructions = rss = None
    if perf_log.exists():
        for row in csv.reader(perf_log.read_text().splitlines(), delimiter=';'):
            if len(row) >= 3 and row[2] == 'instructions:u' and row[0].strip().isdigit():
                instructions = int(row[0])
    if time_log.exists():
        for line in time_log.read_text().splitlines():
            if 'Maximum resident set size (kbytes):' in line:
                rss = int(line.rsplit(':', 1)[1])
    if sha(a.binary) != binary_hash:
        raise SystemExit('checker binary changed during measurement')
    expected = 1 if 'bad' in Path(name).parts else 0
    verdict = (0 if status == 0 else 1) if a.checker in ('rust', 'official') else status
    result = {**identity, 'command': command, 'process_status': status, 'timed_out': timed_out,
              'verdict_correct': not timed_out and verdict == expected,
              'instructions': instructions, 'virtual_cpu_seconds': instructions / 6e9 if instructions else None,
              'peak_rss_kib': rss, 'wall_seconds': elapsed,
              'raw': {k: str(v) for k, v in [('perf', perf_log), ('time', time_log),
                                             ('stdout', stdout_log), ('stderr', stderr_log)]}}
    result_file.write_text(json.dumps(result, indent=2) + '\n')
    results.append(result)
    print(name, status, instructions, rss, flush=True)
all_results = [json.loads(p.read_text()) for p in sorted(a.output.rglob('*.measurement.json'))]
(a.output / 'results.json').write_text(json.dumps(all_results, indent=2) + '\n')
if any(not r['verdict_correct'] or r['instructions'] is None for r in results):
    raise SystemExit(1)
