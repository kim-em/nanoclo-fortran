"""Reproduce the eight large arena exports from pinned source revisions.

Requires git, elan/Lake, and Python PyYAML. Existing source checkouts are never
reset or overwritten. Export files are installed only after successful export.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import yaml

EXPORTER_REV = '411dce7db58a3afc60ecab2d211acd1042b593dc'
NAMES = ['init', 'std', 'cedar', 'cslib', 'con-leche', 'mathlib',
         'perf/magma-string-n4', 'perf/magma-string-pair-n9']
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--arena', type=Path, required=True)
p.add_argument('--work', type=Path, required=True)
p.add_argument('--output', type=Path, required=True)
p.add_argument('--only', choices=NAMES, action='append')
p.add_argument('--jobs', type=int, default=4)
a = p.parse_args()
a.arena = a.arena.resolve(); a.work = a.work.resolve(); a.output = a.output.resolve()
env = {**os.environ, 'LEAN_NUM_THREADS': str(a.jobs)}

def run(args, cwd=None, **kwargs):
    return subprocess.run(args, cwd=cwd, check=True, env=env, **kwargs)

def revision(directory):
    return subprocess.check_output(['git', '-C', str(directory), 'rev-parse', 'HEAD'], text=True).strip()

def checkout(url, rev, directory):
    if directory.exists():
        if revision(directory) != rev:
            raise SystemExit(f'existing checkout has a different revision: {directory}')
        return
    directory.mkdir(parents=True)
    run(['git', 'init', '-q', str(directory)])
    run(['git', '-C', str(directory), 'remote', 'add', 'origin', url])
    run(['git', '-C', str(directory), 'fetch', '--depth=1', 'origin', rev])
    run(['git', '-C', str(directory), 'checkout', '--detach', 'FETCH_HEAD'])

def exporter(toolchain):
    directory = a.work / 'exporters' / toolchain.replace('/', '_').replace(':', '_')
    checkout('https://github.com/leanprover/lean4export', EXPORTER_REV, directory)
    (directory / 'lean-toolchain').write_text(toolchain + '\n')
    run(['lake', 'build'], directory)
    return directory / '.lake/build/bin/lean4export'

arena_revision = revision(a.arena)
for name in a.only or NAMES:
    config = yaml.safe_load((a.arena / 'tests' / (name + '.yaml')).read_text())
    output = a.output / 'good' / (name + '.ndjson')
    provenance = output.with_suffix('.provenance.json')
    if output.exists() and provenance.exists():
        saved = json.loads(provenance.read_text())
        with output.open('rb') as f:
            digest = hashlib.file_digest(f, 'sha256').hexdigest()
        if saved['sha256'] != digest or saved['config'] != config:
            raise SystemExit(f'existing export has different inputs: {output}')
        print('retained', output, flush=True)
        continue
    if output.exists():
        raise SystemExit(f'existing export has no provenance: {output}')
    if 'leanfile' in config:
        source = a.work / 'leanfiles' / name
        source.mkdir(parents=True, exist_ok=True)
        shutil.copy(a.arena / config['leanfile'], source / 'Test.lean')
        shutil.copy(a.arena / 'tests/lean-toolchain', source / 'lean-toolchain')
        (source / 'lakefile.toml').write_text('name = "test"\n\n[[lean_lib]]\nname = "Test"\n')
        module = 'Test'
    else:
        source = a.work / 'sources' / name
        checkout(config['url'], config['rev'], source)
        if name == 'cedar':
            source = source / 'cedar-lean'
        module = config['module']
        if name in ('mathlib', 'cslib'):
            run(['lake', 'exe', 'cache', 'get'], source)
    toolchain = (source / 'lean-toolchain').read_text().strip()
    binary = exporter(toolchain)
    run(['lake', 'build', module], source)
    command = ['lake', 'env', str(binary), module]
    if config.get('export-decls'):
        command += ['--', *config['export-decls']]
    output.parent.mkdir(parents=True, exist_ok=True)
    partial = output.with_suffix('.partial')
    with partial.open('wb') as f:
        run(command, source, stdout=f)
    partial.replace(output)
    with output.open('rb') as f:
        digest = hashlib.file_digest(f, 'sha256').hexdigest()
    record = {'test': name, 'sha256': digest, 'bytes': output.stat().st_size,
              'arena_revision': arena_revision, 'exporter_revision': EXPORTER_REV,
              'config': config, 'toolchain': toolchain}
    provenance.write_text(json.dumps(record, indent=2) + '\n')
    print(name, digest, output.stat().st_size, flush=True)
