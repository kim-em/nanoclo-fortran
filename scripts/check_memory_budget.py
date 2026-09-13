"""Run an accepted export inside an existing cgroup and retain memory evidence.

Launch through systemd-run with MemoryMax, MemorySwapMax=0 and LimitSTACK=infinity.
The checker and input must be supplied explicitly; this script never changes
cgroup limits or other processes.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--binary', type=Path, required=True)
p.add_argument('--input', type=Path, required=True)
p.add_argument('--output', type=Path, required=True)
p.add_argument('--memory-bytes', type=int, default=16000000000)
p.add_argument('--jobs', type=int, default=4)
a = p.parse_args()
binary, source = a.binary.resolve(), a.input.resolve()
a.output.parent.mkdir(parents=True, exist_ok=True)

def sha(path):
    with path.open('rb') as f:
        return hashlib.file_digest(f, 'sha256').hexdigest()

cgroup = Path('/sys/fs/cgroup') / Path('/proc/self/cgroup').read_text().strip().split('::', 1)[1].lstrip('/')
def memory():
    return {name: (cgroup / name).read_text().strip() for name in
            ['memory.max', 'memory.swap.max', 'memory.peak', 'memory.events', 'memory.swap.peak', 'memory.current']
            if (cgroup / name).exists()}

record = {'binary_sha256': sha(binary), 'input_sha256': sha(source), 'jobs': a.jobs,
          'cgroup': str(cgroup), 'before': memory(),
          'command': [str(binary), f'--jobs={a.jobs}', str(source)],
          'OMP_STACKSIZE': '1G', 'memory_limit_bytes': a.memory_bytes, 'swap_limit_bytes': 0}
if record['before']['memory.max'] != str(a.memory_bytes) or record['before']['memory.swap.max'] != '0':
    raise SystemExit('launch inside a cgroup with the requested MemoryMax and MemorySwapMax=0')
start = time.monotonic()
with a.output.with_suffix('.stdout').open('w') as out, a.output.with_suffix('.stderr').open('w') as err:
    r = subprocess.run(record['command'], stdout=out, stderr=err, env={**os.environ, 'OMP_STACKSIZE': '1G'})
record.update(process_status=r.returncode, wall_seconds=time.monotonic()-start, after=memory())
if sha(binary) != record['binary_sha256']:
    raise SystemExit('checker binary changed during the run')
a.output.write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps(record, indent=2), flush=True)
raise SystemExit(0 if r.returncode == 0 else 1)
