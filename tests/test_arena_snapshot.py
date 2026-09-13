"""Check the pinned small arena corpus, including nanoclo's open-outcome verdicts."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import subprocess
import tarfile
import tempfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('binary', type=Path)
p.add_argument('--jobs', type=int, default=4)
a = p.parse_args()
binary = a.binary.resolve()
fixtures = Path(__file__).resolve().parent / 'fixtures'
manifest = json.loads((fixtures / 'arena-small.json').read_text())
archive = fixtures / 'arena-small.tar.gz'
assert hashlib.sha256(archive.read_bytes()).hexdigest() == manifest['archive_sha256']
_, hard = resource.getrlimit(resource.RLIMIT_STACK)
resource.setrlimit(resource.RLIMIT_STACK, (hard, hard))
env = {**os.environ, 'OMP_STACKSIZE': '1G'}
with tempfile.TemporaryDirectory(prefix='nanoclo-arena-') as temp:
    root = Path(temp)
    with tarfile.open(archive) as tar:
        tar.extractall(root, filter='data')
    assert {str(f.relative_to(root)) for f in root.rglob('*.ndjson')} == {c['file'] for c in manifest['cases']}
    for case in manifest['cases']:
        path = root / case['file']
        assert hashlib.sha256(path.read_bytes()).hexdigest() == case['sha256'], case['test']
        expected = case['pinned_nanoclo_status']
        if case['outcome'] != 'either':
            assert expected == {'accept': 0, 'reject': 1}[case['outcome']]
        r = subprocess.run([str(binary), f'--jobs={a.jobs}', str(path)],
                           capture_output=True, text=True, env=env, timeout=600)
        assert r.returncode == expected, (case['test'], expected, r.returncode, r.stderr)
print(f"arena snapshot: {len(manifest['cases'])} cases passed; zero declines; jobs={a.jobs}")
