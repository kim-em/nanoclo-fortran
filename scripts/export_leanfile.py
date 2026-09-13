"""Reproduce an arena leanfile export in an isolated Lake project."""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import yaml

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('arena', type=Path)
p.add_argument('test')
p.add_argument('exporter', type=Path)
p.add_argument('work', type=Path)
p.add_argument('output', type=Path)
a = p.parse_args()
config = yaml.safe_load((a.arena / 'tests' / (a.test + '.yaml')).read_text())
a.work.mkdir(parents=True, exist_ok=True)
shutil.copy(a.arena / config['leanfile'], a.work / 'Test.lean')
shutil.copy(a.arena / 'tests/lean-toolchain', a.work / 'lean-toolchain')
(a.work / 'lakefile.toml').write_text('name = "test"\n\n[[lean_lib]]\nname = "Test"\n')
subprocess.run(['lake', 'build', 'Test'], cwd=a.work, check=True)
a.output.parent.mkdir(parents=True, exist_ok=True)
command = ['lake', 'env', str(a.exporter.resolve()), 'Test']
if config.get('export-decls'):
    command += ['--', *config['export-decls']]
with a.output.open('wb') as f:
    subprocess.run(command, cwd=a.work, stdout=f, check=True)
with a.output.open('rb') as f:
    digest = hashlib.file_digest(f, 'sha256').hexdigest()
record = {'test': a.test, 'sha256': digest, 'bytes': a.output.stat().st_size,
          'arena_revision': subprocess.check_output(['git', '-C', str(a.arena), 'rev-parse', 'HEAD'], text=True).strip(),
          'config': config, 'toolchain': (a.work / 'lean-toolchain').read_text().strip()}
a.output.with_suffix('.provenance.json').write_text(json.dumps(record, indent=2) + '\n')
print(record, flush=True)
