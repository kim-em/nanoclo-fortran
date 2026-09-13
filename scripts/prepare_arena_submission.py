"""Write a portable, revision-pinned arena checker definition for a release."""
import argparse
from pathlib import Path
import re

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--revision', required=True, help='full published commit SHA')
p.add_argument('--output', type=Path, required=True)
a = p.parse_args()
if not re.fullmatch(r'[a-fA-F0-9]{40}', a.revision):
    p.error('--revision must be a full 40-character commit SHA')
a.output.parent.mkdir(parents=True, exist_ok=True)
a.output.write_text(f'''description: |
  Fortran 2018 translation of nanoclo's delayed-substitution Lean 4 kernel.
  Uses struct-of-arrays arenas, interned closure environments, and private
  OpenMP worker contexts with shared immutable exported expressions.
  Follows nanoclo 4cdd12f; algorithm fidelity is checked with all 26 counters
  on every Init declaration. Supports nested inductives and Nat/String literals.
url: https://github.com/kim-em/nanoclo-fortran
ref: main
rev: "{a.revision.lower()}"
threads: 4
build: nix develop --command make
run: |
  ulimit -s unlimited
  export OMP_STACKSIZE=1G
  exec timeout 3600 ./build/nanoclo-fortran --jobs=4 "$IN"
''')
print(a.output)
