"""Cross-check limb arithmetic with Python's independent arbitrary-size integers."""
import math
from pathlib import Path
import random
import subprocess
import sys

rng = random.Random(782493)
ops = {'add': lambda x,y:x+y, 'sub':lambda x,y:max(0,x-y), 'mul':lambda x,y:x*y,
       'div':lambda x,y:x//y if y else 0, 'mod':lambda x,y:x%y if y else x,
       'gcd':math.gcd, 'and':lambda x,y:x&y, 'or':lambda x,y:x|y, 'xor':lambda x,y:x^y,
       'shl':lambda x,y:x<<y, 'shr':lambda x,y:x>>y, 'pow':pow}
cases=[]
bounds=[0,1,2,65535,65536,65537,2**31-1,2**31,2**32-1,2**32,2**32+1,2**63-1,2**63,2**64-1,2**64,2**128-1]
for op in list(ops)[:9]:
    for x in bounds:
        for y in bounds:
            cases.append((op,x,y))
for _ in range(1200):
    op=rng.choice(list(ops)[:9]); x=rng.getrandbits(rng.randrange(1,2049)); y=rng.getrandbits(rng.randrange(1,1025))
    cases.append((op,x,y))
for _ in range(200):
    x=rng.getrandbits(rng.randrange(1,1025)); y=rng.randrange(0,2049)
    cases.extend([('shl',x,y),('shr',x,y),('pow',rng.randrange(0,65537),rng.randrange(0,100))])
raw=''.join(f'{op} {x} {y}\n' for op,x,y in cases)
r=subprocess.run([str(Path(sys.argv[1]).resolve())],input=raw,capture_output=True,text=True,timeout=60)
assert r.returncode==0,r.stderr
actual=r.stdout.splitlines()
assert len(actual)==len(cases),(len(actual),len(cases))
for case,line in zip(cases,actual):
    op,x,y=case; expected=ops[op](x,y)
    assert int(line)==expected,(case,line,expected)
print(f'bignums: {len(cases)} arithmetic comparisons passed')
