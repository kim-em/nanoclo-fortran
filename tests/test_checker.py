"""Every good export must accept; every bad export must reject. Declines fail."""
import json
import argparse
import os
from pathlib import Path
import subprocess
import resource
import tempfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("exe", type=Path)
p.add_argument("exports", type=Path, nargs="*")
p.add_argument("--jobs", type=int, default=1)
args = p.parse_args()
exe = str(args.exe.resolve())
os.environ.setdefault("OMP_STACKSIZE", "1G")
meta = {"meta": {"exporter": {"name": "lean4export", "version": "3.1.0"},
                 "format": {"version": "3.1.0"}, "lean": {"version": "4.29.1", "githash": "0"*40}}}
base = [meta, {"in": 1, "str": {"pre": 0, "str": "test"}},
        {"il": 1, "succ": 0}, {"ie": 0, "sort": 0}, {"ie": 1, "sort": 1}]
checks = 0
# PLAN.md specifies an unlimited stack. Raise the soft limit to the available
# hard limit for child checkers, including the deep Church-numeral fixture.
_, stack_hard = resource.getrlimit(resource.RLIMIT_STACK)
resource.setrlimit(resource.RLIMIT_STACK, (stack_hard, stack_hard))


def definition(ty, val, params=()):
    return {"def": {"name": 1, "type": ty, "value": val, "levelParams": list(params),
                    "safety": "safe", "hints": "opaque"}}


def run(path, expected):
    global checks
    r = subprocess.run([exe, f"--jobs={args.jobs}", str(path)], capture_output=True, text=True, timeout=600)
    assert r.returncode == expected, (str(path), expected, r.returncode, r.stderr)
    checks += 1


with tempfile.TemporaryDirectory() as temp:
    path = Path(temp) / "test.ndjson"

    def case(records, expected):
        path.write_text("\n".join(json.dumps(r) for r in records))
        run(path, expected)

    case([meta], 0)
    case([*base, definition(1, 0)], 0)
    case([*base, definition(0, 1)], 1)
    case([*base, {"ie": 2, "const": {"name": 1, "us": []}}, definition(1, 2)], 1)
    case([*base, {"in": 2, "str": {"pre": 0, "str": "future"}},
          {"ie": 2, "const": {"name": 2, "us": []}}, definition(1, 2),
          {"axiom": {"name": 2, "type": 1, "levelParams": [], "isUnsafe": False}}], 1)
    case([*base, {"ie": 2, "app": {"fn": 0, "arg": 0}}, definition(1, 2)], 1)
    case([*base, {"ie": 2, "bvar": 0}, definition(1, 2)], 1)
    case([*base, {"il": 2, "param": 1}, {"ie": 2, "sort": 2}, definition(2, 0)], 1)
    case([*base, definition(1, 0, (1, 1))], 1)
    case([*base, {"ie": 2, "bvar": 0},
          {"ie": 3, "letE": {"name": 0, "type": 1, "value": 0, "body": 2, "nondep": False}},
          definition(1, 3)], 0)
    case([*base, {"ie": 2, "bvar": 0},
          {"ie": 3, "letE": {"name": 0, "type": 0, "value": 0, "body": 2, "nondep": False}},
          definition(1, 3)], 1)
    case([*base, {"axiom": {"name": 1, "type": 1, "levelParams": [], "isUnsafe": True}}], 1)
    case([*base, {"inductive": {"types": [], "ctors": [], "recs": []}}], 0)

for root in args.exports:
    files = [root] if root.is_file() else sorted(root.rglob("*.ndjson"))
    for path in files:
        expected = 1 if "bad" in path.parts else 0
        run(path, expected)
print(f"checker: {checks} cases passed; zero declines")
