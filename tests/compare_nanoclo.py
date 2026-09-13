"""Compare real checker verdicts and upstream counter output on the same files.

Use --non-inductive explicitly for the current fragment checkpoint. Without
it, unsupported files are reported as mismatches, never filtered as success.
Rust's nonzero exit is mapped to reject as in the local arena nanoclo.yaml;
raw process statuses are also retained in the evidence artifact.
"""
import argparse
import hashlib
import gzip
import json
import os
import resource
from pathlib import Path
import subprocess

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("--rust", type=Path, required=True)
p.add_argument("--port", type=Path, required=True)
p.add_argument("--config", type=Path, required=True)
p.add_argument("--exports", type=Path, required=True)
p.add_argument("--output", type=Path, required=True)
p.add_argument("--non-inductive", action="store_true")
p.add_argument("--port-arg", action="append", default=[])
p.add_argument("--all-declaration-counters", action="store_true",
               help="use a Rust baseline produced by scripts/instrument_nanoclo.py")
p.add_argument("--timeout", type=int, default=60)
args = p.parse_args()
# Match the arena stack setup for deep recursive conversion fixtures.
_, stack_hard = resource.getrlimit(resource.RLIMIT_STACK)
resource.setrlimit(resource.RLIMIT_STACK, (stack_hard, stack_hard))
env = {**os.environ, "NANOCLO_CTRS": "1", "NANOCLO_DECLCTRS": "1"}
if args.all_declaration_counters:
    env["NANOCLO_DECLCTRS_ALL"] = "1"

def binary_hash(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

rust_hash, port_hash = binary_hash(args.rust), binary_hash(args.port)
results = []
excluded = 0
files = [args.exports] if args.exports.is_file() else sorted(args.exports.rglob("*.ndjson"))
for path in files:
    data = path.read_bytes()
    if args.non_inductive:
        records = [json.loads(line) for line in data.splitlines()]
        if any(any(k in r for k in ("inductive", "quot", "ctor", "rec")) for r in records):
            excluded += 1
            continue
    rust = subprocess.run([str(args.rust.resolve()), str(args.config.resolve())],
                          input=data, capture_output=True, env=env, timeout=args.timeout)
    port = subprocess.run([str(args.port.resolve()), *args.port_arg, str(path.resolve())],
                          capture_output=True, env=env, timeout=args.timeout)

    def counters(r):
        return [line.rstrip() for line in r.stderr.decode().splitlines()
                if line.startswith(("DC\t", "DC26\t", "CTRS "))]

    rust_verdict = 0 if rust.returncode == 0 else 1
    rc, pc = counters(rust), counters(port)
    counter_equal = None
    if rust.returncode == 0 and port.returncode == 0:
        counter_equal = bool(rc) and rc == pc
        if args.all_declaration_counters:
            counter_equal = counter_equal and any(line.startswith("DC26\t") for line in rc)
    results.append({"file": path.name if args.exports.is_file() else str(path.relative_to(args.exports)),
                    "sha256": hashlib.sha256(data).hexdigest(),
                    "rust_process_status": rust.returncode,
                    "port_process_status": port.returncode,
                    "verdict_equal": rust_verdict == port.returncode,
                    "counters_equal": counter_equal,
                    "rust_counters": rc, "port_counters": pc})
if (rust_hash, port_hash) != (binary_hash(args.rust), binary_hash(args.port)):
    raise SystemExit("checker binary changed during comparison; no evidence artifact written")
report = {"scope": "non-inductive fragment" if args.non_inductive else "entire supplied corpus",
          "counter_coverage": {"per_declaration": "all 26" if args.all_declaration_counters else ["whnf_core", "def_eq", "infer"], "run_total_count": 26},
          "rust_binary_sha256": rust_hash,
          "port_binary_sha256": port_hash,
          "excluded_files": excluded,
          "port_arguments": args.port_arg,
          "rust_config": json.loads(args.config.read_text()),
          "results": results}
args.output.parent.mkdir(parents=True, exist_ok=True)
serialized = (json.dumps(report, indent=2) + "\n").encode()
if args.output.suffix == '.gz':
    args.output.write_bytes(gzip.compress(serialized, mtime=0))
else:
    args.output.write_bytes(serialized)
verdicts = sum(r["verdict_equal"] for r in results)
accepted = [r for r in results if r["counters_equal"] is not None]
matching = sum(r["counters_equal"] for r in accepted)
print(f"verdicts: {verdicts}/{len(results)}; accepted-stream counters: {matching}/{len(accepted)}; excluded: {excluded}")
if verdicts != len(results) or matching != len(accepted):
    raise SystemExit(1)
