"""Integration tests; optional real exports are compared with Python's JSON parser.

Scanning deliberately does not imply kernel acceptance. Malformed name/level
references decline; missing files are errors; the empty environment is accepted.
"""
import collections
import json
from pathlib import Path
import subprocess
import sys
import tempfile

exe = str(Path(sys.argv[1]).resolve())
meta = {"meta": {"exporter": {"name": "lean4export", "version": "3.1.0"},
                 "format": {"version": "3.1.0"},
                 "lean": {"githash": "0" * 40, "version": "4.29.1"}}}
checks = 0


def run(path, code=0, scan=True):
    global checks
    result = subprocess.run([exe, *(["--scan"] if scan else []), str(path)],
                            capture_output=True, text=True)
    assert result.returncode == code, (path, result.returncode, result.stderr)
    checks += 1
    return result.stdout


with tempfile.TemporaryDirectory() as directory:
    path = Path(directory) / "stream.ndjson"

    def write(records):
        path.write_text("\n".join(json.dumps(x) for x in records))

    write([meta])
    assert "records=1\n" in run(path)
    run(path, 0, scan=False)
    run(Path(directory) / "missing", 3)
    for option in ['--jobs=0', '--jobs=-1', '--jobs=257', '--jobs=oops',
                   '--nat-extension=maybe', '--string-extension=', '--axioms=some']:
        r = subprocess.run([exe, option, str(path)], capture_output=True, text=True)
        assert r.returncode == 3, (option, r.returncode, r.stderr)
        checks += 1
    for kind, option in [('natVal', '--nat-extension=false'), ('strVal', '--string-extension=false')]:
        write([meta, {'ie': 0, kind: '123' if kind == 'natVal' else 'λ😀'}])
        r = subprocess.run([exe, option, str(path)], capture_output=True, text=True)
        assert r.returncode == 1, (option, r.returncode, r.stderr)
        checks += 1
    write([meta, {'in': 1, 'str': {'pre': 0, 'str': 'extraAxiom'}},
           {'ie': 0, 'sort': 0},
           {'axiom': {'name': 1, 'type': 0, 'levelParams': [], 'isUnsafe': False}}])
    for policy, expected in [('all', 0), ('standard', 1)]:
        r = subprocess.run([exe, '--axioms=' + policy, str(path)], capture_output=True, text=True)
        assert r.returncode == expected, (policy, r.returncode, r.stderr)
        checks += 1
    write([meta, {'in': 2147483646, 'str': {'pre': 0, 'str': 'lastIndex'}},
           {'il': 2147483646, 'succ': 0}, {'ie': 2147483646, 'sort': 2147483646},
           {'axiom': {'name': 2147483646, 'type': 2147483646, 'levelParams': [], 'isUnsafe': False}}])
    run(path)
    run(path, scan=False)
    records = [meta,
               {"in": 1000000, "str": {"pre": 0, "str": "p😀\n"}},
               {"in": 2, "str": {"pre": 0, "str": "p😀\n"}},
               {"in": 3, "num": {"pre": 2, "i": 4294967295}},
               {"il": 8, "param": 2}, {"il": 2, "succ": 0},
               {"il": 1, "imax": [8, 2]}, {"il": 9, "max": [1, 0]}]
    write(records)
    output = run(path)
    assert "interned_names=3\n" in output
    assert "interned_levels=5\n" in output
    for bad in [
        {"in": 2, "str": {"pre": 0, "str": "duplicate"}},
        {"in": 4, "str": {"pre": 999, "str": "undefined"}},
        {"in": 2147483648, "str": {"pre": 0, "str": "overflow"}},
        {"il": 10, "succ": 999}, {"il": 0, "succ": 0},
        {"il": 10, "imax": [0]}, {"il": 10, "max": [0, 0, 0]},
        {"il": 10, "param": 999}, {"future": {}},
    ]:
        write([*records, bad])
        run(path, 2)
    for data in [b"", b"{}", b"{", b"[]", b'null',
                 json.dumps({"meta": {"format": {"version": "3.2.0"}}}).encode()]:
        path.write_bytes(data)
        run(path, 2)

# File parsing must preserve records across chunk boundaries, including UTF-8,
# JSON escapes, CRLF, long records and a final line without a newline.
with tempfile.TemporaryDirectory() as directory:
    path = Path(directory) / "chunked.ndjson"
    header = json.dumps(meta).encode()
    chunk = 1048576
    for boundary in [chunk-1, chunk, chunk+1, 2*chunk+3]:
        prefix = header + b"\n" + b'{' + b' ' * (boundary-len(header)-3)
        record = b'"in": 1, "str": {"pre": 0, "str": "' + "λ😀".encode() + b'\\n"}}'
        for ending in [b"", b"\n", b"\r\n"]:
            path.write_bytes(prefix + record + ending)
            output = run(path)
            assert "records=2\n" in output
            assert "names=1\n" in output
    # Position the split inside a UTF-8 sequence and inside a JSON escape.
    for tail in ["😀".encode()+b'"}}', b'\\u03bb"}}']:
        prefix = header+b'\n{"in":1,"str":{"pre":0,"str":"'
        path.write_bytes(prefix+b'a'*(chunk-len(prefix)-1)+tail)
        assert "records=2\n" in run(path)
    path.write_bytes(header+b"\n"+b" "*(chunk+17)+b"{}\n")
    r = subprocess.run([exe, "--scan", str(path)], capture_output=True, text=True)
    assert r.returncode == 2 and "line 2:" in r.stderr, r.stderr
    checks += 1
    path.write_bytes(header+b"\n\n")
    r = subprocess.run([exe, "--scan", str(path)], capture_output=True, text=True)
    assert r.returncode == 2 and "line 2:" in r.stderr, r.stderr
    checks += 1

for root in map(Path, sys.argv[2:]):
    files = [root] if root.is_file() else sorted(root.rglob("*.ndjson"))
    for path in files:
        # This compares record classification/counting, not declaration validity.
        records = [json.loads(line) for line in path.read_text().splitlines()]
        kinds = collections.Counter(next(k for k in r if k not in ("in", "il", "ie")) for r in records)
        result = subprocess.run([exe, "--scan", str(path)], capture_output=True, text=True)
        # Malformed references/metadata in static reject fixtures may decline.
        if result.returncode in (1, 2) and "bad" in path.parts:
            checks += 1
            continue
        assert result.returncode == 0, (path, result.stderr)
        counts = dict(line.split("=") for line in result.stdout.splitlines())
        assert int(counts["records"]) == len(records), path
        assert int(counts["names"]) == kinds["str"] + kinds["num"], path
        assert int(counts["levels"]) == sum(kinds[k] for k in ("succ", "max", "imax", "param")), path
        assert int(counts["expressions"]) == sum("ie" in r for r in records), path
        assert int(counts["declarations"]) == sum(kinds[k] for k in ("axiom", "thm", "def", "opaque", "quot", "inductive")), path
        checks += 1
print(f"cli: {checks} checks passed")
