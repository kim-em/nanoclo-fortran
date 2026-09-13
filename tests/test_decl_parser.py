"""Compare parsed declaration SoA fields against independent JSON decoding."""
from copy import deepcopy
import json
from pathlib import Path
import subprocess
import sys
import tempfile

exe = str(Path(sys.argv[1]).resolve())
checks = 0

def run(path, expected=0):
    global checks
    r = subprocess.run([exe, str(path)], capture_output=True, text=True, timeout=60)
    assert r.returncode == expected, (str(path), expected, r.returncode, r.stderr)
    checks += 1
    return r.stdout

def compare(path):
    global checks
    output = run(path)
    maps = {kind: {} for kind in 'NEPSR'}
    decls = []
    for line in output.splitlines():
        kind, *fields = line.split()
        values = list(map(int, fields))
        if kind == 'D':
            decls.append(values)
        else:
            maps[kind][values[0]] = values[1] if kind in 'NEP' else values[1:]
    def seq(idx):
        out = []
        while idx != 1:
            head, idx = maps['S'][idx]
            out.append(head)
        return out
    names, exprs, params = maps['N'], maps['E'], maps['P']
    expected = []
    tags = {'axiom': 1, 'def': 2, 'thm': 3, 'opaque': 4, 'quot': 5, 'types': 6, 'ctors': 7, 'recs': 8}
    for record in map(json.loads, path.read_text().splitlines()):
        blocks = []
        if 'inductive' in record:
            block = record['inductive']
            start = len(expected) + 1
            end = start + sum(len(block[k]) for k in ('types', 'ctors', 'recs'))
            blocks = [(kind, d, start, end) for kind in ('types', 'ctors', 'recs') for d in block[kind]]
        else:
            blocks = [(kind, d, len(expected)+1, len(expected)+2) for kind, d in record.items() if kind in tags]
        for kind, d, start, end in blocks:
            hint = d.get('hints', 'opaque')
            hk = 0 if hint == 'opaque' else 2 if hint == 'abbrev' else 1
            height = hint['regular'] if isinstance(hint, dict) else 0
            expected.append([tags[kind], names[d['name']], [params[names[n]] for n in d['levelParams']],
                             exprs[d['type']], exprs[d['value']] if 'value' in d else 0, hk, height,
                             d.get('numParams', 0), d.get('numIndices', 0), d.get('numMotives', 0),
                             d.get('numMinors', 0), int(d.get('isRec', False)), int(d.get('numNested', 0)>0),
                             int(d.get('k', False)), [names[n] for n in d.get('all', [])] if kind in ('types', 'recs') else [],
                             [names[n] for n in d.get('ctors', [])] if kind == 'types' else [], names[d['induct']] if 'induct' in d else 0,
                             d.get('cidx', 0), d.get('numFields', 0),
                             [[names[r['ctor']], r['nfields'], exprs[r['rhs']]] for r in d.get('rules', [])],
                             start, end])
    for row in decls:
        for i in (2, 14, 15):
            row[i] = seq(row[i])
        row[19] = [maps['R'][r] for r in seq(row[19])]
    assert len(decls) == len(expected), (str(path), len(decls), len(expected))
    for i, (actual, want) in enumerate(zip(decls, expected)):
        assert actual == want, (str(path), i, actual, want)
    checks += len(expected)

meta = {'meta': {'format': {'version': '3.1.0'}}}
base = [meta, *[{'in': n, 'str': {'pre': 0, 'str': str(n)}} for n in range(1, 5)],
        {'ie': 0, 'sort': 0}]
# Deliberately reverse JSON field order; declaration insertion order is fixed.
block = {'recs': [{'name': 3, 'type': 0, 'levelParams': [4], 'isUnsafe': False, 'numParams': 2,
                  'numIndices': 3, 'numMotives': 4, 'numMinors': 5, 'all': [1], 'k': True,
                  'rules': [{'ctor': 2, 'nfields': 65535, 'rhs': 0}]}],
         'ctors': [{'name': 2, 'type': 0, 'levelParams': [4], 'isUnsafe': False,
                    'induct': 1, 'cidx': 7, 'numParams': 2, 'numFields': 9}],
         'types': [{'name': 1, 'type': 0, 'levelParams': [4], 'isUnsafe': False, 'all': [1],
                    'ctors': [2], 'isRec': True, 'isReflexive': False, 'numIndices': 3,
                    'numNested': 2, 'numParams': 2}]}
with tempfile.TemporaryDirectory() as tmp:
    path = Path(tmp) / 'block.ndjson'
    def write(b):
        path.write_text('\n'.join(json.dumps(r) for r in [*base, {'inductive': b}]))
    write(block); compare(path)
    for group in ('types', 'ctors', 'recs'):
        b = deepcopy(block); b[group][0]['isUnsafe'] = True; write(b); run(path, 1)
        b = deepcopy(block); b[group][0]['type'] = 999; write(b); run(path, 2)
        b = deepcopy(block); b[group][0]['numParams'] = 65536; write(b); run(path, 2)
        b = deepcopy(block); del b[group][0]['levelParams']; write(b); run(path, 2)
        b = deepcopy(block); b[group] = {}; write(b); run(path, 2)
    b = deepcopy(block); b['ctors'][0]['name'] = 1; write(b); run(path, 1)
    b = deepcopy(block); b['recs'][0]['rules'][0]['rhs'] = 999; write(b); run(path, 2)
    b = deepcopy(block); b['types'][0]['all'] = [999]; write(b); run(path, 2)
    b = deepcopy(block); b['types'][0]['isRec'] = 0; write(b); run(path, 2)

for root in map(Path, sys.argv[2:]):
    for path in sorted(root.rglob('*.ndjson')):
        if 'good' in path.parts:
            compare(path)
print(f'declaration parser: {checks} checks passed')
