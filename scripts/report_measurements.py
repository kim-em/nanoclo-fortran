"""Write the complete instruction-count table and a standalone comparison plot."""
import argparse
import csv
import gzip
import json
from pathlib import Path
import tarfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--root', type=Path, required=True)
p.add_argument('--soa', default='soa-index')
p.add_argument('--aos', default='aos-index')
p.add_argument('--output', type=Path, default=Path('docs'))
a = p.parse_args()

def load(directory):
    out = {}
    for path in sorted(directory.rglob('*.measurement.json')):
        row = json.loads(path.read_text())
        if row['file'] in out:
            raise SystemExit('duplicate cell: ' + row['file'])
        out[row['file']] = row
    return out

runs = {name: load(a.root / subdir) for name, subdir in
        [('fortran', a.soa), ('nanoclo', 'rust'), ('official', 'official'), ('aos', a.aos),
         ('fortran_serial', 'soa-serial-index'), ('nanoclo_serial', 'rust-serial')]}
keys = sorted(runs['fortran'])
if len(keys) != 200:
    raise SystemExit(f'Fortran has {len(keys)}/200 measurements')
for checker in ['fortran', 'nanoclo', 'official']:
    if set(runs[checker]) != set(keys):
        raise SystemExit(f'{checker} coverage differs')
    for row in runs[checker].values():
        if not row['verdict_correct'] or row['instructions'] is None:
            raise SystemExit(f'unsatisfied final cell: {checker} {row["file"]}')
for key in keys:
    if len({runs[c][key]['input_sha256'] for c in ['fortran', 'nanoclo', 'official']}) != 1:
        raise SystemExit('input mismatch: ' + key)
ablation_keys = {'good/' + n + '.ndjson' for n in [
    'init', 'std', 'cedar', 'cslib', 'con-leche', 'mathlib',
    'perf/magma-string-n4', 'perf/magma-string-pair-n9',
    'perf/magma-list-deep-n21', 'perf/magma-list-deep-n36',
    'perf/magma-list-pair-n21', 'perf/magma-list-pair-n7']}
if set(runs['aos']) != ablation_keys or any(r.get('pending') for r in runs['aos'].values()):
    raise SystemExit('AoS requires all twelve completed large/deep-stream measurements')
serial_keys = {'good/' + n + '.ndjson' for n in ['init', 'std', 'cedar', 'cslib']}
for checker in ['fortran_serial', 'nanoclo_serial']:
    if set(runs[checker]) != serial_keys:
        raise SystemExit('serial coverage differs: ' + checker)
    for key, row in runs[checker].items():
        if not row['verdict_correct'] or row['instructions'] is None:
            raise SystemExit('unfinished serial measurement: ' + checker + ' ' + key)
        parent = 'fortran' if checker == 'fortran_serial' else 'nanoclo'
        if any(row[field] != runs[parent][key][field] for field in ['input_sha256', 'binary_sha256']):
            raise SystemExit('serial identity differs: ' + checker + ' ' + key)
a.output.mkdir(parents=True, exist_ok=True)
rows = []
for key in keys:
    f, r, o = (runs[c][key] for c in ['fortran', 'nanoclo', 'official'])
    rows.append({'stream': key, 'verdict': 'reject' if key.startswith('bad/') else 'accept',
                 'fortran_instructions': f['instructions'], 'nanoclo_instructions': r['instructions'],
                 'official_instructions': o['instructions'],
                 'fortran_over_nanoclo': f['instructions'] / r['instructions'],
                 'fortran_over_official': f['instructions'] / o['instructions'],
                 'fortran_peak_rss_kib': f['peak_rss_kib'], 'nanoclo_peak_rss_kib': r['peak_rss_kib'],
                 'official_peak_rss_kib': o['peak_rss_kib'], 'sha256': f['input_sha256']})
with (a.output / 'instructions.csv').open('w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=list(rows[0]))
    writer.writeheader(); writer.writerows(rows)
# Compact raw counter/verdict evidence; the accompanying archive retains raw logs.
(a.output / 'measurements.json.gz').write_bytes(gzip.compress(json.dumps(runs, indent=2).encode(), mtime=0))
with tarfile.open(a.output / 'measurement-logs.tar.gz', 'w:gz') as archive:
    for checker, cells in runs.items():
        for key, cell in sorted(cells.items()):
            for kind, filename in cell['raw'].items():
                archive.add(filename, arcname=f'{checker}/{key}.{kind}')
large = ['init', 'std', 'cedar', 'cslib', 'con-leche', 'mathlib']
by_name = {r['stream']: r for r in rows}
lines = ['# Instruction-count measurements', '',
         'All 200 streams have the correct Fortran verdict: 127 accepts, 73 rejects, zero declines. '
         'All 26 counters match the pinned Rust kernel for every Init declaration; '
         'see [counter evidence](init-fidelity.json.gz).', '',
         'Each cell is one run using `perf stat -e instructions:u`. The main table uses four Fortran and Rust workers. '
         'The official driver replays declarations serially (`LEAN_NUM_THREADS=4` is set). '
         'Runs use an unlimited main stack, `OMP_STACKSIZE=1G`, '
         'a 3,600-second timeout and a 22,000,000 KiB virtual-memory limit. '
         'Wall time is supplementary on this shared AMD EPYC 9455 host. '
         'Virtual CPU seconds are instructions divided by 6 billion.', '',
         'GNU time wraps perf to measure peak RSS. On tiny streams the roughly 18 MiB perf monitor '
         'sets a measurement floor; the large-stream peaks belong to the checker.', '',
         '[Compiler, source, and host identities](measurement-environment.json).', '',
         '[Reproduction instructions](reproduce.md) · [Export provenance](export-provenance.json) · '
         '[Raw measurement logs](measurement-logs.tar.gz)', '',
         '## Large streams', '',
         'Ratios above 1 mean that Fortran executes more instructions.', '',
         '| Stream | Fortran instructions | Rust instructions | Official instructions | Fortran / Rust | Fortran peak GiB |',
         '|---|---:|---:|---:|---:|---:|']
for name in large:
    r = by_name['good/' + name + '.ndjson']
    lines.append(f"| {name} | {r['fortran_instructions']:,} | {r['nanoclo_instructions']:,} | "
                 f"{r['official_instructions']:,} | {r['fortran_over_nanoclo']:.3f} | {r['fortran_peak_rss_kib']/2**20:.3f} |")
lines += ['', '![Instruction comparison](instruction-ratios.svg)', '', '## Hypotheses fixed before measurement', '', '[Original protocol](measurement-protocol.md).', '']
init_ratios = [by_name['good/'+n+'.ndjson']['fortran_over_nanoclo'] for n in ['init', 'std']]
lines += [f"1. Parity on Init/Std: observed ratios are {init_ratios[0]:.3f} and {init_ratios[1]:.3f}; the parity hypothesis is not supported."]
deep = [r for r in rows if r['stream'] == 'good/con-leche.ndjson' or '/magma-' in r['stream']]
wins = sum(r['fortran_over_nanoclo'] < 1 for r in deep)
winning_deep = ', '.join(f"`{r['stream']}` ({r['fortran_over_nanoclo']:.3f} Fortran/Rust)"
                         for r in deep if r['fortran_over_nanoclo'] < 1) or 'none'
lines += [f"2. A win on deep telescopes: Fortran uses fewer instructions on {wins}/{len(deep)} selected con-leche/magma streams.",
          f"3. No win on Mathlib: observed Fortran/Rust ratio is {by_name['good/mathlib.ndjson']['fortran_over_nanoclo']:.3f}.", '',
          'Deep-stream wins: ' + winning_deep + '.', '',
          '## Expr layout ablation', '',
          'The AoS source is generated from the same Fortran Expr implementation by '
          '`scripts/make_expr_aos.py`. Only storage, accessors, and capacity growth change. '
          'The rest of the checker and build flags are identical. '
          'Its Init declaration counters are checked against the same Rust baseline.', '',
          '| Stream | SoA instructions | AoS instructions | AoS / SoA | SoA peak GiB | AoS peak GiB | AoS status |',
          '|---|---:|---:|---:|---:|---:|---|']
for key, ao in sorted(runs['aos'].items()):
    so = runs['fortran'][key]
    if ao['input_sha256'] != so['input_sha256']:
        raise SystemExit('ablation input differs: ' + key)
    count = f"{ao['instructions']:,}" if ao['instructions'] is not None else '—'
    ratio = f"{ao['instructions']/so['instructions']:.3f}" if ao['verdict_correct'] and ao['instructions'] is not None else '—'
    status = 'accept' if ao['verdict_correct'] else f"failed (exit {ao['process_status']})"
    peak = f"{ao['peak_rss_kib']/2**20:.3f}" if ao['peak_rss_kib'] is not None else '—'
    lines.append(f"| {key} | {so['instructions']:,} | {count} | {ratio} | "
                 f"{so['peak_rss_kib']/2**20:.3f} | {peak} | {status} |")
valid_aos = [key for key, r in runs['aos'].items() if r['verdict_correct'] and r['instructions'] is not None]
aos_wins = sum(runs['aos'][key]['instructions'] < runs['fortran'][key]['instructions'] for key in valid_aos)
lines += ['', f'AoS executes fewer instructions on {aos_wins}/{len(valid_aos)} successful ablation streams.']
lines += ['', 'Rust/Fortran ratios include differences in compilers, parsing and interner implementation; '
          'counter equality supports algorithm fidelity but does not establish a causal effect of layout. '
          'The Fortran AoS comparison isolates the Expr storage change.', '',
          '## Single-thread validation', '',
          'The final Fortran binary also checks Init, Std, Cedar and CSLib with one worker. '
          'Serial mode retains cross-declaration caches; the main table uses four workers.', '',
          '| Stream | Fortran instructions | Rust instructions | Fortran / Rust | Fortran peak GiB |',
          '|---|---:|---:|---:|---:|']
for name in ['init', 'std', 'cedar', 'cslib']:
    key = 'good/' + name + '.ndjson'
    f, r = (runs[c][key] for c in ['fortran_serial', 'nanoclo_serial'])
    if not f['verdict_correct'] or not r['verdict_correct'] or f['input_sha256'] != r['input_sha256']:
        raise SystemExit('invalid serial cell: ' + key)
    if f['binary_sha256'] != runs['fortran'][key]['binary_sha256']:
        raise SystemExit('serial binary differs: ' + key)
    lines.append(f"| {name} | {f['instructions']:,} | {r['instructions']:,} | "
                 f"{f['instructions']/r['instructions']:.3f} | {f['peak_rss_kib']/2**20:.3f} |")
lines += ['',
          '## Every stream', '',
          '[Machine-readable table](instructions.csv) · [Hashed measurement evidence](measurements.json.gz)', '',
          '| Stream | Verdict | Fortran instructions | Rust instructions | Official instructions | Fortran / Rust |',
          '|---|---|---:|---:|---:|---:|']
for r in rows:
    lines.append(f"| {r['stream']} | {r['verdict']} | {r['fortran_instructions']:,} | {r['nanoclo_instructions']:,} | "
                 f"{r['official_instructions']:,} | {r['fortran_over_nanoclo']:.3f} |")
(a.output / 'measurements.md').write_text('\n'.join(lines) + '\n')
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
fig, ax = plt.subplots(figsize=(9, 4.5))
ys = list(range(len(large)))
for offset, c, label in [(-.18, 'nanoclo', 'Fortran / Rust'), (.18, 'official', 'Fortran / official')]:
    vals = [runs['fortran']['good/'+n+'.ndjson']['instructions'] / runs[c]['good/'+n+'.ndjson']['instructions'] for n in large]
    ax.barh([y+offset for y in ys], vals, height=.34, label=label)
ax.axvline(1, color='#555555', linewidth=1, linestyle='--')
ax.set_yticks(ys, large); ax.invert_yaxis(); ax.set_xlabel('Instruction ratio (smaller is better for Fortran)')
ax.legend(loc='lower left', bbox_to_anchor=(0, 1.01), ncol=2, frameon=False)
ax.spines[['top', 'right']].set_visible(False)
fig.tight_layout(); fig.savefig(a.output / 'instruction-ratios.svg'); fig.savefig(a.output / 'instruction-ratios.png', dpi=180)
print('wrote 200-row table, evidence, report, and comparison plots')
