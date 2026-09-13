"""Compare the frozen optimized checker with the original measured baselines."""
import argparse
import csv
import gzip
import hashlib
import json
from pathlib import Path
import tarfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--root', type=Path, default=Path('_tmp/measurements'))
p.add_argument('--output', type=Path, default=Path('docs'))
a = p.parse_args()
runs = {}
for label, directory in [('optimized', 'optimized'), ('original', 'soa-index'), ('rust', 'rust')]:
    rows = [json.loads(f.read_text()) for f in sorted((a.root / directory).rglob('*.measurement.json'))]
    runs[label] = {r['file']: r for r in rows}
    if len(rows) != 200 or len(runs[label]) != 200:
        raise SystemExit(f'{label}: expected 200 unique completed cells, got {len(rows)}')
    for r in rows:
        if (r.get('pending') or not r['verdict_correct'] or r['timed_out']
                or not r['instructions'] or not r['peak_rss_kib']):
            raise SystemExit(f'incomplete or incorrect cell: {label} {r["file"]}')
    if len({r['binary_sha256'] for r in rows}) != 1:
        raise SystemExit(f'mixed binaries: {label}')
keys = sorted(runs['original'])
for label in runs:
    if set(runs[label]) != set(keys):
        raise SystemExit(f'coverage mismatch: {label}')
identity = json.loads((a.output / 'optimization-environment.json').read_text())
if runs['optimized'][keys[0]]['binary_sha256'] != identity['binary_sha256']:
    raise SystemExit('optimized binary differs from source manifest')
for path, digest in identity['source_files_sha256'].items():
    if hashlib.sha256(Path(path).read_bytes()).hexdigest() != digest:
        raise SystemExit(f'source changed since measurements: {path}')
fidelity_summary = {}
for scope, expected, accepted in [('init', 1, 1), ('corpus', 192, 119), ('either', 15, 8)]:
    with gzip.open(a.output / f'optimization-{scope}-fidelity.json.gz', 'rt') as f:
        data = json.load(f)
    if (data['port_binary_sha256'] != identity['binary_sha256'] or data['excluded_files']
            or data['counter_coverage']['per_declaration'] != 'all 26'
            or len(data['results']) != expected):
        raise SystemExit(f'fidelity identity or coverage mismatch: {scope}')
    accepted_rows = [r for r in data['results'] if r['port_process_status'] == 0]
    if len(accepted_rows) != accepted or any(not r['verdict_equal'] for r in data['results']):
        raise SystemExit(f'fidelity verdict mismatch: {scope}')
    for r in accepted_rows:
        if not r['counters_equal'] or r['rust_counters'] != r['port_counters']:
            raise SystemExit(f'fidelity counters mismatch: {scope} {r["file"]}')
    declarations = sum(sum(line.startswith('DC26\t') for line in r['port_counters']) for r in accepted_rows)
    if scope == 'init' and declarations != 54475:
        raise SystemExit('Init declaration coverage differs')
    fidelity_summary[scope] = dict(verdicts=expected, accepted_counter_streams=accepted,
                                   declarations=declarations)
either = json.loads((a.output / 'optimization-either-verdicts.json').read_text())
if (either['binary_sha256'] != identity['binary_sha256'] or either['jobs'] != 4
        or len(either['results']) != 15):
    raise SystemExit('four-worker either evidence differs')
serial_either = {r['file']: r for r in data['results']}
for r in either['results']:
    if (r['process_status'] != serial_either[r['file']]['port_process_status']
            or r['input_sha256'] != serial_either[r['file']]['sha256']):
        raise SystemExit('four-worker either verdict or input differs')
(a.output / 'optimization-fidelity-summary.json').write_text(json.dumps({
    'binary_sha256': identity['binary_sha256'], 'counter_fields': 26,
    'checks': fidelity_summary, 'four_worker_either_cases': 15}, indent=2) + '\n')
rows = []
for key in keys:
    new, old, rust = (runs[k][key] for k in ('optimized', 'original', 'rust'))
    for field in ('input_sha256', 'jobs', 'memory_limit_kib', 'timeout_seconds'):
        if len({r[field] for r in (new, old, rust)}) != 1:
            raise SystemExit(f'{field} mismatch: {key}')
    rows.append(dict(stream=key, verdict='reject' if key.startswith('bad/') else 'accept',
        optimized_instructions=new['instructions'], original_instructions=old['instructions'],
        rust_instructions=rust['instructions'],
        fewer_instructions_percent=100 * (1-new['instructions']/old['instructions']),
        original_over_rust=old['instructions']/rust['instructions'],
        optimized_over_rust=new['instructions']/rust['instructions'],
        original_peak_rss_kib=old['peak_rss_kib'], optimized_peak_rss_kib=new['peak_rss_kib'],
        rust_peak_rss_kib=rust['peak_rss_kib']))
with (a.output / 'optimization-instructions.csv').open('w') as f:
    w = csv.DictWriter(f, fieldnames=rows[0].keys()); w.writeheader(); w.writerows(rows)
with gzip.open(a.output / 'optimization-measurements.json.gz', 'wt') as f:
    json.dump(runs, f, separators=(',', ':'))
with tarfile.open(a.output / 'optimization-logs.tar.gz', 'w:gz') as t:
    for key in keys:
        for kind, path in runs['optimized'][key]['raw'].items():
            t.add(path, arcname='optimized/' + key.removesuffix('.ndjson') + '.' + kind)
large_names = ['init', 'std', 'cedar', 'cslib', 'con-leche', 'mathlib']
large = [next(r for r in rows if r['stream'] == f'good/{n}.ndjson') for n in large_names]
lines = ['# Optimization results', '',
    'The optimized checker preserves all 200 scored verdicts: **127 accepts, 73 rejects, zero declines**.',
    'The additional 15 current `either` cases match pinned nanoclo: **8 accepts, 7 rejects, zero declines**.',
    'These open-outcome cases are reported separately from scored correctness.', '',
    '## Performance against the original nanoclo pin', '',
    'Counts are user-space hardware instructions, one run per cell, with four workers.',
    'Rust is the uninstrumented release build of `4cdd12f6283ee236afc653b353566d5df59d36fe`.',
    'The current arena nanoclo release is newer, so this is not a current leaderboard ranking.',
    'Ratios above 1 mean Fortran executes more instructions; shared-host wall times are retained',
    'as supplementary data and are not used to claim elapsed-time speedups.', '',
    '| Stream | Original / Rust | Optimized / Rust | Fewer instructions | Optimized instructions | Peak GiB |',
    '|---|---:|---:|---:|---:|---:|']
for name, r in zip(large_names, large):
    lines.append(f'| {name} | {r["original_over_rust"]:.3f} | {r["optimized_over_rust"]:.3f} | {r["fewer_instructions_percent"]:.1f}% | {r["optimized_instructions"]:,} | {r["optimized_peak_rss_kib"]/1048576:.3f} |')
deep = [r for r in rows if r['stream'].startswith('good/perf/magma-') or r['stream'] == 'good/con-leche.ndjson']
wins = [r for r in deep if r['optimized_over_rust'] < 1]
lines += ['', f'Fortran uses fewer instructions on **{len(wins)}/{len(deep)}** of the original selected deep/literal streams:', '']
for r in wins:
    lines.append(f'- `{r["stream"]}`: {r["optimized_over_rust"]:.3f}× Rust.')
lines += ['', '[All 200 comparisons](optimization-instructions.csv) · [Full records](optimization-measurements.json.gz)',
    '· [Raw optimized logs](optimization-logs.tar.gz) · [Source, binary and host identities](optimization-environment.json)',
    '· [Original baseline and AoS experiment](measurements.md)', '',
    'All runs use the same input hashes and limits: 3,600 seconds, 22,000,000 KiB virtual memory,',
    'unlimited main stack and `OMP_STACKSIZE=1G`. Virtual CPU seconds are instructions / 6 billion.',
    'GNU time wraps perf for RSS, so tiny streams have a roughly 18 MiB monitor floor.', '',
    '## What changed and why it remains faithful', '',
    'Instruction sampling on the original Init run attributed about 44% to hash-table lookup,',
    'insertion and reserve, and 7.3% to arena growth guards ([profile](optimization-baseline-profile.txt)).',
    'The retained changes are:', '',
    '- Cache table capacity masks and the existing 70% load threshold; bypass redundant reserve calls.',
    '- Pass scalar table keys by value and make table methods non-overridable so the compiler can specialize them.',
    '- Use a cheaper hash only for bucket placement. Full four-word equality still controls lookup.',
    '  The structural hashes stored in arenas remain unchanged. Signed keys are widened before',
    '  multiplication; even the extreme signed 32-bit cases fit signed 64-bit intermediates.',
    '- Move the cold allocation/copy work out of the inlineable arena growth guard.',
    '- Enable stronger GCC inlining with `-finline-limit=1000 --param inline-unit-growth=100`.', '',
    'The table remains five separate struct-of-arrays columns. Ownership transfer now moves its',
    'cached metadata together with storage. Kernel evaluation, conversion, reduction ordering,',
    'cache keys and invalidation policies are unchanged.', '',
    'All 26 counters match on every one of Init’s **54,475 declarations**, and on every declaration',
    'in the **119 accepted original small fixtures** and **8 accepted new either fixtures**.',
    'Rejected streams are compared by verdict. This is measured algorithm-fidelity evidence,',
    'not a formal equivalence proof.', '',
    '[Init counters](optimization-init-fidelity.json.gz) · [Small-corpus counters](optimization-corpus-fidelity.json.gz)',
    '· [Either-case counters](optimization-either-fidelity.json.gz) · [Four-worker either verdicts](optimization-either-verdicts.json)',
    '', '## Exploratory Init cells', '',
    'Each row was a distinct build and one exploratory run; only the final optimized binary',
    'was used for the complete 200-stream rerun above ([exploratory records](optimization-experiments.json)).',
    'A combined 2-D table allocation was tried',
    'and discarded because it increased instruction counts.', '',
    '| Variant | Init instructions |', '|---|---:|',
    '| Original | 322,754,986,181 |',
    '| Table metadata, scalar arguments and growth guard | 304,072,378,245 |',
    '| Plus bucket hash | 298,469,418,360 |',
    '| Plus combined 2-D table allocation (discarded) | 303,396,306,048 |',
    '| Combined allocation plus stronger inlining (discarded) | 240,404,830,468 |',
    '| Separate columns plus stronger inlining | 227,388,163,807 |', '',
    'The final build additionally hoists two cache-key assignments to suppress false-positive',
    'uninitialized-variable warnings exposed by stronger inlining. Its measured Init count is',
    'the value in the main table; scheduling and allocation cause small instruction variations.', '',
    '## Validation and submission', '',
    'The complete checked-debug suite passes, including 21,940 core checks, 205 checker cases',
    'and 7,432 independent declaration-metadata comparisons. Clean release and debug builds',
    'also pass in the pinned Nix environment; see [build evidence](submission-build.json),',
    '[verification logs](optimization-verification-logs.tar.gz) and [binary identity checks](optimization-binary-check.json).',
    'The original AoS ablation remains historical evidence; it has not been remeasured with',
    'these optimization flags. The generated AoS expression tests still pass.', '',
    'Mathlib succeeds within the local timeout and virtual-memory cap. Its physical-memory',
    'headroom still needs validation on the arena’s 16 GB runner.',
    'See the [submission checklist](submission.md) for the prepared integration and release steps.', '']
(a.output / 'optimization-report.md').write_text('\n'.join(lines))
print(f'Wrote {len(rows)} complete comparisons; all verdicts correct.')
for r in large:
    print(r['stream'], f'{r["fewer_instructions_percent"]:.1f}% fewer', f'{r["optimized_over_rust"]:.3f}x Rust')
