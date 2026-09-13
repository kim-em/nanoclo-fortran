# Implementation evidence

All milestones and success criteria in PLAN.md are complete. The final checker
has all 200 correct arena verdicts, exact Init counter fidelity, and a complete
three-way measurement table. No decline counts as a successful verdict.

## Submission preparation (2026-09-13)

The current arena corpus is reconciled at `ac1c13762de41b594fa24b90ede8cfd97ac6a765`: 198 scored cases and 17 open-outcome cases. A pinned 207-file small snapshot is
bundled for release/debug CI, and the eight large inputs retain their measured
hashes. See [submission readiness](docs/submission.md) for private-release and
resource-validation evidence. Historical performance tables below retain their
original corpus names and classifications.

## Optimized build (2026-09-12)

The current build adds table metadata caching, cheaper bucket placement and stronger
compiler inlining. [The optimization report](docs/optimization-report.md) records
the complete rerun against the preserved Rust and Fortran baselines. All 200 scored
verdicts remain correct, with zero declines; all 15 additional current arena
`either` cases also match pinned nanoclo (8 accept, 7 reject, zero declines).
All 26 counters match on Init’s 54,475 declarations, the 119 accepted original
small fixtures and the eight accepted new cases. The checked-debug suite passes,
with 21,940 core checks and the original 205 checker cases. Clean pinned-Nix
release/debug suites and portable wrapper/schema checks pass.

The default and locally registered binaries match the optimized measurement binary.
See [current identities](docs/optimization-environment.json) and
[submission readiness](docs/submission.md).

## Original implementation baseline

The evidence below predates the optimization and retains its original binary
identities, compiler flags, serial runs and AoS measurements.

### Implemented and verified

The port includes hash-consed terms, Myers closure environments, read-set
projection, interned values/spines, closure inference, lazy conversion and
budgeted probes, constructor/iota/K/Quot reduction, dependent projections,
and inductive validation. Nested types are specialized into temporary mutual
types, then generated recursors are restored and compared with the export.

All 15 Nat primitives use interned arrays of base-2^32 limbs. Successor
compression, literal recursors, large-argument deferral/demand and UTF-8
String expansion are connected. The executable calls no external kernel
or arithmetic library.

Exported expressions are held in immutable shared storage. Workers have private
temporary storage, interners and caches; OpenMP dynamically schedules declarations.
Serial mode retains cross-declaration caches with nanoclo's 2^22-expression
epoch threshold. Parser maps use compact dense indices with a sparse fallback,
and input bytes and parser staging are released before checking.

Verified using gfortran 15.3.0 with the stack settings in PLAN.md:

- Final SoA: **all 200 measured correct verdicts**, 127 accepts and 73 rejects,
  **zero declines**. Mathlib passed with four workers under the specified
  22,000,000 KiB virtual-memory limit and 3,600-second timeout.
- Pinned Rust and the official kernel also have all 200 correct verdicts
  on the same input hashes. All 600 main measurement cells are complete.
- All 26 counters match the pinned Rust kernel on each of Init's **54,475
  declarations**, in both SoA and AoS builds, including run totals.
- All 192 small fixtures match Rust's verdicts, and all 119 accepted fixtures
  match all 26 per-declaration counters and run totals.
- Release and checked debug suites pass 205 checker cases, including the
  192 small fixtures and 13 synthetic cases, with zero declines.
- Init, Std, Cedar and CSLib all pass single-threaded on the final binary.
- The Expr AoS ablation changes only storage, accessors and capacity growth.
  Its debug suite and all 12 large/deep-stream measurements pass.
  AoS Mathlib also finishes under the same memory and timeout limits.
- The local arena checker is registered, its schema validates, and
  `lka.py build-checker nanoclo-fortran` succeeds. The default and registered
  executables were byte-identical to that measured SoA binary at baseline completion.

Final fidelity evidence:

- [All 200 arena verdicts](docs/arena-verdicts.json)
- [Readable counter summary](docs/fidelity-summary.json)
- [All Init counters](docs/init-fidelity.json.gz)
- [All AoS Init counters](docs/init-aos-fidelity.json.gz)
- [All small-corpus counters](docs/corpus-fidelity.json.gz)

The isolated Rust counter build contains reporting-only instrumentation;
[its exact patch](docs/nanoclo-counters.patch) is included. Performance
measurements use the original uninstrumented Rust binary.

Component checks: 21,935 core, 4,104 independent bignum arithmetic comparisons,
2,017 expression, 1,083,354 environment, 132,158 read-set, 10,019 value,
304 `eq_mod`, 9 evaluator/probe, 23 inductive reduction, 2,036 parser,
224 CLI and 7,432 independent declaration-metadata comparisons.
[Release/debug and registration logs](docs/verification-logs.tar.gz) are included.

### Original measurements and reproducibility

[Reproduction instructions](docs/reproduce.md) cover builds, the complete
export corpus, fidelity instrumentation, performance measurements and arena
registration. [Source and host identities](docs/measurement-environment.json)
and [export provenance](docs/export-provenance.json) pin the inputs.

The [complete measurement report](docs/measurements.md) contains all 200
instruction-count comparisons, peak RSS, all three original hypotheses, the
12-stream Expr AoS ablation and four single-thread comparisons. The main
large-stream Fortran/Rust ratios range from 2.394 to 2.837. One deep-list stream
uses about 30% fewer Fortran instructions than Rust. AoS uses fewer instructions
than SoA on all 12 ablation streams; on Mathlib its peak RSS is higher.

All four-worker main and ablation measurements use a 3,600-second timeout and
22,000,000 KiB virtual-memory limit. SoA Mathlib accepts in 1,523 seconds with
14,990,840 KiB peak RSS; AoS accepts in 1,552 seconds with 17,365,712 KiB peak RSS.
The official Mathlib reference accepts in 3,235 seconds under the same limits.
No timeout extension was used. Wall time is supplementary on the shared host.

The machine-readable [instruction table](docs/instructions.csv),
[hashed measurement evidence](docs/measurements.json.gz), and
[raw logs](docs/measurement-logs.tar.gz) are included. Source hashes in the
measurement manifest identify the exact implementation used throughout.
