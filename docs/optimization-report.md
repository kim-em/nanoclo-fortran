# Optimization results

The optimized checker preserves all 200 scored verdicts: **127 accepts, 73 rejects, zero declines**.
The additional 15 current `either` cases match pinned nanoclo: **8 accepts, 7 rejects, zero declines**.
These open-outcome cases are reported separately from scored correctness.

## Performance against the original nanoclo pin

Counts are user-space hardware instructions, one run per cell, with four workers.
Rust is the uninstrumented release build of `4cdd12f6283ee236afc653b353566d5df59d36fe`.
The current arena nanoclo release is newer, so this is not a current leaderboard ranking.
Ratios above 1 mean Fortran executes more instructions; shared-host wall times are retained
as supplementary data and are not used to claim elapsed-time speedups.

| Stream | Original / Rust | Optimized / Rust | Fewer instructions | Optimized instructions | Peak GiB |
|---|---:|---:|---:|---:|---:|
| init | 2.652 | 1.866 | 29.6% | 227,133,583,932 | 0.939 |
| std | 2.639 | 1.878 | 28.9% | 380,345,482,348 | 1.693 |
| cedar | 2.837 | 2.066 | 27.2% | 609,968,564,336 | 3.008 |
| cslib | 2.668 | 1.945 | 27.1% | 1,519,297,547,044 | 6.591 |
| con-leche | 2.394 | 1.711 | 28.5% | 452,757,228,034 | 2.471 |
| mathlib | 2.674 | 1.962 | 26.6% | 5,548,006,192,976 | 14.295 |

Fortran uses fewer instructions on **2/7** of the original selected deep/literal streams:

- `good/perf/magma-list-deep-n36.ndjson`: 0.431× Rust.
- `good/perf/magma-list-pair-n21.ndjson`: 0.878× Rust.

[All 200 comparisons](optimization-instructions.csv) · [Full records](optimization-measurements.json.gz)
· [Raw optimized logs](optimization-logs.tar.gz) · [Source, binary and host identities](optimization-environment.json)
· [Original baseline and AoS experiment](measurements.md)

All runs use the same input hashes and limits: 3,600 seconds, 22,000,000 KiB virtual memory,
unlimited main stack and `OMP_STACKSIZE=1G`. Virtual CPU seconds are instructions / 6 billion.
GNU time wraps perf for RSS, so tiny streams have a roughly 18 MiB monitor floor.

## What changed and why it remains faithful

Instruction sampling on the original Init run attributed about 44% to hash-table lookup,
insertion and reserve, and 7.3% to array growth guards ([profile](optimization-baseline-profile.txt)).
The retained changes are:

- Cache table capacity masks and the existing 70% load threshold; bypass redundant reserve calls.
- Pass scalar table keys by value and make table methods non-overridable so the compiler can specialize them.
- Use a cheaper hash only for bucket placement. Full four-word equality still controls lookup.
  The structural hashes stored with kernel objects remain unchanged. Signed keys are widened before
  multiplication; even the extreme signed 32-bit cases fit signed 64-bit intermediates.
- Move the cold allocation/copy work out of the inlineable array growth guard.
- Enable stronger GCC inlining with `-finline-limit=1000 --param inline-unit-growth=100`.

The table remains five separate struct-of-arrays columns. Ownership transfer now moves its
cached metadata together with storage. Kernel evaluation, conversion, reduction ordering,
cache keys and invalidation policies are unchanged.

All 26 counters match on every one of Init’s **54,475 declarations**, and on every declaration
in the **119 accepted original small fixtures** and **8 accepted new either fixtures**.
Rejected streams are compared by verdict. This is measured algorithm-fidelity evidence,
not a formal equivalence proof.

[Init counters](optimization-init-fidelity.json.gz) · [Small-corpus counters](optimization-corpus-fidelity.json.gz)
· [Either-case counters](optimization-either-fidelity.json.gz) · [Four-worker either verdicts](optimization-either-verdicts.json)

## Exploratory Init cells

Each row was a distinct build and one exploratory run; only the final optimized binary
was used for the complete 200-stream rerun above ([exploratory records](optimization-experiments.json)).
A combined 2-D table allocation was tried
and discarded because it increased instruction counts.

| Variant | Init instructions |
|---|---:|
| Original | 322,754,986,181 |
| Table metadata, scalar arguments and growth guard | 304,072,378,245 |
| Plus bucket hash | 298,469,418,360 |
| Plus combined 2-D table allocation (discarded) | 303,396,306,048 |
| Combined allocation plus stronger inlining (discarded) | 240,404,830,468 |
| Separate columns plus stronger inlining | 227,388,163,807 |

The final build additionally hoists two cache-key assignments to suppress false-positive
uninitialized-variable warnings exposed by stronger inlining. Its measured Init count is
the value in the main table; scheduling and allocation cause small instruction variations.

## Validation and submission

The complete checked-debug suite passes, including 21,940 core checks, 205 checker cases
and 7,432 independent declaration-metadata comparisons. Clean release and debug builds
also pass in the pinned Nix environment; see [build evidence](submission-build.json),
[verification logs](optimization-verification-logs.tar.gz) and [binary identity checks](optimization-binary-check.json).
The original AoS ablation remains historical evidence; it has not been remeasured with
these optimization flags. The generated AoS expression tests still pass.

Mathlib succeeds within the local timeout and virtual-memory cap. Its physical-memory
headroom still needs validation on the arena’s 16 GB runner.
See the [submission checklist](submission.md) for the prepared integration and release steps.
