# Kernel Arena submission

[The submission update PR](https://github.com/leanprover/lean-kernel-arena/pull/204) pins release `b3a13ef5f0ae98a56087bef11ee1ec80dde0ef30` to fix the Mathlib
memory failure. The [initial PR](https://github.com/leanprover/lean-kernel-arena/pull/198)
was merged with release `c65af64db73fd41b757d2227193914fcf5188d5d`.
The repository is public, and the [submission bundle](../arena/submission/README.md)
contains the updated checker YAML, patch, release metadata and submitted PR text.

## Current coverage

Validation covers all **215 current cases**: **198 scored** (127 accept,
71 reject) and **17 open-outcome** cases. All scored verdicts are correct;
open-outcome verdicts match pinned nanoclo `4cdd12f`. There are **zero declines**.
The [corpus manifest](submission-corpus.json) was prepared at arena
`ac1c13762de41b594fa24b90ede8cfd97ac6a765`; tests and tutorial definitions remain
unchanged at the update's base `62c45f880410e3a320be72b80be32beb2a6d4e19`.

The eta cases were moved from rejection tutorials into open-outcome corner
cases, and later tutorial names were renumbered. Historical 200-case performance
tables retain their original names and classifications.

## Validation of the update

The checker streams the NDJSON input instead of retaining the entire 5.2 GiB
Mathlib file during parsing. Kernel algorithms and storage layout are unchanged.
[Diagnosis, implementation and validation](mathlib-memory-fix.md).

- Full Mathlib accepts with four workers under a **14 GiB physical-memory cap**,
  swap disabled and **zero OOM events**. A stricter 12 GiB stress test still OOMs
  during checking; that limit is not claimed. [Memory evidence](mathlib-streaming-memory.json).
- All seven other large exports accept. [Results](streaming-large-tests.json).
- Release and checked-debug component tests pass, including chunk-boundary and
  malformed-input regressions. All **207 bundled small fixtures** pass with four
  release workers and in checked-debug serial mode.
  [Public CI](https://github.com/kim-em/nanoclo-fortran/actions/runs/34787793763), [job evidence](streaming-ci.json).
- All 26 counters match on Init's 54,475 declarations, for both the measured
  executable and a fresh public build. [Counter evidence](streaming-init-fidelity.json.gz).
- Unmodified arena tooling anonymously fetched the pinned public source and built
  it with the pinned gfortran 15.3.0 Nix environment. Production source hashes
  match the tested checkout; the YAML schema and wrapper accept/reject/error
  statuses validate. [Public build evidence](streaming-public-build.json).

[Build identities](streaming-environment.json) and
[validation logs](streaming-validation-logs.tar.gz) retain this release's evidence.
The earlier [16 GB memory test](submission-memory.json),
[optimization report](optimization-report.md), and
[original submission logs](submission-validation-logs.tar.gz) describe the
previous binary. That local 16 GB test did not provide enough headroom for the
actual arena runner. Licensing and source attribution are in LICENSE and NOTICE.

## Upstream follow-up

The update PR requests validation including Mathlib. Upstream PR CI passes
`build-test --skip-ci`, so a green PR job alone does not establish Mathlib coverage
on the `nscloud-ubuntu-22.04-amd64-8x16` runner. The next full upstream run remains
the final check on that environment.

Future checker releases require explicit commit-pin updates. Regenerate the YAML
with `scripts/prepare_arena_submission.py`; the development registration at
`arena/nanoclo-fortran.yaml` uses a local absolute path and is separate from the
portable submitted definition.
