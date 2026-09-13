# Kernel Arena submission

The repository is public. [The upstream PR](https://github.com/leanprover/lean-kernel-arena/pull/198) adds the checker pinned to
`c65af64db73fd41b757d2227193914fcf5188d5d`. Its [release CI](https://github.com/kim-em/nanoclo-fortran/actions/runs/34752071630) passed before the PR was opened.
The [submission bundle](../arena/submission/README.md) contains the checker YAML,
patch, release metadata and submitted PR text.

## Current coverage

Validation targets Lean Kernel Arena
`ac1c13762de41b594fa24b90ede8cfd97ac6a765`. Its 215 cases comprise **198 scored**
(127 accept, 71 reject) and **17 open-outcome** cases. All scored verdicts are
correct; open-outcome verdicts match pinned nanoclo `4cdd12f`. There are **zero
declines**. [Current corpus manifest](submission-corpus.json).

The eta cases were moved from the rejection tutorial into open-outcome corner
cases, and later tutorial names were renumbered. Every current small scored
input retains a previously validated hash. The eight large export definitions
retain their semantic inputs. Historical 200-case performance tables preserve
their original names and classifications.

All 26 counters match on Init’s 54,475 declarations, all 119 accepted small scored
fixtures and the eight accepted open-outcome fixtures. The comparison harness
sets the required stack limit itself; this fixed a validation-only crash when
`app-lam` was run with the shell’s 8 MiB default. Production source is unchanged
from the optimized measurements.

## Build and runtime validation

- A pinned gfortran 15.3.0 Nix environment builds the pure-Fortran checker.
- CI runs component/synthetic tests and all **207 bundled small fixtures** in
  release mode with four workers and in checked-debug serial mode.
  [Public release CI evidence](submission-public-ci.json).
- An earlier clean private build passed all 207 small cases through the
  unmodified arena runner. [Runner evidence](submission-arena-integration.json).
- The public release was fetched and built through unmodified arena tooling
  with Git credential configuration disabled. Source hashes match the measured
  checker, the YAML schema validates, and the wrapper preserves accept, reject
  and error statuses. [Public build evidence](submission-public-build.json).
- Mathlib accepted with four workers under a **16,000,000,000-byte cgroup**, with
  swap disabled and **zero OOM events**, in approximately 20 minutes on the shared
  host. Charged memory reached the configured cap, including accounted file cache;
  this differs from process RSS. Historical optimized peak RSS was 14.295 GiB.
  [Physical-memory evidence](submission-memory.json).

[Validation logs](submission-validation-logs.tar.gz) retain the earlier clean
build, corpus runs, fidelity checks, memory check and private CI monitor.
The [optimization report](optimization-report.md) retains performance evidence
against the original nanoclo pin; it is not a current leaderboard ranking.
Licensing and source attribution are in the repository’s LICENSE and NOTICE.

## Upstream follow-up

The PR description requests validation including the large tests. The upstream
PR workflow uses `build-test --skip-ci`, so a green PR job alone does not establish
Mathlib coverage on its `nscloud-ubuntu-22.04-amd64-8x16` runner. Local cgroup
validation does not replace an actual upstream CI result.

Future checker releases require explicit commit-pin updates. Regenerate the YAML
with `scripts/prepare_arena_submission.py`; the development registration at
`arena/nanoclo-fortran.yaml` uses a local absolute path and is separate from the
portable submitted definition.

References: [contributing a checker](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/README.md#contributing-checkers),
[checker schema](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/schemas/checker.json),
and [CI workflow](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/.github/workflows/build-and-deploy.yml).
