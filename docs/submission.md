# Arena submission readiness

The submission target is Lean Kernel Arena at
`ac1c13762de41b594fa24b90ede8cfd97ac6a765` (checked 2026-09-13).
Its published corpus contains 215 cases: **198 scored** (127 accept, 71 reject)
and **17 open-outcome** cases. The two eta cases were moved out of the rejection
tutorial into open-outcome corner cases; the remaining tutorial entries were
renumbered. Every file in the current 190-file small archive matches a previously
validated input hash. The eight large export definitions retain their semantic
inputs. [Current corpus manifest](submission-corpus.json).

## Prepared

- The complete pure-Fortran checker, Apache-2.0 licensing and upstream attribution.
- A pinned gfortran 15.3.0 Nix environment and regression workflow.
- A bundled, hash-verified snapshot of all 207 small cases: 190 scored and 17
  open-outcome. CI runs it with four release workers and a checked serial build,
  alongside the component/synthetic tests. The large exports are separate.
- Exact algorithm-fidelity evidence against nanoclo `4cdd12f`: all 26 counters on
  Init’s 54,475 declarations and every accepted small fixture.
- A portable checker-definition generator, `scripts/prepare_arena_submission.py`.
  It pins a full release commit, uses relative build/run paths, requests four
  workers, sets the required stacks and preserves native exit statuses.
  It declares no tests declined.
- The full original performance report and the later optimization report, with
  source/binary identities and raw evidence. The historical 200-case tables
  retain their original names and classifications.
- A reproducible cgroup-v2 physical-memory check in
  `scripts/check_memory_budget.py`; see [reproduction](reproduce.md).

## Release handoff

The repository must remain private during preparation, and no upstream PR is
opened. A private release commit can be built and checked through authenticated
GitHub access. Public arena CI will need the source made fetchable later.

Generate the checker definition from the exact privately published source commit:

```sh
python3 scripts/prepare_arena_submission.py \
  --revision FULL_RELEASE_COMMIT_SHA \
  --output /path/to/lean-kernel-arena/checkers/nanoclo-fortran.yaml
```

The existing `arena/nanoclo-fortran.yaml` is a development registration using the
local workspace path. The generated definition is the portable submission.
Build that definition from a clean arena checkout to verify the pinned source,
then run the checker through the arena interface. The local credentials needed
while the repository is private are not part of the definition.

Mathlib’s optimized historical peak RSS was **14,989,072 KiB (14.295 GiB)**.
Validate it under a 16,000,000,000-byte physical-memory cgroup with swap disabled;
this is distinct from the historical 22,000,000 KiB virtual-address-space cap.
The final memory evidence is recorded separately from instruction measurements.

After source publication is explicitly authorized, the remaining external steps
are to open the prepared checker PR and inspect upstream CI. Request a run that
includes the large tests: the current PR workflow uses `build-test --skip-ci`,
so a green PR check alone does not establish Mathlib coverage. The current runner
is `nscloud-ubuntu-22.04-amd64-8x16`. Local cgroup validation does not reproduce
every aspect of that runner or replace its CI result.

The arena does not automatically track checker commits; future releases require
explicit revision updates. Its current nanoclo entry is newer than our fidelity
pin, so our performance comparisons remain explicitly against `4cdd12f`.

References: [contributing a checker](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/README.md#contributing-checkers),
[checker schema](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/schemas/checker.json),
and [CI workflow](https://github.com/leanprover/lean-kernel-arena/blob/ac1c13762de41b594fa24b90ede8cfd97ac6a765/.github/workflows/build-and-deploy.yml).
