# Pinned arena fixtures

`arena-small.tar.gz` contains the 190 small scored exports and 17 open-outcome
exports for Lean Kernel Arena revision
`ac1c13762de41b594fa24b90ede8cfd97ac6a765`. The eight large exports remain outside
this repository. The JSON manifest records every input hash and the verdict of
pinned nanoclo `4cdd12f`; open-outcome cases are checked for fidelity to that pin.

The scored archive was downloaded from `https://arena.lean-lang.org/lean-arena-tests.tar.gz`.
Its original archive hash is preserved in the manifest. The open-outcome files
were reproduced from the pinned arena sources with lean4export revision
`411dce7db58a3afc60ecab2d211acd1042b593dc` and Lean v4.29.1; see
`docs/optimization-either-provenance.json` and `docs/submission-eta-fidelity.json.gz`.

These generated fixtures derive from leanprover/lean-kernel-arena and Lean 4,
licensed under Apache-2.0. They are bundled to make regression CI independent
of changes to the live arena download.

Run `python3 tests/test_arena_snapshot.py build/nanoclo-fortran` after building.
