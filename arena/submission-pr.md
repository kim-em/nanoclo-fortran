Add nanoclo-fortran, a pure Fortran 2018 translation of nanoclo's delayed-substitution
Lean 4 kernel. The checker uses struct-of-arrays arenas, shared immutable exported
expressions and private OpenMP contexts for four workers. It supports nested
inductives, Quot, arbitrary-precision Nat primitives and UTF-8 String literals.

The definition pins a release commit and builds with a pinned gfortran 15.3.0 Nix
environment. It preserves accept/reject/decline/error exit codes and has no
predeclared declines.

Validation against arena revision `ac1c13762de41b594fa24b90ede8cfd97ac6a765` covers
all 215 current inputs: 198 scored cases and 17 open-outcome cases. All scored
verdicts are correct; the open-outcome verdicts match pinned nanoclo `4cdd12f`.
All 26 counters match on Init's 54,475 declarations and accepted small fixtures.
The repository includes pinned small-corpus release/debug CI and complete local
measurement records. The optimized Mathlib instruction count corresponds to
about 15.4 virtual CPU minutes, with a historical 14.295 GiB peak RSS.

Please include the large tests in upstream validation; PR CI ordinarily skips
some of these inputs. The repository's `docs/submission.md` records the release
and resource-validation evidence. Local results are not an upstream CI run.
