# Kernel Arena submission

The repository is public and [the upstream PR](https://github.com/leanprover/lean-kernel-arena/pull/198) is open.
The checker pins release `c65af64db73fd41b757d2227193914fcf5188d5d`, whose
[regression CI](https://github.com/kim-em/nanoclo-fortran/actions/runs/34752071630) passed.

- `nanoclo-fortran.yaml`: portable, revision-pinned checker definition.
- `arena.patch`: the addition under upstream `checkers/`.
- `release.json`: release pin, validation records and PR URL.
- `../submission-pr.md`: the concise submitted PR body.

The patch was validated against Lean Kernel Arena
`ac1c13762de41b594fa24b90ede8cfd97ac6a765`. The unmodified arena tool fetched and
built the public source without credentials. Future releases require a new
commit pin. Upstream validation should include the large tests, because PR CI
skips some of them.

See `docs/submission.md` in this repository for the complete evidence.
