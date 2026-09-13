# Prepared submission bundle

The checker definition and patch pin private release
`b99baba4eaa39704d7cfc5163e10a85efb260b03`. The patch has been checked against
Lean Kernel Arena `ac1c13762de41b594fa24b90ede8cfd97ac6a765`.

- `nanoclo-fortran.yaml`: portable, revision-pinned checker definition.
- `arena.patch`: ready-to-apply addition under upstream `checkers/`.
- `release.json`: exact pins and private CI run.
- `../submission-pr.md`: prepared PR body; suggested title: **Add nanoclo-fortran checker**.

The repository remains private and no upstream PR has been opened. After source
publication is explicitly authorized, apply the patch to an upstream checkout,
recheck any intervening arena changes, and submit the prepared PR. Ask for a
validation run including the large tests, because PR CI skips some of them.

The release source was fetched from the private GitHub repository and built by
unmodified `lka.py`. Its binary matches the one exercised through the arena runner
on all 207 current small fixtures. See `docs/submission.md` in the repository root
for the complete evidence and resource checks.
