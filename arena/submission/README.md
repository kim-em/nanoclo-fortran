# Kernel Arena submission

[The update PR](https://github.com/leanprover/lean-kernel-arena/pull/204) pins release `b3a13ef5f0ae98a56087bef11ee1ec80dde0ef30` to fix the Mathlib memory failure.
The [initial submission](https://github.com/leanprover/lean-kernel-arena/pull/198)
was merged with the earlier release.

- `nanoclo-fortran.yaml`: portable, revision-pinned checker definition.
- `arena.patch`: the single-revision update under upstream `checkers/`.
- `release.json`: release pin, validation records, PR URL and previous submission.
- `../submission-pr.md`: the concise submitted PR body.

The patch targets Lean Kernel Arena `62c45f880410e3a320be72b80be32beb2a6d4e19`.
The unmodified arena tool fetched and built the public source without credentials.
Full Mathlib accepts under a 14 GiB memory cap without swap or OOM events;
all seven other large tests and 207 small release/debug cases pass.

See [the memory-fix report](../../docs/mathlib-memory-fix.md) for evidence.
The next full upstream run must confirm Mathlib on its runner; PR CI skips it.
