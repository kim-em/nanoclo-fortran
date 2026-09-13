Update nanoclo-fortran to stream its NDJSON input instead of retaining the full 5.2 GiB Mathlib export during parsing, addressing the published exit-137 failure. Kernel algorithms are unchanged.

Full Mathlib accepts under a 14 GiB memory cap with swap disabled and zero OOM events. All seven other large tests and 207 small release/debug cases pass; all 26 Init counters match nanoclo. [Validation evidence](https://github.com/kim-em/nanoclo-fortran/blob/main/docs/mathlib-memory-fix.md) and [passing CI](https://github.com/kim-em/nanoclo-fortran/actions/runs/34787793763). Please include Mathlib in the next full arena run; PR CI skips it.

:robot: prepared with Codex
