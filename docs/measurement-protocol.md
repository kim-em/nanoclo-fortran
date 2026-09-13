# Measurement protocol (before measurement)

The hypotheses from PLAN.md are fixed before collecting kernel measurements:

1. Instruction-count parity with nanoclo on `init` and `std`.
2. A win on deep telescopes (`con-leche`, `magma-*`) where environment memory
   dominates nanoclo.
3. No win on `mathlib`.

Report outcomes against all three hypotheses regardless of direction. There
are no kernel performance results yet; parser timing does not test them.

For each stream, pin the input SHA-256, nanoclo commit
`4cdd12f6283ee236afc653b353566d5df59d36fe`, official kernel revision, compiler
version, flags, jobs, extension/axiom options, and arena version. Build nanoclo
with `cargo build --release`. Run each cell once with
`perf stat -e instructions:u` and `/usr/bin/time -v` for peak RSS. Preserve raw
output, exit verdicts, timeout/memory limits, and per-declaration counters.
Virtual CPU seconds are instructions divided by 6,000,000,000. Wall time on
this shared host is supplementary data. Failed/declined/timed-out cells must
remain visible rather than being reported as speedups.

Use `ulimit -s unlimited` and `OMP_STACKSIZE=1G`. Run the four-worker mathlib
experiment under `ulimit -v 22000000` and an explicit timeout. Before the
performance comparison, establish counter equivalence on `init-prelude` and
`init`; unfold, probe, cache-hit, and iota counters are required evidence of
algorithm fidelity. Preserve the Expr array-of-structs ablation as a separate
build with otherwise identical code and inputs.
