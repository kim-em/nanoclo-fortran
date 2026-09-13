# nanoclo-fortran: nanoclo's Lean 4 kernel in Fortran 2018, struct-of-arrays

Status: implemented and validated (2026-09-12). All 200 verdicts match nanoclo,
all 26 per-declaration Init counters match, and all measurements are complete.
See [PROGRESS.md](PROGRESS.md), the [original measurement report](docs/measurements.md)
and the later [optimization report](docs/optimization-report.md).
The original goal and success criteria below are preserved.

## Goal

Port nanoclo (Sebastian Graf's Rust Lean 4 kernel based on delayed substitutions and
interned closure environments) to Fortran 2018, keeping the algorithm identical and
changing only the memory layout: every arena becomes a set of parallel integer arrays
(struct-of-arrays), every reference is a 32-bit index, and there is no heap allocation
in the hot path. Measure whether that layout beats the Rust original on the Lean Kernel
Arena streams, using instruction counts.

This is a speed experiment and an existence proof (a complete checker with no garbage
collector, no pointers and no `unsafe`), not a verification project.

## Sources

- nanoclo, branch `main`, commit 4cdd12f (2026-09-09), clone at
  `~/con-leche/_tmp/nanoclo`. https://github.com/sgraf812/nanoclo
- Its ancestor nanoda_lib (`~/con-leche/_tmp/nanoda_lib`) for the inductive checker
  and parser that nanoclo inherited. https://github.com/ammkrn/nanoda_lib
- sokonanoda (`~/con-leche/_tmp/sokonanoda`) for the conversion algorithm nanoclo
  re-implemented. https://github.com/intgrah/sokonanoda
- Lean Kernel Arena, clone at `~/con-leche/_tmp/lean-kernel-arena`.
  https://github.com/leanprover/lean-kernel-arena

Arena numbers for nanoclo (revision 32c30e7a, 2026-09-11): all 200 tests correct,
mathlib at ÷4.2 of the official kernel's instruction count, 6.6 GB.

## What is being ported

nanoclo is 13.3k lines of Rust with zero `unsafe`:

| file | lines | role |
|---|---|---|
| `src/closure.rs` | 1036 | interned environments (`EnvNode{entry, parent, len, next_level, jump}`), Myers skew-binary jump pointers, read-set masks (`uses_mask`: Dense, Mask(u64), Wide, Deep), env projection for cache keys (`key`, `read_key`), `reify`, `eq_mod` |
| `src/nbe.rs` | 539 | `Value` enum (Rigid, Unfold, Lam, Pi, Sort, NatLit, StrLit, Thunk), interned value/spine/env arenas, memo tables, probe state |
| `src/nbe_eval.rs` | 1301 | `nb_eval`, `nb_apply`, `nb_whnf`, unfolding, iota, K-rule, struct eta, Nat and String literal reduction, readback, `nb_type` |
| `src/nbe_conv.rs` | 685 | `nb_unify` with the RIGID const generic, positive/negative caches, budgeted speculative spine probes with iterative deepening (`SPEC_BUDGET = 4096`, cap 2^20), proof irrelevance, eta, struct eta |
| `src/tc.rs` | 929 | declaration driver, thread pool, `infer_*` on closures |
| `src/inductive.rs` | 1757 | inductive checking and recursor generation (nanoda's, on plain `Expr`) |
| `src/parser.rs` | 1691 | lean4export NDJSON: byte-matching fast path plus serde fallback |
| `src/util.rs` | 1087 | `Ptr` (u32 with bit 31 marking export vs scratch dag), `LeanDag` interners, `TcCtx`, config, bignum helpers |
| `src/expr.rs`, `level.rs`, `name.rs`, `env.rs`, `quot.rs` | 1767 | term, level, name types; environment with index cutoff; Quot checks |
| pretty/debug printers, tests | 2400 | incidental |

Everything is already "integers into growable arrays plus hash tables". Terms are
hash-consed `u32` ids with cached `hash`, `num_loose_bvars` and `has_fvars`; values,
spines, closures and environments are interned `u32` ids; value equality is integer
equality; memoisation is by mutating a `forced` field in place.

## The essential algorithm (must be preserved)

1. Hash-consed terms with `num_loose_bvars` on every node.
2. The interned environment arena with Myers jumps; entries `Val(e, env)` (delayed),
   `Neu(fvar)`, `V(value)`; `push_entry` normalising through `norm_clo`.
3. Read-set masks and projected keys for the infer cache and thunk sharing.
4. The interned value graph with in-place `forced` memoisation; spines as interned cons
   lists; `Unfold` kept folded until needed.
5. `nb_unify`'s decision order: id equality, positive/negative caches, literal cases,
   structural, proof irrelevance, eta, struct eta; lazy delta by `ReducibilityHint`;
   budgeted speculative spine probe with deepening and a probe-scoped negative cache.
6. Nat and String literal handling: the 15 Nat primitives, deferral of `Nat.add` with a
   large second argument, `Nat.rec` on literals without unary expansion, `succ` towers.
7. `infer_s` peeling syntactic Pis before evaluating; `eq_mod` before value comparison.

Incidental (may change): `Gen2` two-generation memo capacity policy, `reset_decl`
heuristics, `persist_dag` epochs, mimalloc, `stacker`, `SmallVec`, serde fallback
parser, the pretty printers, the exact thread driver.

## Fortran design

- Compiler: gfortran 13 or newer (via `nix shell nixpkgs#gfortran`); optionally ifx
  for a second data point. `-O3 -march=native`, `-fopenmp`.
- One module per arena, each a set of `integer(int32), allocatable :: field(:)` arrays
  plus a count, grown by doubling with `move_alloc`. Node kinds and fields copied from
  the Rust structs one for one: `expr_tag, expr_a, expr_b, expr_c, expr_hash,
  expr_nlbv, expr_flags`, `env_entry, env_parent, env_len, env_next_level, env_jump`,
  `val_tag, val_head, val_spine, val_env, val_body, val_forced`, `spine_elim,
  spine_parent, spine_len`, and so on.
- Interning tables: one generic open-addressing hash table over `int32` arrays keyed by
  up to four `int32` words, instantiated per arena with an `include` file (Fortran has no
  generics; the include-template idiom is the standard workaround). Same for the ~40 memo
  tables in `CloState`/`Vals`/`ExprCache`; keep nanoclo's names so the two codebases can
  be read side by side.
- Bit tricks: nanoclo's `Ptr` packs an arena tag into bit 31; here two arenas are two
  index spaces with a sign convention (negative = scratch) or a separate tag array.
  Read-set masks (`Mask(u64)`) are `integer(int64)` with `iand`/`ior`/`popcnt`/`trailz`,
  which are intrinsics.
- Bignums: base 2^32 limbs in `int32` arrays with `int64` intermediates, own module;
  matches vow-lean-kernel's approach. GMP through `iso_c_binding` only as a comparison
  build, never the default.
- Recursion: `recursive` procedures; `-frecursive` for gfortran; run with
  `ulimit -s unlimited` and `OMP_STACKSIZE=1G` for the workers (nanoclo gives each
  thread a 1 GiB stack). `nb_whnf`'s `stacker::maybe_grow` has no analogue; if depth
  becomes a problem, convert that one loop to an explicit stack.
- Errors: nanoclo rejects by `panic!`; here every check returns a status and the driver
  maps it to the arena exit codes (0 accept, 1 reject, 2 decline, 3 error). Mixing up
  reject and error is the one thing the arena scores harshly, so this is done first, not
  last.
- Threads: OpenMP `parallel do schedule(dynamic)` over declaration indices with
  thread-private arenas and memo tables, exactly nanoclo's `check_all_declars_par`
  (no shared mutable state, environment visibility by declaration index cutoff).
  Serial mode keeps nanoclo's cross-declaration caches (`persist_dag`).
- Input: the arena passes a file path (`$IN`), so read it with
  `access='stream', form='unformatted'` into one byte array and run nanoclo's byte-
  matching parser over it. No serde fallback; the fast path handles the whole
  lean4export 3.1 format or the run declines.
- Config: a handful of command-line flags (`--jobs`, `--nat-extension`, axiom policy)
  instead of `config.json`.

## Fidelity check: match nanoclo's counters

nanoclo has per-run and per-declaration counters (`NANOCLO_CTRS`, `NANOCLO_DECLCTRS`,
`NANOCLO_DECLTIME` in `src/tc.rs`). Port the counters and diff them per declaration on
`init-prelude` and `init`. Equal unfold, probe, cache-hit and iota counts are the
evidence that it is the same algorithm; the verdicts alone are not.

## Milestones

- M0: repo skeleton, `fpm` or plain `Makefile`, byte-array input, `Name`/`Level` arenas
  with interning, level `leq` with nanoclo's unit tests translated
  (`src/tests/level.rs`). Test: parse `init-prelude` and count records.
- M1: `Expr` arena with cached fields, instantiate/abstract, the environment arena with
  Myers jumps. Test: tutorial `001` to `005`.
- M2: values, `nb_eval`/`nb_apply`/`nb_whnf`, readback, `infer_*`. Test: rest of the
  tutorial that needs no inductives.
- M3: `nb_unify` with caches and probes. Test: `perf/*` ladders (beta-ladder,
  let-ladder, app-lam, church-numerals) with instruction counts next to nanoclo's.
- M4: inductives (port `inductive.rs`), Quot, recursor and K reduction. Test: whole
  tutorial, all `corner-cases/*`, the static reject tests.
- M5: Nat and String literals, bignums. Test: `init-prelude` accepted, counters match.
- M6: `init`, `std`, `cedar`, `cslib` single-threaded; OpenMP; `mathlib` with
  `--jobs=4` under `ulimit -v 22000000` and `timeout`.
- M7: measurement write-up.

## Measurement

- `perf stat -e instructions:u` per stream, one run per cell, alongside nanoclo built
  from the pinned commit with `cargo build --release` (nix provides cargo). The arena's
  "virtual CPU time" is instructions divided by 6 GHz, so this is the number that
  matters. Wall time on this shared machine is data, not a measurement.
- Peak RSS via `/usr/bin/time -v`.
- Hypotheses stated up front: (a) parity on `init`/`std`; (b) a win on the deep-telescope
  streams (`con-leche`, `magma-*`) where nanoclo's environments dominate memory;
  (c) no win on `mathlib`. Report against these whatever the outcome.
- Ablation worth one afternoon: array-of-structs (`type` with allocatable array of
  derived type) versus struct-of-arrays for the `Expr` arena, same code otherwise.

## Success criteria

1. All 200 arena tests with nanoclo's verdicts, registered as a local arena checker
   (`arena/nanoclo-fortran.yaml`).
2. Per-declaration counters equal to nanoclo's on `init`.
3. An instruction-count table for every stream against nanoclo and the official kernel.

## Risks

- Verbosity: 13k lines of Rust with pattern matching becomes more Fortran; the include-
  template hash table and disciplined naming keep it reviewable.
- String handling and the parser are the least pleasant part of Fortran; budget for it.
- Deep recursion in `nb_whnf`/`nb_unify` on the ladders; explicit stacks are the fallback.
- Fortran has no closures, so the `RIGID` const-generic split of `nb_unify` becomes two
  procedures or a flag; a flag is fine.
