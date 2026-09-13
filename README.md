# nanoclo-fortran

A Fortran 2018 port of nanoclo's Lean 4 kernel with struct-of-arrays arenas.
**All 200 arena streams have the correct verdict: 127 accepts, 73 rejects,
zero declines.** All 26 counters match Rust on every one of Init's 54,475
declarations. The 15 additional arena `either` cases in that snapshot also match the pinned Rust verdicts,
with zero declines. See the [optimized performance report](docs/optimization-report.md),
[implementation evidence](PROGRESS.md) and [the plan](PLAN.md).

Build and test with gfortran 13 or newer and GNU Make 4.3 or newer:

```sh
nix develop --command make
nix develop --command make test EXPORTS=/path/to/arena/exports
nix develop --command make debug EXPORTS=/path/to/arena/exports
```

The flake pins gfortran 15.3.0 and GNU Make. Nix requires the build files to be
tracked in the checkout. On systems with gfortran installed, run `make` directly. Release builds use
`-O3 -march=native -flto=auto -fopenmp -frecursive` with stronger inlining
(`-finline-limit=1000 --param inline-unit-growth=100`). Debug builds enable bounds,
overflow and runtime checks in a separate build directory.

```sh
ulimit -s unlimited
export OMP_STACKSIZE=1G
build/nanoclo-fortran --jobs=4 /path/to/export.ndjson
```

The checker implements closure inference, lazy evaluation/conversion,
inductive positivity and recursor validation, nested inductives, Quot, all
15 Nat primitives with arbitrary-precision arithmetic, and UTF-8 String
literal expansion. Worker contexts share immutable exported expressions
and keep private scratch arenas and caches. The default `--jobs=1` mode
retains cross-declaration caches.

Options use `=`: `--jobs=N`, `--nat-extension=true|false`,
`--string-extension=true|false`, and `--axioms=all|standard`.
Both literal extensions and all axioms are enabled by default, matching
nanoclo's arena configuration. Standard axiom mode allows `propext`,
`Quot.sound` and `Classical.choice`.

`--scan` parses the stream and reports record counts; a successful scan
is not a kernel verdict. Exit codes are 0 accept, 1 reject, 2 decline for
unsupported/malformed export input, and 3 error. No arena stream declines.

The executable uses only Fortran modules, without C bindings, an external
arithmetic library or a delegated kernel. Python supports fixture preparation,
tests and measurements. The [source map](docs/source-map.md) links the
Fortran implementation to its Rust counterparts. [Reproduction instructions](docs/reproduce.md) cover
the complete corpus, counter checks, Expr AoS ablation and local arena setup.

The port follows nanoclo commit `4cdd12f6283ee236afc653b353566d5df59d36fe`.
Translations carry source notices; the Apache-2.0 license and attribution
are in [LICENSE](LICENSE) and [NOTICE](NOTICE).

[Submission readiness](docs/submission.md) records the portable checker definition,
clean-build validation and remaining publication/arena CI steps. The
[original baseline](docs/measurements.md) preserves the initial three-way and AoS measurements.

A pinned snapshot of the current small arena corpus is bundled for regression CI
(190 scored cases and 17 open-outcome cases). After building, run:

```sh
nix develop --command python3 tests/test_arena_snapshot.py build/nanoclo-fortran
```

The historical 200-case performance report retains its original inputs; the
[current submission coverage](docs/submission-corpus.json) tracks upstream’s
renaming and reclassification of the eta cases.
