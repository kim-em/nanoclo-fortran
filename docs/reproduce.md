# Reproducing the checks and measurements

The tested source, compiler versions and library pins are recorded in
[optimization-environment.json](optimization-environment.json) (current build),
[measurement-environment.json](measurement-environment.json) (original baseline) and
[export-provenance.json](export-provenance.json). The default checker is pure
Fortran; Python and Lean are used only to prepare fixtures and collect evidence.

## Build and regression tests

```sh
nix develop --command make
nix develop --command make test EXPORTS=_tmp/arena
nix develop --command make debug EXPORTS=_tmp/arena
nix develop --command make BUILD=build/aos EXPR_LAYOUT=aos
```

GNU Make 4.3 or newer and gfortran 13 or newer are required. Measurements here
used gfortran 15.3.0. The AoS build generates just the Expr storage module;
all other source and compiler flags are shared with the default SoA build.

## Input corpus

The small arena archive contains 192 streams. Download and extract
`https://arena.lean-lang.org/lean-arena-tests.tar.gz` into `_tmp/arena`.
Its SHA-256 is
`1187de969cefe8f1a5bca09321239e8c01a9a54bebbf665989eb1beb8de6db40`.
The eight remaining exports are generated from the arena definitions at
revision `08affb0326c16bad8d0933a0a2332d52b420ba4a`:

```sh
python3 scripts/prepare_exports.py \
  --arena /path/to/lean-kernel-arena --work _tmp/export-work \
  --output _tmp/arena-large
```

This requires PyYAML, git, and elan/Lake. It installs the toolchain-specific
exporter, builds each pinned library, exports the configured module/declarations,
and writes hashes and provenance. In particular, `Std` is exported through the
arena's `Test` module, which imports Std. Exporting `Std` directly produces a
different stream. Existing mismatched checkouts or exports are refused.

## Rust and official baselines

Build nanoclo revision `4cdd12f6283ee236afc653b353566d5df59d36fe` with
`cargo build --release`. The four-worker measurement config is:

```json
{
  "use_stdin": true,
  "num_threads": 4,
  "nat_extension": true,
  "string_extension": true,
  "unsafe_permit_all_axioms": true,
  "unpermitted_axiom_hard_error": false,
  "print_success_message": false
}
```

The official baseline is the arena's `checkers/official` Lake project, built
with Lean `v4.34.0-rc2` and the exact lean4export dependency revision in
[measurement-environment.json](measurement-environment.json). Its driver calls
`Export.Parse` and `KernelEnv.replay`; the replay itself is serial.

## Counter fidelity

Use a separate reporting-only Rust build for counters. The instrumentation
script exports a fresh copy of the pinned source and adds all 26 per-declaration
counters. The [exact patch](nanoclo-counters.patch) is included here. Use
`python3 scripts/instrument_nanoclo.py --help` for its arguments, build the
result with Cargo, and set `num_threads` to 1 in the config above.

```sh
ulimit -s unlimited
python3 tests/compare_nanoclo.py \
  --rust /path/to/instrumented/nanoclo --port build/nanoclo-fortran \
  --config /path/to/serial-config.json \
  --exports _tmp/arena-large/good/init.ndjson \
  --all-declaration-counters --timeout 600 \
  --output docs/optimization-init-fidelity.json.gz
```

Repeat with `--exports _tmp/arena` for the small corpus and the AoS binary for
its Init gate. The comparison refuses changed binaries and records input hashes,
process statuses, configuration, and every counter line. Accepted streams are
compared declaration by declaration; rejected streams are compared by verdict.

## Instructions and peak memory

The commands below describe the **original** 620-cell protocol. Its results and
source identities are preserved in `measurement-environment.json`. Do not mix
current binaries into those output directories. For the optimized source, use:

```sh
nix shell nixpkgs#time -c python3 scripts/measure.py \
  --checker soa --binary build/nanoclo-fortran --jobs 4 \
  --exports _tmp/arena _tmp/arena-large --output _tmp/measurements/optimized
python3 scripts/report_optimization.py --root _tmp/measurements --output docs
```

The optimization report reuses the original `soa-index` and `rust` measurements
only after validating matching input hashes, thread counts and limits. Its source
and binary must match `optimization-environment.json`. Reproducing the original
protocol requires the [original source snapshot](optimization-baseline-source.tar.gz)
and flags. Extract it in a separate checkout to avoid replacing the optimized source.

```sh
nix shell nixpkgs#time -c python3 scripts/measure.py \
  --checker soa --binary build/nanoclo-fortran --jobs 4 \
  --exports _tmp/arena _tmp/arena-large --output _tmp/measurements/soa-index
nix shell nixpkgs#time -c python3 scripts/measure.py \
  --checker rust --binary /path/to/nanoclo --config /path/to/parallel-config.json \
  --jobs 4 --exports _tmp/arena _tmp/arena-large --output _tmp/measurements/rust
nix shell nixpkgs#time -c python3 scripts/measure.py \
  --checker official --binary /path/to/official/kernel --jobs 4 \
  --exports _tmp/arena _tmp/arena-large --output _tmp/measurements/official
nix shell nixpkgs#time -c python3 scripts/measure.py \
  --checker aos --binary build/aos/nanoclo-fortran --jobs 4 \
  --exports _tmp/arena-large --output _tmp/measurements/aos-index
python3 scripts/report_measurements.py --root _tmp/measurements --output docs
```

For the AoS ablation, also run the four `good/perf/magma-list-*.ndjson`
streams from `_tmp/arena`, using `--only` for each and the same AoS output
directory. These cover the remaining deep-telescope hypothesis streams.

The report also expects the four single-thread cells in
`_tmp/measurements/soa-serial-index` and `_tmp/measurements/rust-serial`.
Run the corresponding commands above with `--jobs 1`, a one-thread Rust
config, and `--only good/init.ndjson --only good/std.ndjson
--only good/cedar.ndjson --only good/cslib.ndjson`.

The report generator needs Matplotlib. The measurement script needs working
Linux hardware performance counters and GNU time. It records one run per cell,
resumes only cells with identical binary/input/configuration hashes, and retains
raw stdout, stderr, perf, time, and JSON data. Each checker is limited to
22,000,000 KiB virtual memory and 3,600 seconds; the main stack is unlimited and
`OMP_STACKSIZE=1G`. The Fortran checker returns its native arena verdict, including
reject=1. Rust and the official driver's nonzero exits are normalized to reject
as in their arena wrappers; a timeout always fails the measurement.

The local arena registration is [nanoclo-fortran.yaml](../arena/nanoclo-fortran.yaml).
It uses this workspace's absolute path; adjust that path in a relocated checkout,
then place or symlink the file in the arena's `checkers` directory and run
`lka.py build-checker nanoclo-fortran`.

## Current arena additions and submission

The 15 new `either` corner cases are pinned by the configurations and input hashes
in [optimization-either-provenance.json](optimization-either-provenance.json).
Use the arena source at `526bb2280b6d0cff670ee6e28725f96da960cc32`.
For static `file` configurations, copy that file from the pinned arena checkout.
For each `leanfile` configuration, build lean4export at the exporter revision in
`export-provenance.json` using the pinned arena `tests/lean-toolchain`, then run:

```sh
python3 scripts/export_leanfile.py /path/to/arena corner-cases/CASE \
  /path/to/lean4export _tmp/either-work/CASE _tmp/arena-either/corner-cases/CASE.ndjson
```

Run `tests/compare_nanoclo.py` with `--exports _tmp/arena-either` and the same
serial/all-counter options as above. These cases have open arena outcomes;
compare their verdicts to nanoclo rather than labeling rejection a scored error.
The portable definition and remaining release steps are documented in
[submission.md](submission.md).

## Pinned submission regression snapshot

The current 207 small fixtures (190 scored, 17 open-outcome) are bundled in
`tests/fixtures/arena-small.tar.gz`; the companion JSON manifest pins every hash
and nanoclo verdict. Run `python3 tests/test_arena_snapshot.py build/nanoclo-fortran`
for the four-worker gate, or use `build/debug/nanoclo-fortran --jobs 1` for the
checked build. The release workflow runs both, in addition to component tests.

For a Linux cgroup-v2 memory check, replace the absolute paths below and run:

```sh
systemd-run --user --wait --pipe \
  -p MemoryMax=16000000000 -p MemorySwapMax=0 \
  -p LimitSTACK=infinity -p RuntimeMaxSec=3600 \
  python3 /path/to/nanoclo-fortran/scripts/check_memory_budget.py \
    --binary /path/to/nanoclo-fortran/build/nanoclo-fortran \
    --input /path/to/mathlib.ndjson --output /path/to/memory-evidence.json
```

This checks actual charged memory (including file cache) with swap disabled,
separately from the historical virtual-address-space cap. The evidence records
cgroup limits, peak usage, OOM events, hashes and the checker exit status.
