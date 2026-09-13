# Mathlib memory failure and streaming input

Release `b3a13ef5f0ae98a56087bef11ee1ec80dde0ef30` replaces the whole-file input copy with streaming reads.
Mathlib now accepts under a **14 GiB physical-memory limit with no swap and zero
OOM events**. [Full-run evidence](mathlib-streaming-memory.json).

The arena run at `512ee87e6209bda5004cb38789ba424a1b04ee93` killed the previous
release's Mathlib process with exit 137 after 73.3 seconds, at a reported
15,249,715,200-byte peak RSS. No proof rejection or diagnostic was emitted.
[Published failure evidence](mathlib-arena-failure.json).

The CLI previously retained the entire 5.2 GiB NDJSON export while parsing it
into kernel data structures. That left insufficient memory headroom on the
shared 16 GB arena runner. A previous local 16 GB cgroup run succeeded, but it
did not reproduce the runner's other memory consumers. The older executable
also reproduces an OOM kill locally when restricted to 12 GiB without swap.

The CLI now reads through a 1 MiB buffer. Complete records pass to the existing
JSON tokenizer and record parser in the same order. A partial record is retained
across reads; the buffer grows only when one record exceeds its capacity. A
record without a final newline is handled, and error messages retain line numbers.
The existing in-memory parser remains available to component tests and tools.
Kernel storage layout, evaluation and conversion algorithms are unchanged.

## Validation

- Full Mathlib accepts with four workers in 798.1 seconds
  (13.3 minutes) on the shared local host, under a
  15,032,385,536-byte cgroup with swap disabled and zero OOM events. The observed
  peak process RSS was **13.56 GiB**. Charged cgroup memory reached its cap,
  including reclaimable file cache; that is a different measure from process RSS.
  [Memory evidence](mathlib-streaming-memory.json),
  [RSS samples](streaming-mathlib-rss.json.gz).
- All seven other large exports accept. [Per-input results](streaming-large-tests.json).
- All 207 small cases pass with four release workers and in checked-debug serial
  mode, with zero declines. Component and synthetic tests also pass, including
  long records, final lines without a newline, CRLF, UTF-8 and JSON escapes split
  across read boundaries, and malformed-input line numbers.
  [Public CI](https://github.com/kim-em/nanoclo-fortran/actions/runs/34787793763),
  [CI job evidence](streaming-ci.json).
- All 26 counters match pinned nanoclo on Init's 54,475 declarations for both the
  measured executable and a fresh public build through unmodified arena tooling.
  [Counter evidence](streaming-init-fidelity.json.gz),
  [Public build and wrapper checks](streaming-public-build.json).

A stricter **12 GiB** attempt of the streaming checker still exhausted memory
later during proof checking, after 5 minutes 22 seconds. This release does not
claim to fit that limit. The completed 14 GiB test demonstrates more headroom
than the original 16 GB validation, but the next full upstream arena run remains
the final check on its own runner. PR CI skips Mathlib.

[Build identities](streaming-environment.json) and
[validation logs](streaming-validation-logs.tar.gz) retain the measurements and
both unsuccessful 12 GiB stress tests. The Nix wrapper ignored `-march=native`
in this environment; this run is not a controlled speed comparison with the
historical optimization measurements. Earlier performance tables and memory
evidence remain records of the original pinned binary.
