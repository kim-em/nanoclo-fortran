# Mathlib memory failure and streaming input

The arena run at `512ee87e6209bda5004cb38789ba424a1b04ee93` killed
nanoclo-fortran's Mathlib process with exit 137 after 73.3 seconds, at a reported
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

Validation so far: release and checked-debug component tests pass, including
chunk-boundary regression cases; all 207 small cases pass with four release
workers; all 26 counters match nanoclo on Init's 54,475 declarations.
[Init counter evidence](streaming-init-fidelity.json.gz).
The full Mathlib run under a 12 GiB physical-memory cap and zero swap is in
progress; the completed evidence will be recorded before updating the arena pin.

The former performance tables and memory evidence describe the original pinned
binary. They are retained as historical records, not substituted with results
from the new parser.
