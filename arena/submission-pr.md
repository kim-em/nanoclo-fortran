Adds a Fortran 2018 port of nanoclo, storing kernel objects in parallel arrays and checking with four OpenMP workers. Pins a release commit and a reproducible Nix build.

All 215 current cases are covered with zero declines. Release/debug CI passes the 207 small cases, all 26 counters match nanoclo on Init, and Mathlib passes a 16 GB memory limit with swap disabled. Please include the large tests in upstream validation.

:robot: prepared with Codex
