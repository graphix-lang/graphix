# Repros for the 2026-10-05 review

Each file is named for the finding it demonstrates (see `design/REVIEW-2026-10-05.md`).
The header of each file states the command, the expected output and what HEAD
(`c722befe`) printed.

- `.gx`: run with `graphix --no-cache <file>` (it may need a timeout: reactive
  programs do not exit) or `graphix-fuzz check <file>`.
- `.sh` / `.py`: self-contained drivers (corrupted caches, LSP sessions, sockets,
  generated inputs).
- `.rs`: integration tests that need the crate harness. Copy one into the owning
  crate's `tests/` directory and run `cargo test -p <crate> --test <name>`.
- `x-stack-05.py` writes the 600 KB program it needs. `x-image-04.img.gz` is a
  corrupted image captured from a fuzz run, used by `x-image-04.sh`.
