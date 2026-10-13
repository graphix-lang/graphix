# Review 2026-10-12: repros

The evidence for `design/REVIEW-2026-10-12.md`. `repro/<id>.<ext>` reproduces
the finding `<id>`; the CR that ends in `(<id>)` names it as `probe: ...`.

- Each repro's header gives its command, the expected output and what HEAD
  (`7324207f`) printed.
- `.gx` runs with `graphix --no-cache <file>`, or with `graphix-fuzz check
  <file>` where it compares the engines.
- `.sh` and `.py` drive the binaries or the language server themselves.
- A `.rs` repro is an integration test; copy it into the crate named in its
  header (`tests/`) to run it.
- A directory is a module tree; its README or header says which file to run.

The directory also holds the reviewers' own probes and variants, so not every
file is cited by a CR.

Until the fuzzer's comment wrapping is fixed (`fuzz-lib-b-01`), `graphix-fuzz
check` on a file that starts with a comment reports a vacuous `AGREE
(reject)`. Strip the header before trusting a reject.
