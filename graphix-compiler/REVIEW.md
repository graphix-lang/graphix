# Compiler review

Rechecked the six remaining findings against `bde703a6` on 2026-09-23.
All six are resolved and their XCR comments are removed. The 15 findings
closed in the previous pass remain closed: **none of the 21 findings is
open**. This follow-up changes only review comments and this record.

| ID | Resolution verified |
| --- | --- |
| CR09 | A dereference stores one optional address and releases it when its reference bottoms. Derived places unregister, bottom their mirrors and stop writes until the address returns. |
| CR11 | Places, ordinary array accesses and slice bounds share `array::check_index`; string indices are rejected while Error-valued elements remain valid. |
| CR12 | A composed place carries both its full registration path and its remaining steps. Its mirror reads only those remaining steps from the dereference's production. |
| CR13 | Every image occurrence is a reference of stable length; definitions are appended separately by `ImageEncoder::finish`. Repeated and reordered measurements no longer alter frame lengths. |
| CR18 | Recursive rewrite scopes use pooled maps, rewritten sequences and lambda arguments build Arc slices directly, and capture analysis uses pooled collections. |
| CR20 | Builder caches retain only empty boxes. Value and string builders return to their pools on release, allowing oversized capacity to be discarded. |

The image layout and its invariants are documented in
[program_image.md](../design/program_image.md). Earlier reproductions and
review responses remain in git history.

## Validation

- Full workspace `cargo test` passed, including the reference regressions in
  both engines, the nested-reference mirror test, image framing and sharing
  tests, oversized value/string builder tests, and the seq suite.
- The module-origin and real-file import-cycle regressions also passed.
- `cargo fmt --all --check` and `git diff --check` passed.
- The slow-test release gate and a long fuzzer campaign were not run.
