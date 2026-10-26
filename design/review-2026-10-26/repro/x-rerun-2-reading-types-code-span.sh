#!/bin/sh
# Title: book-ui-b-04 variant: book/src/core/reading_types.md's Quick Reference Table writes
#   `Error<\`ParseError>` and `Result<i64, \`Err>`; CommonMark takes no backslash escape inside
#   a code span, so each span ends at the escaped backtick.
# Command (repo root): sh design/review-2026-10-26/repro/x-rerun-2-reading-types-code-span.sh
# Expected: no output (each example renders as one code span, written `` Error<`ParseError> ``).
# Observed: <code>Error&lt;\</code>ParseError&gt;`  (and the same for Result<i64, \`Err>)
grep -o 'Error containing type[^/]*</td><td><code>[^<]*</code>[^<]*' docs/book/core/reading_types.html | grep '\\</code>'
grep -n '\\`' book/src/core/reading_types.md
