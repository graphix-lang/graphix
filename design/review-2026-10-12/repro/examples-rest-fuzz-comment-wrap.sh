#!/bin/sh
# A valid program carrying the `// expect: reject` header the regress
# corpus re-attaches (build.rs strip_header) is rejected by a PARSE error
# from the wrapper (`let result = {// expect: reject`), so the pin agrees
# as `reject` whatever the checker does. Same for any file whose first
# line is a comment (every saved finding) under `check`/`run`/`minimize`.
G=${GRAPHIX_FUZZ:-graphix-fuzz}
d=$(dirname "$0")
$G run "$d/examples-rest-fuzz-comment-wrap.gx" | head -1   # CompileErr: Unexpected `/`
$G check "$d/examples-rest-fuzz-comment-wrap.gx"           # AGREE (reject)
grep -v '^ *//' "$d/examples-rest-fuzz-comment-wrap.gx" > /tmp/stripped.gx
$G check /tmp/stripped.gx                                  # AGREE (trace)
