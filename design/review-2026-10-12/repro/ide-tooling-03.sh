#!/bin/bash
# ide-tooling-03: book/theme/graphix-highlight.js under the highlight.js
# mdbook ships (10.1.1, docs/book/highlight-*.js).
# Expected: function names and escapes get classes a theme styles
#   (hljs-title, hljs-...), and the template splice \[y[0]] is one subst.
# Observed (HEAD 7324207f): <span class="hljs-title.function"> and
#   <span class="hljs-char.escape"> (10.x does not split dotted scopes, no
#   CSS rule matches them), and the subst ends at the inner `]` of y[0].
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)"
HLJS="$(ls "$REPO"/docs/book/highlight-*.js | head -1)"
node - "$HLJS" "$REPO/book/theme/graphix-highlight.js" <<'JS'
const fs = require('fs');
const m = { exports: {} };
new Function('module', 'exports', fs.readFileSync(process.argv[2], 'utf8') + '\nmodule.exports = hljs;')(m, m.exports);
const hljs = m.exports;
hljs.registerLanguage('graphix', require(process.argv[3]));
console.log(hljs.highlight('graphix',
  'let x = array::map(a, |v| f(v) + 1); let s = "a\\n[x]"; let t = """x \\[y[0]] z"""').value);
process.exit(0);
JS
