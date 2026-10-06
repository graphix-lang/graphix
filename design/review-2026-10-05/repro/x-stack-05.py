#!/usr/bin/env python3
"""x-stack-05: writes x-stack-05.gx (about 600 KB) beside this script.

Run: python3 x-stack-05.py && graphix --check x-stack-05.gx && \
     graphix fmt --width 100000000 --stdout x-stack-05.gx
Expected: --check passes and fmt prints the program (or refuses it).
Observed at c722befe: --check exits 0; fmt aborts with "thread 'main' has
overflowed its stack" (exit 134) in the derived ExprKind::eq under
format_source's reparse comparison (format.rs:328). The program is 300
parenthesized 1000-term `+` chains, about 300k AST levels, under the parser's
nesting limit.
"""
import os

HEADER = '// x-stack-05: Expr\'s PartialEq is unguarded; `graphix fmt` overflows the stack on a file --check accepts\n// command: graphix --check x-stack-05.gx; graphix fmt --width 100000000 --stdout x-stack-05.gx\n// expected: --check passes and fmt prints the program on one line (or refuses it with an error)\n// observed: --check exit 0; fmt aborts with "thread \'main\' has overflowed its stack" (exit 134),\n// in the recursive derived ExprKind::eq under format_source\'s reparse comparison (format.rs:328).\n// The program is 300 parenthesized 1000-term `+` chains, about 300k AST levels, under the\n// parser\'s nesting limit. At the default width the printer is quadratic first (fmt_flat).\n'

body = "(" * 300 + "x" + "+x" * 999 + ")" + ("+x" * 999 + ")") * 299 + "+x" * 999 + ";"
src = HEADER + "let x = 1;\nlet y = " + body + "\ny\n"
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-stack-05.gx")
open(out, "w").write(src)
print("wrote", out)
