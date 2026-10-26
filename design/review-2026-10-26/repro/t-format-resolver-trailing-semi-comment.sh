#!/bin/sh
# Title: graphix fmt refuses a statement list whose trailing `;` is followed
#        by an own-line comment ("formatter bug: ... lost or moved a comment")
# Command: sh design/review-2026-10-26/repro/t-format-resolver-trailing-semi-comment.sh
# Expected: each input formats (e.g. "{\n    a;\n    b;\n    // c\n}\n"), rc=0
# Observed: "formatter bug: the formatted text lost or moved a comment or an
#           attribute", rc=1, for all three inputs. Without the trailing `;`
#           ("{ a; b\n// c\n}") or with a comma list ("[1, 2,\n// c\n]") it formats.
# Cause: the parser (`commented`, Slot::End) hangs the below-comments on the
#        last non-empty item `b`, but `print::pretty_items`/`pretty_file_items`
#        print `below` only for the list's last item, here the empty statement.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'x;\n// c\n' > $d/a.gx
printf '{ a; b;\n// c\n}\n' > $d/b.gx
printf 'let f = |y| {\n    a(y);\n    b(y);\n    // todo\n};\nf\n' > $d/c.gx
for f in a b c; do $G fmt --stdout $d/$f.gx; echo "rc=$?"; done
rm -rf $d
