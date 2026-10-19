#!/bin/sh
# Title: a hugged bracket body is laid out one column past the width
# Command: sh x-formatter-hug-width-off-by-one.sh [path-to-graphix]
# Expected: every line of the output within the 90-column width (the call
#   breaks after `ffff([` as it does one character later).
# Observed: "91|let zz = ffff([aaaa...aaaa, 1]);" -- the line is 91 columns.
#   pretty_tail_bare accepts the hugging layout when its first line fits
#   exactly, without the column of the `;` that follows; with two hugs
#   (ffff(`A(gggg([..])))) the same overflow happens. In the stdlib,
#   stdlib/graphix-package-gui/src/graphix/chart.gx's `scatter3d` formats to a
#   91-column `| -> Dataset `Scatter3D({ .. });` line the same way.
#   Second program: a chain of lone calls hugs at every level with no fit
#   check at all (ApplyExpr::fmt_pretty_inner), giving a 136-column line.
G=${1:-graphix}
A=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
printf 'let zz = ffff([%s, 1]);\nzz\n' "$A" | "$G" fmt | awk '{printf "%3d|%s\n", length($0), $0}'
printf 'let x = %s1%s;\nx\n' "$(printf 'ffffffffff(%.0s' 1 2 3 4 5 6 7 8 9 10 11 12)" "$(printf ')%.0s' 1 2 3 4 5 6 7 8 9 10 11 12)" | "$G" fmt | awk '{printf "%3d|%s\n", length($0), $0}'
