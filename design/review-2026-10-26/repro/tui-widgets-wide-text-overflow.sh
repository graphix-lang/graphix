#!/bin/sh
# Title: a chart y-axis title, a chart dataset name (with a legend) or a
# horizontal bar label whose width is near a multiple of 65536 overflows
# ratatui's u16 layout arithmetic (chart.rs `w + 1`, `inner_width + 2`,
# barchart.rs `area.x + label_size + margin`): a panic in a build with
# overflow checks (dev, soak), a wrapped width in release.
# Command: sh design/review-2026-10-26/repro/tui-widgets-wide-text-overflow.sh
# Expected: three charts drawn with their text cut to the screen, no panic.
# Observed (debug binary):
#   ytitle: panicked at ratatui-widgets-0.3.2/src/chart.rs:803:16: attempt to add with overflow
#   legend: panicked at ratatui-widgets-0.3.2/src/chart.rs:816:36: attempt to add with overflow
#   hlabel: panicked at ratatui-widgets-0.3.2/src/barchart.rs:531:20: attempt to add with overflow
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
python3 - "$D" <<'PY'
import sys
d = sys.argv[1]
t = 'a' * 65535
n = 'a' * 65534
open(f'{d}/ytitle.gx', 'w').write(f'''use tui::{{line, chart::{{axis, chart, dataset}}}};

chart(
    #x_axis: &axis({{min: 0.0, max: 10.0}}),
    #y_axis: &axis(#title: line("{t}"), {{min: 0.0, max: 10.0}}),
    &[dataset(&[(1.0, 1.0)])]
)
''')
open(f'{d}/legend.gx', 'w').write(f'''use tui::{{line, chart::{{axis, chart, dataset}}}};

chart(
    #legend_position: &`TopRight,
    #x_axis: &axis({{min: 0.0, max: 10.0}}),
    #y_axis: &axis({{min: 0.0, max: 10.0}}),
    &[dataset(#name: &line("{n}"), &[(1.0, 1.0)])]
)
''')
open(f'{d}/hlabel.gx', 'w').write(f'''use tui::{{line, barchart::{{bar, bar_chart, bar_group}}}};

bar_chart(#direction: &`Horizontal, &[bar_group([bar(#label: &line("{t}"), &3)])])
''')
PY
for p in ytitle legend hlabel; do
  echo "$p:"
  timeout -s KILL 15 script -qec "stty rows 10 cols 40; timeout -s KILL 6 $G --no-cache $D/$p.gx" /dev/null 2>&1 | grep -a -A1 panicked | head -2
done
rm -rf $D
