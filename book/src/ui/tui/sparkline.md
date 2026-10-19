# The Sparkline Widget

The `sparkline` widget renders compact inline charts perfect for dashboards and status displays. It shows data trends in minimal space, with support for color-coded bars based on thresholds.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/sparkline.gxi}}
```

## Parameters

### sparkline
- **max** - Maximum value for scaling (auto-scales if not specified)
- **style** - Default style for bars
- **direction** - `LeftToRight` (default) or `RightToLeft`

### sparkline_bar
- **style** - Style for this specific bar

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/sparkline_basic.gx}}
```

![Basic Sparkline](./media/sparkline_basic.png)

### Threshold-based Coloring

```graphix
{{#include ../../examples/tui/sparkline_threshold.gx}}
```

![Threshold Colors](./media/sparkline_threshold.gif)

### Multi-metric Dashboard

```graphix
{{#include ../../examples/tui/sparkline_dashboard.gx}}
```

![Multi Sparkline Dashboard](./media/sparkline_dashboard.png)

### Sparkline from Netidx

This example is self-contained: it both publishes and subscribes to a
simulated CPU value over netidx, so you can run it without any
external publisher. Drop the `publish` call and point the subscribe at
<!-- CR claude for claude: [bug/low] The pipeline below publishes field 6 of top's
`%Cpu(s): <us> us, <sy> sy, <ni> ni, <id> id` line, which is the nice percentage, so the
chart reads about 0 while the CPU is busy. Without `-b`, `top` also fails in a pipe with
no tty. Fix: `top -b | grep --line-buffered Cpu | awk '{
printf("/local/metrics/cpu|f64|%s\n", 100 - $8); fflush() }'`. probe:
design/review-2026-10-19/repro/book-ui-a-sparkline-top-field.sh (book-ui-a-04) -->
a real path to chart live data instead — for example, the output of:

```
top | \
grep --line-buffered Cpu | \
awk '{ printf("/local/metrics/cpu|f64|%s\n", $6); fflush() }' | \
netidx publisher
```

```graphix
{{#include ../../examples/tui/sparkline_rolling.gx}}
```

![Rolling Sparkline](./media/sparkline_rolling.gif)

## Use Cases

Sparklines are ideal for:
- System resource monitoring (CPU, memory, network)
- Real-time metrics dashboards
- Compact data visualization in lists or tables
- Rate of change visualization

## See Also

- [chart](chart.md) - For detailed charts with axes
- [gauge](gauge.md) - For single current value display
- [linegauge](linegauge.md) - For horizontal progress bars
