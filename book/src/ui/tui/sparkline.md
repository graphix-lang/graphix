# The Sparkline Widget

The `sparkline` widget renders compact inline charts perfect for dashboards and status displays. It shows data trends in minimal space, with support for color-coded bars based on thresholds.

<!-- CR claude for claude: [doc-drift/low] The interface blocks on the tui widget pages
are hand copies that have drifted from the .gxi files. Positional parameters are named
`a`, `v` or `n` where the module says `data`, `value`, `child`, `bars`, `bounds`,
`lines` or `shapes` (sparkline, chart, barchart, scroll, block, paragraph, canvas).
canvas.md and chart.md list three markers, but Marker has eight (Bar, HalfBlock,
Quadrant, Sextant and Octant are missing). scroll.md does not say that begin_symbol,
end_symbol and track_symbol default to null and that null removes the arrow or track,
while a null thumb_symbol keeps ratatui's default. Copy each block from its .gxi, list
every marker, and document the scrollbar symbol semantics. (tui-widgets-07) -->
## Interface

```graphix
type RenderDirection = [
  `LeftToRight,
  `RightToLeft
];

type SparklineBar = {
  style: [Style, null],
  value: [f64, null]
};

val sparkline_bar: fn(
  ?#style: [Style, null],
  v: [f64, null]
) -> SparklineBar;

val sparkline: fn(
  ?#absent_value_style: &[Style, null],
  ?#absent_value_symbol: &[string, null],
  ?#direction: &[RenderDirection, null],
  ?#max: &[f64, null],
  ?#style: &[Style, null],
  a: &Array<[SparklineBar, f64, null]>
) -> Tui;
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
