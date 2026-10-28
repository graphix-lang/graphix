# The Chart Widget

The `chart` widget renders line charts with multiple datasets, custom axes, labels, and styling. It's ideal for visualizing time series data, trends, sensor readings, and any numeric data relationships.

Finite line segments are clipped to the axis bounds before rendering, including
segments with extremely large endpoints. A segment crossing the viewport remains
visible even when subtracting its endpoints would overflow.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/chart.gxi}}
```

## Parameters

### chart
- **style** - Background style for the chart area
- **x_axis** - X-axis configuration. Optional, but without it the chart has empty bounds and draws nothing
- **y_axis** - Y-axis configuration, the same

### axis
- **title** - Line for axis title
- **labels** - Array of lines displayed along axis
- **style** - Style for axis lines and ticks

### dataset
- **style** - Style for the dataset (line and markers)
- **graph_type** - `Line`, `Scatter`, or `Bar`
- **marker** - One of `Dot`, `Block`, `Bar`, `Braille`, `HalfBlock`, `Quadrant`, `Sextant`, `Octant`
- **name** - Line naming the dataset (for legends)

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/chart_basic.gx}}
```

![Basic Chart](./media/chart_basic.png)

### Real-time Data Visualization

```graphix
{{#include ../../examples/tui/chart_realtime.gx}}
```

![Real Time Chart](./media/chart_realtime.gif)

### Multiple Datasets

```graphix
{{#include ../../examples/tui/chart_multi.gx}}
```

![Multi Chart](./media/chart_multi.png)


## Marker Comparison

A marker decides how many points one terminal cell can plot (columns by rows):

- **Dot** (`•`), **Block** (`█`), **Bar** (`▄`): one point per cell.
- **HalfBlock**: 1x2, the half block characters; a cell is about twice as tall as it is wide, so the points are square.
- **Quadrant**: 2x2.
- **Sextant**: 2x3.
- **Braille**, **Octant**: 2x4, the finest grids.

Braille needs a font with the Unicode Braille Patterns; Sextant and Octant need the Symbols for Legacy Computing block, which fewer terminals support. A terminal without the glyphs shows replacement characters (`�`).

## See Also

- [barchart](barchart.md) - For categorical data visualization
- [sparkline](sparkline.md) - For compact inline charts
- [canvas](canvas.md) - For custom graphics
