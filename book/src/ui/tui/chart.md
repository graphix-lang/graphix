# The Chart Widget

The `chart` widget renders line charts with multiple datasets, custom axes, labels, and styling. It's ideal for visualizing time series data, trends, sensor readings, and any numeric data relationships.

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

- **Dot**: Fastest, lowest resolution, good for dense data
- **Braille**: Smoothest curves, medium performance, best visual quality
- **Block**: High contrast, medium performance

## See Also

- [barchart](barchart.md) - For categorical data visualization
- [sparkline](sparkline.md) - For compact inline charts
- [canvas](canvas.md) - For custom graphics
