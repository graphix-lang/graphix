# The Chart Widget

The `chart` widget renders data visualizations with multiple datasets, axis labels, and automatic or manual axis scaling. It supports line charts, scatter plots, bar charts, area charts, dashed lines, candlestick charts, and error bars. Multiple series types can be mixed on the same chart.

## Interface

```graphix
type SeriesStyle = {
  color: [Color, null],
  label: [string, null],
  stroke_width: [f64, null],
  point_size: [f64, null]
};

type CandlestickStyle = {
  gain_color: [Color, null],
  loss_color: [Color, null],
  bar_width: [f64, null],
  label: [string, null]
};

type OhlcPoint = {x: f64, open: f64, high: f64, low: f64, close: f64};
type ErrorBarPoint = {x: f64, min: f64, avg: f64, max: f64};

type Dataset = [
  `Line({data: &Array<(f64, f64)>, style: SeriesStyle}),
  `Scatter({data: &Array<(f64, f64)>, style: SeriesStyle}),
  `Bar({data: &Array<(f64, f64)>, style: SeriesStyle}),
  `Area({data: &Array<(f64, f64)>, style: SeriesStyle}),
  `DashedLine({data: &Array<(f64, f64)>, dash: f64, gap: f64, style: SeriesStyle}),
  `Candlestick({data: &Array<OhlcPoint>, style: CandlestickStyle}),
  `ErrorBar({data: &Array<ErrorBarPoint>, style: SeriesStyle})
];

type MeshStyle = {
  show_x_grid: [bool, null],
  show_y_grid: [bool, null],
  grid_color: [Color, null],
  axis_color: [Color, null],
  label_color: [Color, null],
  label_size: [f64, null],
  x_labels: [i64, null],
  y_labels: [i64, null]
};

type LegendStyle = {
  background: [Color, null],
  border: [Color, null],
  label_color: [Color, null]
};

type LegendPosition = [
  `UpperLeft, `UpperRight, `LowerLeft, `LowerRight,
  `MiddleLeft, `MiddleRight, `UpperMiddle, `LowerMiddle
];

val chart: fn(
  ?#title: &[string, null],
  ?#title_color: &[Color, null],
  ?#x_label: &[string, null],
  ?#y_label: &[string, null],
  ?#x_range: &[{min: f64, max: f64}, null],
  ?#y_range: &[{min: f64, max: f64}, null],
  ?#width: &Length,
  ?#height: &Length,
  ?#background: &[Color, null],
  ?#margin: &[f64, null],
  ?#title_size: &[f64, null],
  ?#legend_position: &[LegendPosition, null],
  ?#legend_style: &[LegendStyle, null],
  ?#mesh: &[MeshStyle, null],
  &Array<Dataset>
) -> Widget
```

## Chart Parameters

- **title** — Chart title displayed above the plot area. Null for no title.
- **title_color** — Color of the chart title text. Defaults to black when null.
- **x_label** — Label for the x-axis. Null for no label.
- **y_label** — Label for the y-axis. Null for no label.
- **x_range** — Manual x-axis range as `{min: f64, max: f64}`. When null, the range is computed automatically from the data.
- **y_range** — Manual y-axis range. Same format as x_range.
- **width** — Horizontal sizing as a `Length`. Defaults to `` `Fill ``.
- **height** — Vertical sizing as a `Length`. Defaults to `` `Fill ``.
- **background** — Background color as a `Color` struct. Defaults to white when null.
- **margin** — Margin in pixels around the plot area. Defaults to 10.
- **title_size** — Font size for the chart title. Defaults to 16.
- **legend_position** — Position of the series legend. Defaults to `` `UpperLeft `` when null.
- **legend_style** — Legend appearance via a `LegendStyle` struct. Controls background, border, and text color. When null, defaults to white background with black border.
- **mesh** — Grid and axis styling via a `MeshStyle` struct. When null, plotters defaults are used.

The positional argument is a reference to an array of `Dataset` values. Multiple datasets can be plotted on the same axes.

## Series Constructors

Rather than constructing `Dataset` variants directly, use these convenience functions. All style parameters are optional and default to null.

### `chart::line`

```graphix
chart::line(#label: "Price", #color: {r: 1.0, g: 0.0, b: 0.0, a: 1.0}, &data)
```

Points connected by straight line segments. Good for time series and trends.

### `chart::scatter`

```graphix
chart::scatter(#label: "Points", #point_size: 5.0, &data)
```

Individual points without connecting lines. Good for distributions and correlations.

### `chart::bar`

```graphix
chart::bar(#label: "Counts", &data)
```

Vertical bars from the x-axis to each data point. Good for categorical comparisons.

### `chart::area`

```graphix
chart::area(#label: "Volume", &data)
```

Like line but with the region between the line and the x-axis filled with 30% opacity.

### `chart::dashed_line`

```graphix
chart::dashed_line(#label: "Projection", #dash: 10.0, #gap: 5.0, &data)
```

A dashed line series. `dash` and `gap` control the dash and gap lengths in pixels. Defaults to 5.0 each.

### `chart::candlestick`

```graphix
chart::candlestick(#label: "OHLC", &ohlc_data)
```

Financial candlestick chart. Data is `Array<OhlcPoint>` where each point has `{x, open, high, low, close}` fields. Gain candles (close > open) are green by default; loss candles are red. Override with `#gain_color` and `#loss_color`.

### `chart::error_bar`

```graphix
chart::error_bar(#label: "Confidence", &error_data)
```

Vertical error bars. Data is `Array<ErrorBarPoint>` where each point has `{x, min, avg, max}` fields. A circle is drawn at the average value with whiskers extending to min and max.

## Series Style Fields

The `SeriesStyle` struct (used by line, scatter, bar, area, dashed_line, error_bar) has:

- **color** — Series color. When null, a color is assigned from a default palette.
- **label** — Display name shown in the legend. When null, no legend entry is created.
- **stroke_width** — Line/stroke width in pixels. Defaults to 2.
- **point_size** — Point radius for scatter plots. Defaults to 3.

The `CandlestickStyle` struct (used by candlestick) has:

- **gain_color** — Color for gain candles (close > open). Defaults to green.
- **loss_color** — Color for loss candles (close <= open). Defaults to red.
- **bar_width** — Width of candlestick bodies in pixels. Defaults to 5.
- **label** — Display name shown in the legend.

## Mesh Style Fields

- **show_x_grid** — Show vertical grid lines. Defaults to true when null.
- **show_y_grid** — Show horizontal grid lines. Defaults to true when null.
- **grid_color** — Color of grid lines.
- **axis_color** — Color of axis lines.
- **label_color** — Color of tick labels and axis descriptions. Essential for dark backgrounds where the default black text is invisible.
- **label_size** — Font size for axis labels.
- **x_labels** — Number of x-axis tick labels.
- **y_labels** — Number of y-axis tick labels.

## Legend Style Fields

- **background** — Legend background color. Defaults to white when null.
- **border** — Legend border color. Defaults to black when null.
- **label_color** — Color of legend text labels. Defaults to plotters default (black) when null.

## Examples

### Multiple Datasets

```graphix
{{#include ../../examples/gui/chart.gx}}
```

![Chart](./media/chart.gif)

### Bar Chart

```graphix
{{#include ../../examples/gui/chart_bar.gx}}
```

### Candlestick

```graphix
{{#include ../../examples/gui/chart_candlestick.gx}}
```

### Error Bars

```graphix
{{#include ../../examples/gui/chart_error_bar.gx}}
```

### Styling

```graphix
{{#include ../../examples/gui/chart_styles.gx}}
```

## See Also

- [canvas](canvas.md) — Low-level drawing for custom visualizations
