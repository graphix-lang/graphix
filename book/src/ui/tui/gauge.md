# The Gauge Widget

The `gauge` widget displays a single value as a filled progress indicator, perfect for showing percentages, completion status, or resource usage. It provides a clear visual representation of how full or complete something is.

## API

```graphix
mod gauge: sig {
    /// Creates a gauge widget showing progress from 0.0 to 1.0
    val gauge: fn(
        ?#gauge_style: &Style,
        ?#label: &Line,
        ?#use_unicode: &bool,
        ?#style: &Style,
        &f64
    ) -> Widget;
}
```

## Parameters

- **gauge_style** - Style for the filled portion
- **label** - Line or span displayed in the center
- **use_unicode** - Use Unicode block characters for smoother rendering
- **style** - Style for the unfilled portion

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/gauge_basic.gx}}
```

![Basic Gauge](./media/gauge_basic.png)

### Progress with Color Thresholds

```graphix
{{#include ../../examples/tui/gauge_threshold.gx}}
```

![Gauge With Color](./media/gauge_threshold.gif)

### Resource Usage

```graphix
{{#include ../../examples/tui/gauge_resource.gx}}
```

![Resource Usage Gauge](./media/gauge_resource.png)

## See Also

- [linegauge](linegauge.md) - For horizontal line-based gauges
- [sparkline](sparkline.md) - For historical trend display
- [barchart](barchart.md) - For comparing multiple values
