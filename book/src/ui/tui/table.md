# The Table Widget

The `table` widget displays structured data in rows and columns with support for selection, scrolling, and custom styling. It's ideal for data grids, process monitors, file listings, and any tabular data display.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/table.gxi}}
```

## Parameters

- **header** - Row object for the table header
- **selected** - Index of the currently selected row
- **row_highlight_style** - Style for the selected row
- **highlight_symbol** - String before selected row
- **highlight_spacing** - When to show highlight symbol: `Always`, `WhenSelected`, `Never`
- **widths** - Array of column width constraints
- **column_spacing** - Number of spaces between columns
- **style** - Base style for the table

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/table_basic.gx}}
```

![Basic Table](./media/table_basic.png)

### Interactive Table

```graphix
{{#include ../../examples/tui/table_interactive.gx}}
```

![Interactive Table](./media/table_interactive.gif)

### Conditional Cell Styling

```graphix
{{#include ../../examples/tui/table_styled_cells.gx}}
```

![Conditional Styling](./media/table_styled_cells.png)

### Real-time Updates

```graphix
{{#include ../../examples/tui/table_realtime.gx}}
```

![Realtime Table](./media/table_realtime.gif)

## See Also

- [list](list.md) - For simpler single-column selection
- [scrollbar](scroll.md) - For adding scrollbars
- [block](block.md) - For containing tables with borders
