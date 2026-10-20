# The Layout Widget

The `layout` widget arranges child widgets in horizontal or vertical layouts with flexible sizing constraints. It's the primary tool for organizing complex TUI interfaces and supports focus management for interactive applications.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/layout.gxi}}
```

## Parameters

- **direction** - `Horizontal` or `Vertical` (default: `Vertical`)
- **focused** - Index of the currently focused child (0-indexed, the first when not set). Every event, keys and mouse alike, goes only to the focused child: an `input_handler` in another child hears nothing, and a click over another pane goes to the focused one
- **flex** - How leftover space is distributed when children don't fill it: `Legacy`, `Start`, `End`, `Center`, `SpaceBetween`, `SpaceEvenly`, `SpaceAround`

## Constraint Types

- **Percentage(n)** - Allocates n% of available space
- **Length(n)** - Fixed width/height in cells
- **Min(n)** - At least n cells
- **Max(n)** - At most n cells
- **Ratio(num, den)** - Fractional allocation (num/den)
- **Fill(n)** - Takes remaining space after other constraints

## Examples

### Basic Layout

```graphix
{{#include ../../examples/tui/layout_basic.gx}}
```

![Basic Layout](./media/layout_basic.png)

### Three-Pane Layout with Focus

```graphix
{{#include ../../examples/tui/layout_focus.gx}}
```

![Layout With Focus](./media/layout_focus.gif)

### Nested Layouts

```graphix
{{#include ../../examples/tui/layout_nested.gx}}
```

![Nested Layout](./media/layout_nested.png)

### Header/Content/Footer

```graphix
{{#include ../../examples/tui/layout_header_footer.gx}}
```

![Layout With Header and Footer](./media/layout_header_footer.png)

### Nested Layout with Focus and Scrolling

Nested layouts whose focused pane is highlighted, with a long paragraph
scrolled beside a scrollbar.

```graphix
{{#include ../../examples/tui/layout_nested_focus.gx}}
```

## See Also

- [block](block.md) - Common child widget for layouts
- [input_handler](input.md) - For handling focus changes
