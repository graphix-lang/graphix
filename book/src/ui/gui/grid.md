# The Grid Widget

A layout container that arranges child widgets in a grid with a fixed number of columns. Items wrap to the next row automatically.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/grid.gxi}}
```

## Parameters

- **`#spacing`** -- Space in pixels between grid cells, applied both horizontally and vertically. Defaults to `0.0`.
- **`#columns`** -- How columns are determined. `` `Fixed(n) `` uses exactly `n` columns. `` `Fluid(max_width) `` uses as many columns as needed so that none is wider than `max_width` pixels. Defaults to `` `Fixed(3) ``.
- **`#width`** -- Total width of the grid in pixels, or `null` to use the available space. Defaults to `null`.
- **`#height`** -- How row heights are determined. `` `AspectRatio(ratio) `` sets each cell's height relative to its width (e.g. `1.0` for square cells). `` `EvenlyDistribute(len) `` divides the given `Length` evenly among all rows. Defaults to `` `AspectRatio(1.0) ``.
- **positional `&Array<Widget>`** -- The child widgets to arrange in the grid. They fill left-to-right, top-to-bottom.

## Examples

<!-- CR claude for claude: [doc-drift/low] The included grid.gx sets no colors: `item`
is a plain container around text. Rename the heading to describe what the example shows,
or give `item` a container style so the heading is true. Found by reading; not run.
(examples-gui-02) -->
### Colored Grid Items

```graphix
{{#include ../../examples/gui/grid.gx}}
```

![Grid](./media/grid.png)

## See Also

- [column](column.md) -- single-column vertical layout
- [row](row.md) -- single-row horizontal layout
- [types](types.md) -- for `Length` and other shared types
