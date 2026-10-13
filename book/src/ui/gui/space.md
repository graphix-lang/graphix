# Space & Rules

Space and rules are layout utilities for controlling whitespace and adding visual dividers between widgets.

## Space

The `space` widget creates empty space with configurable dimensions. Use it to push widgets apart within a row or column.

### Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/space.gxi}}
```

### Parameters

- **width** — horizontal size of the space
- **height** — vertical size of the space

A common pattern is `space(#width: &`Fill)` to push items to opposite ends of a row.

### Example

```graphix
{{#include ../../examples/gui/space.gx}}
```

![Space Widget](./media/space.png)

## Rules

Rules draw horizontal or vertical lines to visually separate content.

### Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/rule.gxi}}
```

### Parameters

- **height** (horizontal_rule) — thickness of the horizontal line in pixels
- **width** (vertical_rule) — thickness of the vertical line in pixels

### Example

```graphix
{{#include ../../examples/gui/rule.gx}}
```

![Rules](./media/rule.png)

## See Also

- [Column](column.md) — vertical layout where rules serve as dividers
- [Row](row.md) — horizontal layout where vertical rules separate sections
- [Scrollable](scrollable.md) — rules work well between scrollable list items
