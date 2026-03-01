# The Mouse Area Widget

The `mouse_area` widget wraps a child and captures mouse events within its bounds. Use it to add click, hover, and movement tracking to any widget.

## Interface

```graphix
val mouse_area: fn(
  ?#on_press: fn(null) -> Any,
  ?#on_release: fn(null) -> Any,
  ?#on_enter: fn(null) -> Any,
  ?#on_exit: fn(null) -> Any,
  ?#on_move: fn({x: f64, y: f64}) -> Any,
  &Widget
) -> Widget
```

## Parameters

- **on_press** — called when the mouse button is pressed inside the area
- **on_release** — called when the mouse button is released inside the area
- **on_enter** — called when the mouse cursor enters the area
- **on_exit** — called when the mouse cursor leaves the area
- **on_move** — called with `{x, y}` coordinates as the mouse moves within the area

The positional argument is a reference to the child widget.

## Examples

```graphix
{{#include ../../examples/gui/mouse_area.gx}}
```

![Mouse Area](./media/mouse_area.png)

## See Also

- [Keyboard Area](keyboard_area.md) — keyboard event capture
- [Button](button.md) — simpler click handling
- [Tooltip](tooltip.md) — hover-triggered popups
