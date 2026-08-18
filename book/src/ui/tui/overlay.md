# The Overlay Widget

The `overlay` widget draws a reactive stack of layers over a base widget —
the modal and popup primitive. Each layer is centered, sized by its
constraints, and cleared before it draws, so the base never bleeds
through. While any layer is up, the topmost layer captures **all**
input; when the layers array is empty, the overlay is just its base.
Open a modal by pushing a layer, close it by popping it — in practice,
by deriving the layers array from your state with `select`.

## Interface

```graphix
val layer: fn(
  ?#width: &Constraint,
  ?#height: &Constraint,
  ?#size: &[Size, null],
  child: Tui
) -> Layer;

val overlay: fn(#layers: &Array<Layer>, base: Tui) -> Tui;
```

## Parameters

### layer

- **width** - Horizontal size of the layer, as a layout `Constraint`
  (default `` `Percentage(60) ``)
- **height** - Vertical size (default `` `Percentage(60) ``)
- **size** - Observes the rectangle the layer was actually given, like
  `layout::child`'s
- **child** - The widget drawn in the layer

### overlay

- **layers** - The stack of layers, drawn in order; the last is topmost
  and captures input
- **base** - The widget under the stack

## Example

A modal opened with `o` and closed with `Esc`. While it is up, its
input handler sees every key; the base handler sees none.

```graphix
{{#include ../../examples/tui/overlay_modal.gx}}
```
