# Types

The GUI library defines a set of shared types used across all widgets. These types control layout, sizing, colors, fonts, and more. They are all defined in the top-level `gui` module: write them `gui::Length`, or import them by name, `use gui::{Length, Padding}`.

## Layout Types

### Length

Controls how a widget is sized along a single axis:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:length}}
```

### Padding

Controls spacing between a widget's border and its content:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:padding}}
```

- `` `All(px) `` -- uniform padding on all sides.
- `` `Axis({x, y}) `` -- separate horizontal (`x`) and vertical (`y`) padding.
- `` `Each({top, right, bottom, left}) `` -- individual padding per side.

```graphix
container(
  #padding: &`All(20.0),
  &text(&"Padded content")
);

container(
  #padding: &`Each({top: 10.0, right: 20.0, bottom: 10.0, left: 20.0}),
  &text(&"Different padding per side")
)
```

### Size

A width/height pair used for window dimensions:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:size}}
```

### HAlign and VAlign

Horizontal and vertical alignment for positioning content within a container:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:align}}
```

```graphix
column(
  #halign: &`Center,
  #width: &`Fill,
  &[text(&"Centered text")]
);

container(
  #halign: &`Center,
  #valign: &`Center,
  #width: &`Fill,
  #height: &`Fill,
  &text(&"Dead center")
)
```

## Visual Types

### Color

RGBA color with floating-point components in the range 0.0 to 1.0. Color is an abstract type — use the `color` constructor to create values. Components default to 0.0 except alpha which defaults to 1.0. Out-of-range values return an `InvalidColor` error.

```graphix
// Solid red ($ swallows errors with a warning)
let red = color(#r: 1.0)$;

// Semi-transparent blue (? propagates errors)
let blue_50 = color(#b: 1.0, #a: 0.5)?
```

Colors are used in custom themes and per-widget style overrides. See the [theming](theming.md) page for details.

### Font Types

Fonts are described by family, weight, and style:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:font}}
```

- `FontFamily` selects the font. `` `Name(string) `` allows loading a specific named font.
- `FontWeight` ranges from `` `Thin `` (lightest) to `` `Black `` (heaviest).
- `FontStyle` controls italic/oblique rendering.

```graphix
text(
  #font: &{ family: `Monospace, weight: `Bold, style: `Normal },
  &"Monospace bold text"
)
```

## Content Types

### ScrollDirection

Controls which axes a scrollable widget allows scrolling on:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:scroll_direction}}
```

### TooltipPosition

Controls where a tooltip appears relative to its target widget:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:tooltip_position}}
```

### ContentFit

Controls how an image or SVG is scaled within its bounds:

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/mod.gxi:content_fit}}
```

You do not normally construct `Widget` variants directly. Each widget module exports a constructor function (e.g., `gui::text::text(...)`, `gui::button::button(...)`) that returns `Widget`. This means a column's children array has type `Array<Widget>` and can contain any mix of widgets:

```graphix
use gui::{button::button, column::column, slider::slider, text::text};

let v = 50.0;

column(
  #spacing: &10.0,
  &[
    text(&"A text widget"),
    button(&text(&"A button")),
    slider(#min: &0.0, #max: &100.0, &v)
  ]
)
```
