# Types

The GUI library defines a set of shared types used across all widgets. These types control layout, sizing, colors, fonts, and more. They are all defined in the top-level `gui` module and become available when you write `use gui`.

## Layout Types

### Length

Controls how a widget is sized along a single axis:

```graphix
type Length = [`Fill, `FillPortion(i64), `Shrink, `Fixed(f64)];
```

- `` `Fill `` -- expand to fill all available space.
- `` `FillPortion(n) `` -- fill proportionally. Two widgets with `FillPortion(1)` and `FillPortion(2)` split space 1:2.
- `` `Shrink `` -- take only as much space as the content needs.
- `` `Fixed(px) `` -- exact size in logical pixels.

Most layout widgets accept `#width` and `#height` parameters of type `&Length`:

```graphix
column(
  #width: &`Fill,
  #height: &`Fixed(300.0),
  &[...]
)
```

### Padding

Controls spacing between a widget's border and its content:

```graphix
type Padding = [
  `All(f64),
  `Axis({x: f64, y: f64}),
  `Each({top: f64, right: f64, bottom: f64, left: f64})
];
```

- `` `All(px) `` -- uniform padding on all sides.
- `` `Axis({x, y}) `` -- separate horizontal (`x`) and vertical (`y`) padding.
- `` `Each({top, right, bottom, left}) `` -- individual padding per side.

```graphix
container(
  #padding: &`All(20.0),
  &text(&"Padded content")
)

container(
  #padding: &`Each({top: 10.0, right: 20.0, bottom: 10.0, left: 20.0}),
  &text(&"Different padding per side")
)
```

### Size

A width/height pair used for window dimensions:

```graphix
type Size = { width: f64, height: f64 };
```

```graphix
window(#size: &{ width: 1024.0, height: 768.0 }, &content)
```

### HAlign and VAlign

Horizontal and vertical alignment for positioning content within a container:

```graphix
type HAlign = [`Left, `Center, `Right];
type VAlign = [`Top, `Center, `Bottom];
```

```graphix
column(
  #halign: &`Center,
  #width: &`Fill,
  &[text(&"Centered text")]
)

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

RGBA color with floating-point components in the range 0.0 to 1.0:

```graphix
type Color = { r: f64, g: f64, b: f64, a: f64 };
```

```graphix
// Solid red
let red = { r: 1.0, g: 0.0, b: 0.0, a: 1.0 }

// Semi-transparent blue
let blue_50 = { r: 0.0, g: 0.0, b: 1.0, a: 0.5 }
```

Colors are used in custom themes and per-widget style overrides. See the [theming](theming.md) page for details.

### Font Types

Fonts are described by family, weight, and style:

```graphix
type FontFamily = [`SansSerif, `Serif, `Monospace, `Name(string)];
type FontWeight = [
  `Thin, `ExtraLight, `Light, `Normal, `Medium,
  `SemiBold, `Bold, `ExtraBold, `Black
];
type FontStyle = [`Normal, `Italic, `Oblique];
type Font = { family: FontFamily, weight: FontWeight, style: FontStyle };
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
type ScrollDirection = [`Vertical, `Horizontal, `Both];
```

```graphix
scrollable(#direction: &`Both, &content)
```

### TooltipPosition

Controls where a tooltip appears relative to its target widget:

```graphix
type TooltipPosition = [`Top, `Bottom, `Left, `Right, `FollowCursor];
```

```graphix
tooltip(
  #position: &`Top,
  #tip: &text(&"Tooltip text"),
  &button(&text(&"Hover me"))
)
```

### ContentFit

Controls how an image or SVG is scaled within its bounds:

```graphix
type ContentFit = [`Fill, `Contain, `Cover, `None, `ScaleDown];
```

- `` `Fill `` -- stretch to fill the bounds exactly (may distort).
- `` `Contain `` -- scale to fit within bounds, preserving aspect ratio.
- `` `Cover `` -- scale to cover bounds, preserving aspect ratio (may crop).
- `` `None `` -- no scaling, display at original size.
- `` `ScaleDown `` -- like `` `Contain `` but never scales up.

## The Widget Type

`Widget` is a union of all individual widget types. Most widget constructor functions return `Widget`, which means you can freely mix different widget types in arrays and containers:

```graphix
type Widget = [
  `Button(button::Button),
  `Canvas(canvas::Canvas),
  `Chart(chart::Chart),
  `Checkbox(checkbox::Checkbox),
  `Column(column::Column),
  `ComboBox(combo_box::ComboBox),
  `Container(container::Container),
  `HorizontalRule(rule::HorizontalRule),
  `Image(image::Image),
  `KeyboardArea(keyboard_area::KeyboardArea),
  `MouseArea(mouse_area::MouseArea),
  `PickList(pick_list::PickList),
  `ProgressBar(progress_bar::ProgressBar),
  `Radio(radio::Radio),
  `Row(row::Row),
  `Scrollable(scrollable::Scrollable),
  `Slider(slider::Slider),
  `Space(space::Space),
  `Stack(stack::Stack),
  `Svg(svg::Svg),
  `Text(text::Text),
  `TextEditor(text_editor::TextEditor),
  `TextInput(text_input::TextInput),
  `Toggler(toggler::Toggler),
  `Tooltip(tooltip::Tooltip),
  `VerticalRule(rule::VerticalRule),
  `VerticalSlider(vertical_slider::VerticalSlider)
];
```

You do not normally construct `Widget` variants directly. Each widget module exports a constructor function (e.g., `gui::text::text(...)`, `gui::button::button(...)`) that returns `Widget`. This means a column's children array has type `Array<Widget>` and can contain any mix of widgets:

```graphix
use gui;
use gui::text;
use gui::button;
use gui::slider;

let v = 50.0

column(
  #spacing: &10.0,
  &[
    text(&"A text widget"),
    button(&text(&"A button")),
    slider(#min: &0.0, #max: &100.0, &v)
  ]
)
```
