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
window(#size: &mut { width: 1024.0, height: 768.0 }, &content)
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

RGBA color with floating-point components in the range 0.0 to 1.0. Color is an abstract type — use the `color` constructor to create values. Components default to 0.0 except alpha which defaults to 1.0. Out-of-range values return an `InvalidColor` error.

```graphix
// Solid red ($ swallows errors with a warning)
let red = color(#r: 1.0)$

// Semi-transparent blue (? propagates errors)
let blue_50 = color(#b: 1.0, #a: 0.5)?
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

<!-- CR claude for claude: [doc-drift/low] The `type Widget` block below is stale. It
lists `` `Svg(svg::Svg) ``, but there is no svg module (SVG goes through `image` with
`ImageSource`'s `` `Svg ``). It also omits ContextMenu, Grid, Markdown, MenuBar, QrCode,
Table and DataTable, which the `Widget` type in gui's mod.gxi has. Copy the block from
mod.gxi, or replace it with a sentence and a link so it cannot drift again.
(book-ui-a-05) -->
<!-- CR claude for claude: [doc-drift/medium] The Widget type below is out of date with
gui's mod.gxi. It lacks `ContextMenu, `Grid, `Markdown, `MenuBar, `QrCode, `Table and
`DataTable, and it lists a `Svg variant that does not exist, so a reader who matches on
Widget writes an arm the checker refuses. Four other hand-copied blocks have drifted
too. overview.md's Window and window lack the icon field and the ?#icon argument that
every example passes. radio.md gives #selected as &'a where radio.gxi has &['a, null].
menu.md's MenuAction gives on_click as &fn(a: null) -> Any where menu.gxi has &[fn(e:
null) -> Any, null]. Replace hand-copied interface blocks with {{#include}} of the .gxi,
as image.md, chart.md, keyboard_area.md and data_table.md do; for mod.gxi, include a
line range or an anchor. Also rewrite overview.md's "`use gui` brings the top-level GUI
module into scope" bullet to describe the `use gui::{window, text::text, ..}` form that
hello.gx uses. (examples-gui-02) -->
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

<!-- CR claude for claude: [doc-drift/medium] The example below does not compile. `use
gui; use gui::text; ..` imports the module names, so `column` is not defined and
`text(..)` names the module. The page intro has the same mistake ("become available when
you write `use gui`"): after `use gui;`, `Length` is an undefined type. overview.md's
bullets under 'A few things to notice' repeat it and do not match the `use gui::{window,
button::button, ..}` that hello.gx actually writes. Write `use gui::{column::column,
text::text, button::button, slider::slider};`, say that the shared types are written
`gui::Length` or imported by name (`use gui::{Length, Padding}`), and fix the overview
bullets to match. probe:
design/review-2026-10-12/repro/book-ui-a-types-widget-imports.gx (book-ui-a-04) -->
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
