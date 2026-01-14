# Style

Styles control the visual appearance of TUI widgets, including colors and text
modifiers. Most widgets accept style parameters to customize their appearance.

## Colors

The `Color` type defines available colors:

```graphix
type Color = [
  `Reset,
  `Black,
  `Red,
  `Green,
  `Yellow,
  `Blue,
  `Magenta,
  `Cyan,
  `Gray,
  `DarkGray,
  `LightRed,
  `LightGreen,
  `LightYellow,
  `LightBlue,
  `LightMagenta,
  `LightCyan,
  `White,
  `Rgb({ r: i64, g: i64, b: i64 }),
  `Indexed(i64)
];
```

Use `Reset` to return to the terminal's default color. Use `Rgb` for 24-bit true
color, or `Indexed` for 256-color palette indices.

## Modifiers

Text modifiers change the appearance of text:

```graphix
type Modifier = [
  `Bold,
  `Italic
];
```

## The Style Type

A `Style` combines foreground color, background color, underline color, and
modifiers:

```graphix
type Style = {
  fg: [Color, null],
  bg: [Color, null],
  underline_color: [Color, null],
  add_modifier: [Array<Modifier>, null],
  sub_modifier: [Array<Modifier>, null]
};
```

## Creating Styles

Use the `tui::style` function to create styles. All parameters are optional:

```graphix
val style: fn(
  ?#fg: [Color, null],
  ?#bg: [Color, null],
  ?#underline_color: [Color, null],
  ?#add_modifier: [Array<Modifier>, null],
  ?#sub_modifier: [Array<Modifier>, null]
) -> Style;
```

Examples:

```graphix
// Red foreground
tui::style(#fg: `Red)

// Green text on black background
tui::style(#fg: `Green, #bg: `Black)

// Bold yellow text
tui::style(#fg: `Yellow, #add_modifier: [`Bold])

// Bold italic text with RGB color
tui::style(#fg: `Rgb({ r: 255, g: 128, b: 0 }), #add_modifier: [`Bold, `Italic])

// Default style (no customization)
tui::style()
```

## Spans and Lines

Styles are commonly used with `Span` and `Line` types to create styled text.

### Spans

A `Span` is a piece of text with a single style:

```graphix
type Span = {
  style: Style,
  content: string
};

val span: fn(?#style: Style, string) -> Span;
```

Example:

```graphix
// Create a red "Error:" span
tui::span(#style: tui::style(#fg: `Red), "Error:")

// Plain text span (default style)
tui::span("Hello")
```

### Lines

A `Line` contains one or more spans with optional alignment:

```graphix
type Alignment = [
  `Left,
  `Center,
  `Right
];

type Line = {
  style: Style,
  alignment: [Alignment, null],
  spans: [Array<Span>, string]
};

val line: fn(?#style: Style, ?#alignment: [Alignment, null], [Array<Span>, string]) -> Line;
```

The `spans` field can be either an array of spans (for mixed styling) or a
simple string (which will use the line's style).

Examples:

```graphix
// Simple centered line
tui::line(#alignment: `Center, "Centered Text")

// Line with mixed styles
tui::line([
  tui::span(#style: tui::style(#fg: `Red, #add_modifier: [`Bold]), "Error: "),
  tui::span("Something went wrong")
])

// Right-aligned with style
tui::line(#style: tui::style(#fg: `Gray), #alignment: `Right, "Status: OK")
```

## Using Styles with Widgets

Most widgets accept style parameters. For example:

```graphix
// Styled paragraph
tui::paragraph::paragraph(
  #style: tui::style(#fg: `White, #bg: `Blue),
  "Hello, styled world!"
)

// Styled gauge
tui::gauge::gauge(
  #style: tui::style(#fg: `Green),
  #ratio: 0.75
)
```

See individual widget documentation for specific style parameters they accept.
