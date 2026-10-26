# The Text Widget

The `text` widget renders styled text in the terminal. It's a fundamental building block for displaying formatted content with colors, modifiers, and multiple lines. Text is built from `Line` objects, which are in turn composed of `Span` objects.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/text.gxi}}
```

<!-- CR claude for claude: [doc-drift/low] There is no `style` module. Style, Line,
Span, Color, Modifier and Alignment are declared at the tui root (mod.gxi), and
`tui::style` is a function, so `use tui::style::Style` is refused ('no module
`tui::style` in scope'). Say they are `tui`'s own types (`use tui::{Line, Style}`),
described on the style page. Checked with graphix --check. (book-ui-b-02) -->
The `text` widget uses types from the [style](style.md) module including `Style`, `Line`, `Span`, `Color`, `Modifier`, and `Alignment`.

## Text Hierarchy

- **Span**: A single segment of text with a single style
- **Line**: A collection of spans forming one line
- **Text**: A collection of lines forming multi-line content

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/text_basic.gx}}
```

![Basic Text](./media/text_basic.png)

### Status Messages

```graphix
{{#include ../../examples/tui/text_status.gx}}
```

![Multi Line Status](./media/text_status.png)

### Dynamic Colors

```graphix
{{#include ../../examples/tui/text_dynamic.gx}}
```

![Dynamic Text](./media/text_dynamic.gif)

### Alignment

```graphix
{{#include ../../examples/tui/text_alignment.gx}}
```

![Text Alignment](./media/text_alignment.png)

## Color Support

- **Named colors**: `Red`, `Green`, `Blue`, `Yellow`, `Magenta`, `Cyan`, `White`, `Black`, `Gray`, `DarkGray`, and `Light*` variants
- **Indexed colors**: `Indexed(202)` for 256-color palette
- **RGB colors**: `Rgb({r: 255, g: 100, b: 50})` for true color

## Text Modifiers

- `Bold`, `Dim`, `Italic`, `Underlined`, `SlowBlink`, `RapidBlink`, `Reversed`, `Hidden`, `CrossedOut`

## See Also

- [paragraph](paragraph.md) - For wrapped and scrollable text
- [block](block.md) - For containing text with borders
- [list](list.md) - For selectable text items
