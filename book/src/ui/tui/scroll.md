# The Scrollbar Widget

The `scrollbar` widget adds a visual scrollbar indicator to scrollable content, making it clear when content extends beyond the visible area and showing the current scroll position.

## Interface

```graphix
type ScrollbarOrientation = [
  `VerticalRight,
  `VerticalLeft,
  `HorizontalBottom,
  `HorizontalTop
];

val scrollbar: fn(
  ?#begin_style: &[Style, null],
  ?#begin_symbol: &[string, null],
  ?#content_length: &[i64, null],
  ?#end_style: &[Style, null],
  ?#end_symbol: &[string, null],
  ?#orientation: &[ScrollbarOrientation, null],
  ?#position: &[i64, null],
  ?#size: &[Size, null],
  ?#style: &[Style, null],
  ?#thumb_style: &[Style, null],
  ?#thumb_symbol: &[string, null],
  ?#track_style: &[Style, null],
  ?#track_symbol: &[string, null],
  ?#viewport_length: &[i64, null],
  a: &Tui
) -> Tui;
```

## Parameters

- **position** - Current scroll position, typically the Y offset (default: 0)
- **content_length** - The number of scroll positions in the content; nothing measures the child, so a scrollbar without one draws no bar (default: 0)
- **viewport_length** - How many positions the view shows at once, which sizes the thumb (default: the bar's own length)
- **size** (output) - Rendered size of the scrollbar area

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/scroll_basic.gx}}
```

![Basic Scrollbar](./media/scroll_basic.png)

### Scrollable Paragraph

```graphix
{{#include ../../examples/tui/scroll_paragraph.gx}}
```

![Scrollable Paragraph](./media/scroll_paragraph.gif)

### Scrollable List

```graphix
{{#include ../../examples/tui/scroll_list.gx}}
```

![Scrollable List](./media/scroll_list.png)


## See Also

- [paragraph](paragraph.md) - For scrollable text content
- [list](list.md) - For scrollable lists
- [table](table.md) - For scrollable tables
- [block](block.md) - For containing scrollable content
