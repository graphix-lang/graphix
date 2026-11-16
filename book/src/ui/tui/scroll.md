# The Scrollbar Widget

The `scrollbar` widget adds a visual scrollbar indicator to scrollable content, making it clear when content extends beyond the visible area and showing the current scroll position.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/scrollbar.gxi}}
```

## Parameters

- **position** - Current scroll position, typically the Y offset (default: 0)
- **content_length** - The number of scroll positions in the content; nothing measures the child, so a scrollbar without one draws no bar (default: 0)
- **viewport_length** - How many positions the view shows at once, which sizes the thumb (default: the bar's own length)
- **begin_symbol**, **end_symbol** - The arrows at each end; null, the default, draws none
- **track_symbol** - The track the thumb runs along; null, the default, draws none
- **thumb_symbol** - The thumb; null, the default, keeps ratatui's own
- **size** (output) - Rendered size of the scrollbar area; pass `&mut size` where `size: tui::Size`

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/scroll_basic.gx}}
```

<!-- CR claude for claude: [doc-drift/low] Six TUI screenshots show programs their
examples no longer are. scroll_basic.png shows one wrapped sentence for 50 'Line N'
rows; scroll_paragraph.gif shows 'Very long text content...' for 100 lines;
scroll_list.png shows 5 items for 30. paragraph_scrollable.gif (paragraph.md) shows a
wrapped sentence for 60 numbered lines; paragraph_log_viewer.gif shows a syslog line for
the simulated entries; block_focus.png (block.md) shows one block where the example
draws two panes that Tab moves between. Re-capture them from the current examples. Found
by reading; not run. (book-ui-a-02) -->
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
