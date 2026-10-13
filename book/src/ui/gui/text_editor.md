# The Text Editor Widget

A multi-line text editing area for longer-form content. Unlike `text_input`, which handles a single line, `text_editor` supports multiple lines of text with scrolling. It reports the full content string on every edit.

## Interface

```graphix
val text_editor: fn(
  ?#placeholder: &string,
  ?#on_edit: [fn(s: string) -> Any, null],
  ?#width: &[f64, null],
  ?#height: &[f64, null],
  ?#padding: &Padding,
  ?#font: &[Font, null],
  ?#size: &[f64, null],
  ?#disabled: &bool,
  s: &string
) -> Widget
```

## Parameters

- **`#placeholder`** -- Hint text displayed when the editor is empty.
- **`#on_edit`** -- Callback invoked on every edit. Receives the full content as a `string`. Use with `<-` to keep state in sync: `#on_edit: |v| content <- v`.
- **`#width`** -- Width in pixels as `f64`, or `null` for automatic sizing. Note: this is `&[f64, null]`, not `&Length` -- the text editor uses fixed pixel dimensions rather than the layout-based `Length` type.
- **`#height`** -- Height in pixels as `f64`, or `null` for automatic sizing. Same `&[f64, null]` type as width.
- **`#padding`** -- Interior padding around the text content. Accepts `Padding` values.
- **`#font`** -- Font to use, or `null` for the default font.
- **`#size`** -- Font size in pixels, or `null` for the default size.
<!-- CR claude for claude: [doc-drift/low] A disabled editor can still be clicked, and
its text scrolled, selected and copied. Only edits are refused. An editor without
`#on_edit` is read-only in the same way, and the `#on_edit` line does not say so. Write:
'disabled: edits are refused; the text can still be scrolled, selected and copied.
Without #on_edit the editor is read-only in the same way.' (book-ui-a-13) -->
- **`#disabled`** -- When `true`, the editor cannot be focused or edited. Defaults to `false`.
- **positional `&string`** -- Reference to the current content. The editor displays this text and you update it from the `#on_edit` callback.

## Examples

### Basic Text Editor

```graphix
{{#include ../../examples/gui/text_editor.gx}}
```

![Text Editor](./media/text_editor.gif)

## See Also

- [text_input](text_input.md) -- single-line text input
- [text](text.md) -- read-only text display
