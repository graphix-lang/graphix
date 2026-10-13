# The Clipboard Module

The `clipboard` module provides functions for reading from and writing to the system clipboard. Unlike other GUI components, clipboard is a plain module of functions, not a widget constructor.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/clipboard.gxi}}
```

## Functions

### Text
- **read_text** — reads text from the clipboard. Takes an event trigger (`Any`) to control when the read happens.
- **write_text** — writes a string to the clipboard.

### Images
- **read_image** — reads an image from the clipboard as a `ClipboardImage` containing raw pixel data.
- **write_image** — writes a `ClipboardImage` to the clipboard.

### HTML
- **read_html** — reads HTML content from the clipboard.
- **write_html** — writes HTML content with alt text to the clipboard.

### Files
- **read_files** — reads file paths from the clipboard (e.g. from a file manager copy).
- **write_files** — writes file paths to the clipboard.

### Utility
- **clear** — clears all clipboard contents.

All read functions take an `Any` argument as an event trigger — they execute when the trigger fires. All functions return `Result` types that may contain a `ClipboardError`.

## Examples

```graphix
{{#include ../../examples/gui/clipboard.gx}}
```

![Clipboard](./media/clipboard.png)

## See Also

- [Button](button.md) — commonly used to trigger clipboard operations
- [Text Input](text_input.md) — text entry for clipboard content
