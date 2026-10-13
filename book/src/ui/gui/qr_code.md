# The QR Code Widget

Encodes a string as a QR code image. The QR code updates reactively when the input string changes.

## Interface

```graphix
val qr_code: fn(
  ?#cell_size: &[f64, null],
  s: &string
) -> Widget
```

## Parameters

- **`#cell_size`** -- Size in pixels of each cell in the QR code matrix, or `null` for the default size. Larger values produce a bigger QR code image.
<!-- CR claude for claude: [doc-drift/low] Not every string encodes. Data beyond QR
capacity (about 2.9 KB in byte mode) fails in iced's `qr_code::Data::new`, and the
widget then renders nothing and only logs 'qr_code: failed to encode data'. Say that
data past QR capacity renders an empty space and logs an error. (book-ui-a-15) -->
- **positional `&string`** -- The data to encode. Any valid string will be encoded into the QR code.

## Examples

### QR Code from Text Input

```graphix
{{#include ../../examples/gui/qr_code.gx}}
```

![QR Code](./media/qr_code.png)

## See Also

- [text](text.md) -- for displaying text alongside a QR code
- [image](image.md) -- for displaying raster images
