# The Image Widget

The `image` widget displays raster images from files, raw bytes or pixel buffers, and SVG images from files or inline XML, with sizing and content fitting controls.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/image.gxi}}
```

## Parameters

The labeled arguments:

- **width** - Horizontal sizing as a `Length`. Defaults to `` `Shrink ``.
- **height** - Vertical sizing as a `Length`. Defaults to `` `Shrink ``.
- **content_fit** - Controls how the content is scaled to fill the available space:
  - `` `Fill `` -- Stretch to fill the entire area, ignoring aspect ratio.
  - `` `Contain `` -- Scale uniformly to fit within the area, preserving aspect ratio. May leave empty space.
  - `` `Cover `` -- Scale uniformly to cover the entire area, preserving aspect ratio. May crop content.
  - `` `None `` -- Display at the original size with no scaling.
  - `` `ScaleDown `` -- Like `` `Contain `` but only scales down, never up.

## Image Sources

The `image` widget accepts an `ImageSource` union:

- **string** -- A file path to a PNG, JPEG, BMP, GIF, or other supported image format, or to an `.svg` or `.svgz` file. The path is relative to the working directory.
- **`` `Svg(string) ``** -- Inline SVG XML.
- **`` `Bytes(bytes) ``** -- Raw image file bytes (e.g. the contents of a PNG file loaded with `sys::fs::read_all_bin`). Useful when image data comes from a network source or is embedded in the program. Bytes literals use the `bytes:<base64>` syntax.
- **`` `Rgba({width, height, pixels}) ``** -- A raw RGBA pixel buffer. The `width` and `height` fields specify the image dimensions, and `pixels` is a `bytes` value containing `width * height * 4` bytes (one byte each for red, green, blue, and alpha per pixel, in row-major order).

An image delivered again with the same source keeps what it has drawn.

## Examples

### Image

```graphix
{{#include ../../examples/gui/image.gx}}
```

![Image](./media/image.png)

### SVG

```graphix
{{#include ../../examples/gui/svg.gx}}
```

![SVG](./media/svg.png)

## See Also

- [canvas](canvas.md) - Programmatic 2D drawing
- [types](types.md) - ContentFit and other shared type definitions
