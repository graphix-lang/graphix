# The Block Widget

The `block` widget is a container that wraps other widgets with optional borders, titles, and styling. It's one of the most commonly used widgets for creating structured layouts and visually separating different sections of your TUI.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/block.gxi}}
```

## Parameters

- **border** - Which sides have a border: `` `All ``, `` `None ``, or an array of sides, e.g. `` &[`Top, `Left] `` (one side alone is still an array, `` &[`Top] ``)
- **border_style** - Style for the border
- **merge_borders** - Strategy for merging borders of adjacent blocks: `Replace`, `Exact`, or `Fuzzy`
- **title** - Line displayed at the top of the block
- **title_bottom** - Line displayed at the bottom of the block
- **style** - Style for the block's interior
- **size** (output) - Rendered size of the block

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/block_basic.gx}}
```

![Basic Block](./media/block_basic.png)

### Focus Indication

Use dynamic styling to show which block has focus; Tab moves it:

```graphix
{{#include ../../examples/tui/block_focus.gx}}
```

![Styled Block](./media/block_focus.png)

### Dynamic Titles

Titles can contain reactive values that update automatically:

```graphix
{{#include ../../examples/tui/block_dynamic_title.gx}}
```

![Block With Dynamic Title](./media/block_dynamic_title.png)

## See Also

- [layout](layout.md) - For arranging multiple blocks
- [paragraph](paragraph.md) - Common content for blocks
- [text](text.md) - For creating styled text content
