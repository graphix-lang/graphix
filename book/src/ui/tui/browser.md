# The Browser Widget

The `browser` widget provides a specialized interface for browsing and interacting with netidx hierarchies. It displays a table of one level's children, one column per published column when the children are rows of a table, with selection and cursor movement support.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/browser.gxi}}
```

## Parameters

- **path** - The netidx path whose children are listed. The listing refreshes when `path` changes and about once a second, so rows published later appear on the next refresh
- **cursor** - Programmatic cursor movement: `Left(n)`, `Right(n)`, `Up(n)`, `Down(n)`
- **selected_row** (output) - Full path of the selected row (e.g. `/t/r1`), null while the listing is empty
- **selected_path** (output) - Full path of the selected cell: the row in a list, the row and column in a table; null while the listing is empty
- **size** (input) - The area the browser is drawn in, which sizes its viewport; pass the enclosing block's `#size` output
- **rate** - The most often a cell's subscription updates the display (default `duration:0.5s`)
- **flex** - How the columns share leftover width (default `` `SpaceBetween ``)
- **style**, **header_style**, **selected_style** - Styles for the table, its header row and the selected cell

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/browser_basic.gx}}
```

![Basic Browser Widget](./media/browser_basic.png)

### Basic Navigation

```graphix
{{#include ../../examples/tui/browser_navigation.gx}}
```

![Browser With Navigation](./media/browser_navigation.gif)

### Commands

A `:` command line beside the browser: `:w <value>` writes the value to
the selected path, and an error from the command shows in the bottom
title.

```graphix
{{#include ../../examples/tui/browser_commands.gx}}
```

## See Also

- [list](list.md) - For simpler selection interfaces
- [table](table.md) - For tabular data display
- [block](block.md) - For containing browsers with borders
