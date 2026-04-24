# The Data Table Widget

A spreadsheet-style widget that renders a netidx `Table` as a grid of
live-subscribed cells. Columns are typed: each one picks the editor
(text, toggle, pick-list, spinner), visualization (progress bar,
sparkline), or action (button) that cells should use. Row sort, row
filter, column widths, and the selection set are all controlled from
graphix — the widget is the rendering surface and the subscription
manager, nothing more.

Rows that are absolute netidx paths trigger live subscriptions.
Non-absolute row names render as *virtual* rows whose cells draw from
each column's `default_value` ref instead of a subscription. Virtual
columns work the same way: a column that appears in `column_types`
but not in the `Table`'s `columns` array is a column whose values
come solely from its default. Virtual and live rows/columns freely
mix.

## Interface

```graphix
type SortDirection = [`Ascending, `Descending];
type SortBy = { column: string, direction: SortDirection };

type ColumnType = [
    `Text({ on_edit: [fn(#path: string, #value: Any) -> Any, null] }),
    `Toggle({ on_edit: [fn(#path: string, #value: bool) -> Any, null] }),
    `Combo({
        choices: Array<{ id: string, label: string }>,
        on_edit: [fn(#path: string, #value: string) -> Any, null]
    }),
    `Spin({
        min: f64,
        max: f64,
        increment: f64,
        on_edit: [fn(#path: string, #value: f64) -> Any, null]
    }),
    `Progress,
    `Button({
        on_click: [fn(#path: string, #value: Any) -> Any, null]
    }),
    `Sparkline({
        history_seconds: f64,
        min: [f64, null],
        max: [f64, null]
    })
];

type ColumnSpec = {
    typ: ColumnType,
    display_name: [string, null],
    default_value: &[null, string, Map<string, Any>],
    on_resize: &[fn(f64) -> Any, null],
    width: &[f64, null]
};

val data_table: fn(
    ?#sort_by: &Array<SortBy>,
    ?#column_types: &Map<string, ColumnSpec>,
    ?#selection: &Array<string>,
    ?#show_row_name: &bool,
    ?#on_select: [fn(#path: string) -> Any, null],
    ?#on_activate: [fn(#path: string) -> Any, null],
    ?#on_header_click: [fn(#column: string) -> Any, null],
    ?#on_update: [fn(#path: string, #value: Primitive) -> Any, null],
    #table: &sys::net::Table
) -> Widget;

val text_column: fn(
    ?#on_edit: [fn(#path: string, #value: Any) -> Any, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val toggle_column: fn(
    ?#on_edit: [fn(#path: string, #value: bool) -> Any, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val combo_column: fn(
    #choices: Array<{ id: string, label: string }>,
    ?#on_edit: [fn(#path: string, #value: string) -> Any, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val spin_column: fn(
    #min: f64,
    #max: f64,
    #increment: f64,
    ?#on_edit: [fn(#path: string, #value: f64) -> Any, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val progress_column: fn(
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val button_column: fn(
    ?#on_click: [fn(#path: string, #value: Any) -> Any, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec;

val sparkline_column: fn(
    #history_seconds: f64,
    ?#min: [f64, null],
    ?#max: [f64, null],
    ?#display_name: [string, null],
    ?#default_value: &[null, string, Map<string, Any>],
    ?#on_resize: &[fn(f64) -> Any, null],
    ?#width: &[f64, null]
) -> ColumnSpec
```

## `data_table` Parameters

- **`#table`** -- The table shape. `{ rows: Array<string>, columns:
  Array<string> }`. `sys::net::list_table(path)` produces this by
  inspecting the netidx resolver. The caller owns the shape: to
  filter, sort, hide, or reorder rows and columns, build the Table
  record in graphix and hand the result to `data_table`. Every change
  to this ref is reconciled against the current subscription set.

- **`#column_types`** -- Map from column name to `ColumnSpec`,
  built with the helper constructors (`text_column`, `toggle_column`,
  ...). A column that doesn't appear in `#table.columns` but is
  declared here becomes a *virtual* column driven entirely by its
  `default_value`. Updating this ref to change only display names,
  callbacks, widths, or defaults does not tear down row subscriptions
  — only adding or removing columns triggers resubscribes.

- **`#sort_by`** -- Array of sort keys applied in order. The first is
  the primary sort; later keys break ties. Empty list (the default)
  preserves the `Table`'s row order. Sort values come from live
  subscriptions to the named columns, so rows without a subscribable
  value for a given column sort to the end.

- **`#selection`** -- Controlled set of selected cell paths. For
  cells in the row-name column this is just `"row_path"`; for every
  other cell it is `"row_path/col_name"`. The widget's own click /
  keyboard handlers never mutate this ref directly — they fire
  `#on_select` and `#on_activate` callbacks that the caller uses to
  drive the ref however they want (single-select, multi-select,
  toggle, etc.).

- **`#show_row_name`** -- When `true` (the default) a synthesized
  leftmost column shows each row's basename (`Path::basename(row)`).
  Set `false` for tables where the row identity is already carried
  by a regular column.

- **`#on_select`** -- Fired whenever a cell is clicked or keyboard
  navigation lands on a cell. The callback receives the full cell
  path (`"row_path/col_name"` for data cells, `"row_path"` for the
  row-name column).

- **`#on_activate`** -- Fired when the user clicks a row-name cell
  or presses Enter while a row is selected. Receives the row path.

- **`#on_header_click`** -- Fired when the user clicks a data
  column's header label. Receives the column name.

- **`#on_update`** -- Fired once per subscription update on every
  cell — useful when you want to mirror live values into graphix
  state (e.g. re-derive an aggregate) without subscribing separately.
  Receives the cell path and new value.

## Column Types

Each `ColumnSpec` carries a `typ: ColumnType` picked with one of the
helper constructors:

- **`text_column`** -- Plain text. With `on_edit` the cell becomes
  editable: clicking a selected cell opens a text field; `Enter`
  commits the typed value via `on_edit`, `Escape` cancels. The
  widget attempts to parse the buffer as a typed graphix value; if
  parsing fails the raw string is sent instead, so users can enter
  `hello` without quotes.

- **`toggle_column`** -- Renders a toggler. Cell values `"true"` or
  `"1"` turn it on. With `on_edit` the user can flip the toggle.

- **`combo_column`** -- Drop-down with a fixed set of `choices`. Each
  choice has an `id` (the raw value published / sent back through
  `on_edit`) and a `label` (what the user sees).

- **`spin_column`** -- Numeric spinner with `min`, `max`, and
  `increment` bounds; `on_edit` receives the clamped new value.

- **`progress_column`** -- Read-only progress bar. Cell values are
  clamped to `[0, 1]`.

- **`button_column`** -- Each cell renders as a button labelled with
  the cell's current value. `on_click` fires with the cell path and
  current value.

- **`sparkline_column`** -- Rolling-line visualization accumulating
  published values over `history_seconds`. By default the y-axis is
  shared across every cell in the column (auto-scaled to the union of
  all rows' values) so cells are visually comparable. Pass `#min` /
  `#max` to fix the axis instead.

## Column Widths and Resizing

Widths are controlled by two refs per column:

- **`width: &[f64, null]`** -- When `Some(f64)` the column is pinned
  to that width (and the resize handle at the column header vanishes
  unless an `on_resize` callback is also set). When `null` the
  column auto-sizes to its content, with a per-column cap (default
  300px).

- **`on_resize: &[fn(f64) -> Any, null]`** -- Fired while the user
  drags a column header's right edge. The callable receives the new
  pixel width. The reference is a `&` field so the callable can be
  swapped, nulled, or initialized reactively.

Double-clicking any column's resize handle auto-fits *every* column
to the widest cell in the entire table (not just the visible window).

## Default Values and Virtual Cells

Each `ColumnSpec.default_value` is a `&[null, string, Map<string,
Any>]`. When a cell has no subscription value (either because the
row is virtual or because the subscription hasn't produced yet) the
widget renders the default:

- **`null`** -- Cell is blank.
- **`string`** -- Same default shown for every row in the column.
- **`Map<string, Any>`** -- Per-row defaults keyed by the row's
  basename. Rows without a matching key fall back to blank.

When the default ref updates reactively (e.g. the map changes), the
widget re-reads it and refreshes the affected cells. Sparkline
columns additionally push each new numeric default into the rolling
history, so a virtual-column sparkline fed from graphix state
accumulates points the same way a subscribed one does.

## Keyboard Navigation

The widget is focusable: clicking into it grants keyboard focus.
Arrow keys move the selection (the currently-rendered selected cell
scrolls into view as needed). `Enter` on a row-name cell fires
`on_activate`; `Space` on an editable cell opens its editor;
`Escape` cancels an in-progress edit.

## Examples

### Basic

Minimal usage: publish three hosts and hand the
`sys::net::list_table` output to `data_table` with no column
configuration. Cells display string values inferred from the netidx
types.

```graphix
{{#include ../../examples/gui/data_table_basic.gx}}
```

![Data Table — Basic](./media/data_table_basic.png)

### Filter and Sort

Row filtering (a regex over basenames) and sort configuration done
in graphix before `#table` and `#sort_by` are handed to the widget.
Shows how the caller owns the data pipeline end to end.

```graphix
{{#include ../../examples/gui/data_table_filter_sort.gx}}
```

![Data Table — Filter and Sort](./media/data_table_filter_sort.png)

### Editable

Every editable column type in one place: `Text`, `Toggle`, `Combo`,
`Spin`. Each cell is published with an `on_write` handler so edits
round-trip through netidx.

```graphix
{{#include ../../examples/gui/data_table_editable.gx}}
```

![Data Table — Editable](./media/data_table_editable.png)

### Calculated Columns

A virtual column whose `default_value` is a reactive
`Map<string, Any>`, rebuilt whenever any subscribed cell updates.
Demonstrates deriving per-row aggregates (here, sums) without
touching netidx.

```graphix
{{#include ../../examples/gui/data_table_calculated.gx}}
```

![Data Table — Calculated](./media/data_table_calculated.png)

### Sparkline

Live rolling charts per row with automatic column-wide y-axis
sharing, so cells are visually comparable without the caller picking
bounds.

```graphix
{{#include ../../examples/gui/data_table_sparkline.gx}}
```

![Data Table — Sparkline](./media/data_table_sparkline.png)

### Virtual Rows and Columns

Mix live subscribed rows with virtual rows whose cells come from
`default_value`. Virtual-column-plus-virtual-row combinations give
you fully client-side cells.

```graphix
{{#include ../../examples/gui/data_table_virtual.gx}}
```

![Data Table — Virtual](./media/data_table_virtual.png)

### Dashboard

Kitchen-sink example combining `Combo` state pickers, `Sparkline`
CPU metrics, `Progress` uptime, a `Button` action column, a regex
row filter in a text input, sort controlled by pick-lists, and
selection echoed in the footer.

```graphix
{{#include ../../examples/gui/data_table_dashboard.gx}}
```

![Data Table — Dashboard](./media/data_table_dashboard.gif)

## See Also

- [Table](table.md) -- static row/column layout from graphix
  widgets, no netidx subscriptions.
- [Chart](chart.md) -- larger-scale line / area / scatter plots.
- [`sys::net`](../../stdlib/sys/net.md) -- the `Table` record and
  `list_table` helper consumed by `#table`.
