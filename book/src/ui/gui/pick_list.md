# The Pick List Widgets

Graphix provides two dropdown selection widgets: `pick_list` (a standard dropdown) and `combo_box` (a searchable dropdown with type-to-filter).

## Pick List

A dropdown menu that lets the user select one option from a list of strings. Clicking the widget opens a dropdown; selecting an item closes it and triggers the callback.

### Interface

```graphix
val pick_list: fn(
  ?#selected: &[string, null],
  ?#on_select: fn(string) -> Any,
  ?#placeholder: &string,
  ?#width: &Length,
  ?#padding: &Padding,
  ?#disabled: &bool,
  &Array<string>
) -> Widget
```

### Parameters

- **`#selected`** -- Reference to the currently selected value, or `null` if nothing is selected. When `null`, the placeholder text is shown instead.
- **`#on_select`** -- Callback invoked when the user picks an item. Receives the selected string. Typically: `#on_select: |v| choice <- v`.
- **`#placeholder`** -- Text displayed when `#selected` is `null`. Gives the user a hint about what to choose.
- **`#width`** -- Width of the dropdown. Accepts `Length` values.
- **`#padding`** -- Interior padding around the displayed text. Accepts `Padding` values.
- **`#disabled`** -- When `true`, the dropdown cannot be opened. Defaults to `false`.
- **positional `&Array<string>`** -- Reference to the list of available options.

## Combo Box

A searchable dropdown that combines a text input with a dropdown list. As the user types, the options are filtered to match. This is useful when the list of options is long and the user needs to find a specific item quickly.

### Interface

```graphix
val combo_box: fn(
  ?#selected: &[string, null],
  ?#on_select: fn(string) -> Any,
  ?#placeholder: &string,
  ?#width: &Length,
  ?#disabled: &bool,
  &Array<string>
) -> Widget
```

### Parameters

- **`#selected`** -- Reference to the currently selected value, or `null` if nothing is selected.
- **`#on_select`** -- Callback invoked when the user selects an item from the filtered list. Receives the selected string.
- **`#placeholder`** -- Placeholder text shown in the input field when nothing is selected.
- **`#width`** -- Width of the widget. Accepts `Length` values.
- **`#disabled`** -- When `true`, the combo box cannot be interacted with. Defaults to `false`.
- **positional `&Array<string>`** -- Reference to the full list of options. The combo box filters this list as the user types.

Note: `combo_box` does not have a `#padding` parameter, unlike `pick_list`.

## Examples

### Pick List

```graphix
{{#include ../../examples/gui/pick_list.gx}}
```

![Pick List](./media/pick_list.png)

### Combo Box

```graphix
{{#include ../../examples/gui/combo_box.gx}}
```

![Combo Box](./media/combo_box.png)

## See Also

- [radio](radio.md) -- for inline single-select when there are few options
