# Menus

The `menu` module provides menu bars and context menus. Both are built from the same `MenuItem` building blocks — actions (clickable items with optional keyboard shortcuts) and dividers.

## Interface

```graphix
{{#include ../../../../stdlib/graphix-package-gui/src/graphix/menu.gxi}}
```

## `menu::shortcut`

<!-- CR claude for claude: [doc-drift/low] This section says menu::shortcut fails only
for a key that is not one character. It also refuses a key with none of `#ctrl`, `#alt`,
`#logo` (menu.gx `shortcut`: "a shortcut needs Ctrl, Alt or Super"), so `shortcut("N")$`
or `shortcut(#shift: true, "N")$` written from this page silently gives an action with
no shortcut. The interface's doc comment was updated when the rule was added
(gui-widgets-b-01), but this prose was not. State the rule here and in the positional
`string` bullet. probe:
design/review-2026-10-26/repro/book-ui-a-shortcut-needs-modifier.gx (book-ui-a-01) -->
Creates a keyboard shortcut from modifier flags and a single character key; any one character, `é` or `€` included. Matching accepts the unmodified key or the character produced by Shift or AltGr, ignoring letter case. The modifier flags must still match: for example, use `#ctrl: true, #shift: true` with `"?"` for Ctrl+Shift+? on a US layout.

- **`#ctrl`** -- Hold Ctrl. Defaults to `false`.
- **`#shift`** -- Hold Shift. Defaults to `false`.
- **`#alt`** -- Hold Alt. Defaults to `false`.
- **`#logo`** -- Hold the logo/super key. Defaults to `false`.
- **positional `string`** -- A single character (e.g. `"N"`, `"Z"`). Returns an error if the key is not exactly one character.

The shortcut text (e.g. "Ctrl+N") is displayed right-aligned in dimmed text next to the menu item label. In a menu bar, pressing the key combination triggers the action anywhere in the window. In a context menu it triggers the action while that menu is open: several rows may carry the same context menu, and a closed one has no row to act on.

## `menu::action` Parameters

- **`#on_click`** -- Callback invoked when the action is clicked. Receives `null`. If omitted, the action is displayed but does nothing.
- **`#shortcut`** -- A `Shortcut` value created by `menu::shortcut(...)`. The shortcut text is shown right-aligned in the menu and the key combination triggers the action. `null` for no shortcut.
- **`#disabled`** -- When `true`, the action is grayed out and `#on_click` is not triggered. Defaults to `false`.
- **positional `&string`** -- The label text for this menu action.

## `menu::divider`

Takes no arguments. Returns a horizontal separator line between menu items.

## `menu::menu`

Groups a list of menu items under a label that appears in the menu bar.

- **positional `&string`** -- The label shown in the menu bar (e.g. `"File"`, `"Edit"`).
- **positional `&Array<MenuItem>`** -- The items in this dropdown menu.

## `menu::bar` Parameters

- **`#width`** -- Width of the menu bar. Accepts `Length` values. Defaults to `` `Shrink ``.
- **positional `&Array<MenuGroup>`** -- The menu groups to display in the bar.

## `menu::context_menu`

Wraps any widget and shows a dropdown menu on right-click. Reuses the same `MenuItem` type as the menu bar.

- **positional `&Array<MenuItem>`** -- The items to display in the context menu.
- **positional `&Widget`** -- The child widget. Right-clicking anywhere on this widget opens the menu at the cursor position.

The menu closes when an item is clicked, when the user clicks outside it, or when Escape is pressed.

## Examples

### Menu Bar

```graphix
{{#include ../../examples/gui/menu_bar.gx}}
```

![Menu Bar](./media/menu_bar.png)

### Context Menu

```graphix
{{#include ../../examples/gui/context_menu.gx}}
```

![Context Menu](./media/context_menu.png)

## See Also

- [button](button.md) -- for standalone clickable actions
- [keyboard_area](keyboard_area.md) -- for capturing keyboard shortcuts
- [mouse_area](mouse_area.md) -- for general mouse event handling
