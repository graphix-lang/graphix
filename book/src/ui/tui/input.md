# Input Handling

The input handling system allows TUI applications to respond to keyboard, mouse,
and other terminal events. Input handlers wrap widgets and intercept events
before they propagate further.

## The Event Type

All terminal events are represented by the `Event` type:

```graphix
type Event = [
  `FocusGained,
  `FocusLost,
  `Key(KeyEvent),
  `Mouse(MouseEvent),
  `Paste(string),
  `Resize(i64, i64)
];
```

- `FocusGained` / `FocusLost`: Terminal window focus changes
- `Key`: Keyboard input
- `Mouse`: Mouse input (requires `tui::mouse <- true`)
- `Paste`: Bracketed paste content
- `Resize`: Terminal was resized to (columns, rows)

## Keyboard Events

Keyboard events contain detailed information about key presses:

```graphix
type KeyEvent = {
  code: KeyCode,
  kind: KeyEventKind,
  modifiers: Array<KeyModifier>,
  state: Array<KeyEventState>
};
```

### Key Codes

```graphix
type KeyCode = [
  `Backspace,
  `Enter,
  `Left,
  `Right,
  `Up,
  `Down,
  `Home,
  `End,
  `PageUp,
  `PageDown,
  `Tab,
  `BackTab,
  `Delete,
  `Insert,
  `F(i64),
  `Char(string),
  `Null,
  `Esc,
  `CapsLock,
  `ScrollLock,
  `NumLock,
  `PrintScreen,
  `Pause,
  `Menu,
  `KeypadBegin,
  `Media(MediaKeyCode),
  `Modifier(ModifierKeyCode)
];
```

Character keys use `Char(string)`, e.g., `Char("a")` or `Char("A")`.
Function keys use `F(i64)`, e.g., `F(1)` for F1.

### Key Event Kind

```graphix
type KeyEventKind = [
  `Press,
  `Repeat,
  `Release
];
```

Most applications only care about `Press` events.

### Key Modifiers

```graphix
type KeyModifier = [
  `Shift,
  `Control,
  `Alt,
  `Super,
  `Hyper,
  `Meta
];
```

The `modifiers` array contains all modifiers held during the key event.

## Mouse Events

Mouse events require enabling mouse support first:

```graphix
tui::mouse <- true
```

Mouse events contain position and action information:

```graphix
type MouseEvent = {
  column: i64,
  kind: MouseEventKind,
  modifiers: Array<KeyModifier>,
  row: i64
};

type MouseEventKind = [
  `Down(MouseButton),
  `Up(MouseButton),
  `Drag(MouseButton),
  `Moved,
  `ScrollDown,
  `ScrollUp,
  `ScrollLeft,
  `ScrollRight
];

type MouseButton = [
  `Left,
  `Right,
  `Middle
];
```

## Creating Input Handlers

Use `tui::input_handler::input_handler` to wrap a widget with event handling:

```graphix
val input_handler: fn(
  ?#enabled: &[bool, null],
  #handle: &fn(Event) -> [`Stop, `Continue] throws 'e,
  &Tui
) -> Tui throws 'e;
```

The handler function receives events and returns:
- `Stop`: The event was handled; don't propagate to other handlers
- `Continue`: Pass the event to the next handler

### Basic Example

```graphix
let count = 0;

tui::input_handler::input_handler(
  #handle: &|event| select event {
    `Key({ code: `Char("q"), .. }) => {
      // Exit on 'q'
      `Stop
    },
    e@ `Key({ code: `Up, .. }) => {
      count <- e ~ count + 1;
      `Stop
    },
    e@ `Key({ code: `Down, .. }) => {
      count <- e ~ count - 1;
      `Stop
    },
    _ => `Continue
  },
  &tui::paragraph::paragraph("[count]")
)
```

### Conditional Handling

The `#enabled` parameter allows dynamically enabling/disabling a handler:

```graphix
let editing = false;

tui::input_handler::input_handler(
  #enabled: &editing,
  #handle: &|event| select event {
    `Key({ code: `Esc, .. }) => {
      editing <- false;
      `Stop
    },
    `Key({ code: `Char(c), .. }) => {
      // Handle character input when editing
      `Stop
    },
    _ => `Continue
  },
  &tui::paragraph::paragraph("Edit mode")
)
```

## Event Propagation

Input handlers form a stack. Events flow from the outermost handler inward:

```graphix
tui::input_handler::input_handler(
  #handle: &|e| /* handler A */ `Continue,
  &tui::input_handler::input_handler(
    #handle: &|e| /* handler B */ `Continue,
    &widget
  )
)
```

If handler A returns `Continue`, the event reaches handler B. If either returns
`Stop`, propagation stops.

## The Raw Event Stream

For advanced use cases, you can access the raw event stream directly via
`tui::event`:

```graphix
select tui::event {
  `Key(k) => dbg(k),
  _ => ()
}
```

However, using `input_handler` is preferred as it integrates properly with the
widget tree and supports the enable/disable pattern.

## Common Patterns

### Vim-style Mode Switching

```graphix
type Mode = [`Normal, `Insert];
let mode: Mode = `Normal;

tui::input_handler::input_handler(
  #handle: &|event| select (mode, event) {
    (`Normal, `Key({ code: `Char("i"), .. })) => {
      mode <- `Insert;
      `Stop
    },
    (`Insert, `Key({ code: `Esc, .. })) => {
      mode <- `Normal;
      `Stop
    },
    (`Insert, `Key({ code: `Char(c), .. })) => {
      // Insert character
      `Stop
    },
    _ => `Continue
  },
  &content
)
```

### Focus Management

```graphix
let focused = 0;
let num_widgets = 3;

tui::input_handler::input_handler(
  #handle: &|event| select event {
    `Key({ code: `Tab, .. }) => {
      focused <- (focused + 1) % num_widgets;
      `Stop
    },
    `Key({ code: `BackTab, .. }) => {
      focused <- (focused - 1 + num_widgets) % num_widgets;
      `Stop
    },
    _ => `Continue
  },
  &widgets
)
```

## See Also

- [style](style.md) - Styling text and widgets
- [layout](layout.md) - Arranging widgets with focus support
