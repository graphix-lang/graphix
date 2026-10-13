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

- `FocusGained` / `FocusLost`: Terminal window focus changes (enabled while the display runs, independently of mouse input)
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

Most applications only care about `Press` events, and must say so: a
terminal that reports releases (the Windows console does) delivers two
events per keystroke, and a handler that matches `` `Key(k) `` alone
runs twice. `on_press(e, f)` calls `f` with the `KeyEvent` when `e` is a
press and continues on everything else. It returns what a handler
returns, so it is usually the handler's whole body:

```graphix
let handle = |e: Event| on_press(e, |k| select k.code {
  `Up => { sel <- k ~ max(0, sel - 1); `Stop },
  `Down => { sel <- k ~ sel + 1; `Stop },
  _ => `Continue
});
input_handler(#handle: &handle, &view)
```

A handler that wants mouse, paste, resize or a release selects on the
`Event` itself. A modal one that lets nothing through wraps the call
and answers every event, which means sampling the answer on the event
(a bare `` `Stop `` is a constant: it answers the first event and no
other):

```graphix
let modal = |e: Event| { on_press(e, keys); e ~ `Stop };
```

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

## Ending the Program

A TUI program runs until Ctrl-C, or until it calls `tui::exit`, which
ends it the same way: the terminal is restored and the runtime stops.
Fire it from the key that quits, sampled with the event:

```graphix,ignore
`Char("q") => { tui::exit(k ~ null); `Stop }
```

## Handing the Terminal to a Program

Some steps belong on the plain terminal — a `sudo` password prompt, an
editor. `tui::suspend` takes a level: while its input is true the
display has released the terminal (leaving the alternate screen and
raw mode, and dropping its input reader), and when the input goes
false the display takes it back with a full repaint. Its result is the
display's actual state, so start the program on the release and clear
the level with the program's exit:

```graphix
let suspended = false;
{
  catch(e) { println("could not run: [e]"); suspended <- e ~ false };
  suspended <- kk ~ true;
  let released = tui::suspend(suspended)?;
  let go = select released { true => released, false => never() };
  let child = sys::process::spawn(sys::process::options(#args: ["-v"], go ~ "sudo"))?;
  let status = sys::process::wait(child.proc)?;
  suspended <- status ~ false
}
```

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
{{#include ../../../../stdlib/graphix-package-tui/src/graphix/input_handler.gxi:handler}}
```

The handler function receives events and returns:
- `Stop`: The event was handled; don't propagate to other handlers
- `Continue`: Pass the event to the next handler

The handler may not raise an error: it is called from the display,
outside any `catch`, so handle errors in its body (`$`, or a `catch`
inside it). `on_press` is the usual body, and it may throw what its
callback throws.

### Basic Example

```graphix
{{#include ../../examples/tui/input_basic.gx}}
```

The `Up` and `Down` arms sample the key (`u ~ count + 1`). A write
inside a select arm fires when its right-hand side fires: `count <-
count + 1` would fire again on its own update and loop, and a constant
such as `mode <- \`Insert` fires once when the arm becomes selected
rather than on every key. Both are useful, and the select chapter's
"Writing From an Arm" spells out when.

### Conditional Handling

The `#enabled` parameter turns a handler on and off. A disabled handler
drops every event: neither it nor anything it wraps, nested handlers
included, sees one. So it gates a whole subtree, and whatever enables it
must sit outside it:

```graphix
{{#include ../../examples/tui/input_conditional.gx}}
```

## Event Propagation

Input handlers form a stack. Events flow from the outermost handler inward:

```graphix
{{#include ../../examples/tui/input_propagation.gx}}
```

If the outer handler returns `Continue`, the event reaches the inner one. If
either returns `Stop`, propagation stops.

## The Raw Event Stream

For advanced use cases, you can access the raw event stream directly via
`tui::event`:

```graphix
{{#include ../../examples/tui/input_raw.gx}}
```

However, using `input_handler` is preferred as it integrates properly with the
widget tree and supports the enable/disable pattern.

## Text Entry

`tui::line_edit` is a single-line editor: `line_edit::state(initial)`
makes its state, `line_edit::handle(&mut st, e)` applies a key (typing,
paste, Backspace, Delete, Left, Right, Home, End) and answers `Stop` or
`Continue` like a handler, and `line_edit::view(&st)` renders it as a
`Line` with the cursor shown (`#mask` hides a secret):

```graphix
{{#include ../../examples/tui/input_line_edit.gx}}
```

`tui::form::form` is a modal form of several line editors: Tab and Down
move to the next field, BackTab and Up to the previous, Enter calls
`#on_submit` with every field's text, Esc calls `#on_cancel`. Render its
`view` in an `input_handler` running its `handle`, on an overlay layer:

```graphix
{{#include ../../examples/tui/input_form.gx}}
```

## Common Patterns

### Vim-style Mode Switching

```graphix
{{#include ../../examples/tui/input_vim.gx}}
```

### Focus Management

```graphix
{{#include ../../examples/tui/input_focus.gx}}
```

## See Also

- [style](style.md) - Styling text and widgets
- [layout](layout.md) - Arranging widgets with focus support
