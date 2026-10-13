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

```graphix
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

<!-- CR claude for claude: [doc-drift/medium] The signature below gives the handler and
the result `throws 'e`. The real `input_handler` in input_handler.gxi has no `throws`:
the handler runs from the display outside any `catch`, so it may not raise, and a
handler that uses `?` is refused with a type mismatch. Copy the signature from the gxi
(the positional is `child`), say that the handler must handle its errors locally (`$`,
or a `catch` in its body), and document `on_press`'s signature here, which does carry
`throws 'e`. probe: design/review-2026-10-12/repro/book-ui-a-handler-throws.gx
(book-ui-a-03) -->
Use `tui::input_handler::input_handler` to wrap a widget with event handling:

```graphix
val input_handler: fn(
  ?#enabled: &[bool, null],
  #handle: &fn(a: Event) -> [`Stop, `Continue] throws 'e,
  a: &Tui
) -> Tui throws 'e;
```

The handler function receives events and returns:
- `Stop`: The event was handled; don't propagate to other handlers
- `Continue`: Pass the event to the next handler

<!-- CR claude for claude: [doc-drift/medium] None of the code blocks from here down
compiles. The single-expression blocks `{ // comment `Stop }` (Basic, Conditional, Vim)
are refused ('a block must contain at least 2 expressions'). `paragraph("[count]")` and
`paragraph("Edit mode")` need `&".."`. The `/* handler A */` comments in Event
Propagation and the `_ => ()` arm in The Raw Event Stream are parse errors. Every
example also matches `` `Key({code: .., ..}) `` without checking the kind, which the Key
Event Kind section says runs every key twice. The propagation handlers return a constant
`` `Continue ``, which answers only the first event. Rewrite them in the
overlay_modal.gx style: `|e: Event| on_press(e, |k| select k.code { .. })`, bare ``
`Stop `` arms, `//` comments on their own line, `_ => never()`, and `e ~ `Continue`.
Moving them into book/src/examples/tui/ would put them under examples_compile. probe:
design/review-2026-10-12/repro/book-ui-a-input-basic-example.gx,
book-ui-a-raw-event-unit.gx (book-ui-a-02) -->
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

The `Up` and `Down` arms sample the event (`e ~ count + 1`). A write
inside a select arm fires when its right-hand side fires: `count <-
count + 1` would fire again on its own update and loop, and a constant
such as `mode <- \`Insert` fires once when the arm becomes selected
rather than on every key. Both are useful, and the select chapter's
"Writing From an Arm" spells out when.

### Conditional Handling

<!-- CR claude for claude: [doc-drift/low] A disabled `input_handler` drops each event:
it neither calls the handler nor passes the event to its child, so the whole wrapped
subtree stops receiving input. It works as a focus gate, not a pass-through. Say so here
and in a `///` doc on `enabled` in input_handler.gxi. The example below can never leave
its initial state: `editing` starts false and its only writer is inside the handler it
disables. Give it an outer handler that sets `editing`. (book-ui-a-11) -->
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

<!-- CR claude for claude: [doc-drift/high] The Tab and BackTab arms write `focused <-
(focused + 1) % num_widgets` without sampling the key. That is the self-loop this page
warns about under the Basic Example: the arm re-fires on its own write, so a single Tab
cycles `focused` forever at 100% CPU. Sample on the key, as overlay_modal.gx does (`` k@
`Tab => { focused <- ((k ~ focused) + 1) % num_widgets; `Stop } ``), and write the
handler with `on_press` (see book-ui-a-02). probe:
design/review-2026-10-12/repro/book-ui-a-focus-selfloop.gx (book-ui-a-01) -->
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

<!-- CR claude for claude: [doc-drift/medium] tui exports `line_edit` (a single-line
editor: `state`, `handle(&st, e)`, `view`, with a masked mode) and `form` (a modal
multi-field form with `on_submit`/`on_cancel`), and no page in the book mentions either.
A reader who needs text entry will hand-write Backspace/Left/Right/paste handling. Add a
'Text entry' section here, or a page per module, showing `line_edit::state`,
`handle(&st, e)` and `view(#focused, &st)`, and a `form(..)` on an overlay layer. List
both in SUMMARY.md and in the TUI overview's widget list. (book-ui-a-07) -->
## See Also

- [style](style.md) - Styling text and widgets
- [layout](layout.md) - Arranging widgets with focus support
