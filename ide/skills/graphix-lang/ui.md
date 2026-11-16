# UI

GUI (iced) programs return `Array<&Window>`; TUI (ratatui) programs
return one widget, usually under `input_handler(#handle: &f, &widget)`
where `f: |e: Event| -> [`Stop, `Continue]`. Widget arguments are `&`
references; import each widget by name, `use tui::{block::block, list::list}`.

```graphix,fragment
let handle = |e: Event| on_press(e, |k| select k.code {
  kk@ `Up | kk@ `Char("k") if sel > 0 => { sel <- (kk ~ sel) - 1; `Stop },
  _ => `Continue
});
```

<!-- CR claude for claude: [doc-drift/low] The claim that a handler with a constant
answer "answers the first event and then never again ... and the keys after it are lost"
is wrong for the modal it is attached to. Run, `|e: Event| { on_press(e, keys); `Stop }`
behaves exactly like the `e ~ `Stop` form: its keys run (Esc closes the modal every
time), and an unanswered event goes nowhere, which is what a stopped one does. The
constant does cost keys when the answer lets events through: a bare `Continue passes no
event to the child, the first included. Say that the `e ~` matters for an answer that
lets events through, and that a modal stops them either way. input.md's Key Event Kind
section implies the same necessity in its parenthesis. probe:
design/review-2026-11-16/repro/examples-tui-constant-answer-handler.sh (examples-tui-03)
-->
<!-- CR claude for claude: [doc-drift/low] This paragraph says a handler whose result is
a constant "answers the first event and then never again". It answers none: the display
first calls it after the program's first cycle, so the constant never fires. An outer
handler `|e: Event| { n <- e ~ n + 1; `Continue }` runs for every key, but the inner
handler never sees one, the first included. book/src/ui/tui/input.md says it correctly
("it never fires, so the handler would answer no event"); make this paragraph say the
same. probe: design/review-2026-11-16/repro/book-ui-b-const-handler-answers-nothing.sh
(book-ui-b-03) -->
`on_press(e, f)` (in `tui::input_handler`) calls `f` with the `KeyEvent`
when `e` is a `` `Press `` and continues on everything else; it returns
`` [`Stop, `Continue] ``, so it is a handler's body or a part of one.
Never write the `` `Key(k) => select k.kind { `Press => .. } `` nest by
hand: forgetting the kind runs every key twice where the terminal
reports releases (Windows). A handler that also wants mouse, paste or
resize selects on the `Event` and calls `on_press` in its `` `Key `` arm;
a modal one that lets nothing through is `|e: Event| { on_press(e, keys);
e ~ `Stop }`. The `e ~` is not optional: a handler whose result is a
constant answers the first event and then never again (the `|v| k` rule),
and the keys after it are lost.

A guard over state other than the key is safe only when the handler sees
every key. An inner handler under one that stops some keys keeps its
last key standing, so a flip of the guarded level re-runs the select on
that stale key. The arm it enters reads the key stale and nothing in it
fires, but the select's own result fires with the guard, carrying what
the stale key computes. Sample the level with the key instead:
`front <- select k ~ (in_tool, front) { (true, _) => never(), .. }`.

Widget modules (each module's function has its name unless noted): gui —
`window` (a function in `gui`) text button text_input checkbox toggler
radio slider vertical_slider progress_bar pick_list combo_box column row
container scrollable stack grid space rule (`horizontal_rule`,
`vertical_rule`) tooltip canvas chart image markdown menu qr_code table
data_table mouse_area keyboard_area text_editor clipboard (`read_text`,
`write_text`, ..); tui — block paragraph text list table tabs gauge
line_gauge sparkline barchart (`bar_chart`) canvas chart calendar
browser scrollbar layout line_edit (`state`, `step`, `handle`, `view`)
form style input_handler overlay(#layers, base) + layer, `line` and
`span` (in `tui`), `tui::suspend(level)` (release the terminal to a
child process while true), `tui::exit`.
Layout: `` `Fill `Shrink `Fixed(f64) ``; padding `` `All(f) `Axis({x,
y}) `Each({top, right, bottom, left}) ``; colors `` `Red .. `Rgb(r, g,
b) ``.
