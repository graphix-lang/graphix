# UI

GUI (iced) programs return `Array<&Window>`; TUI (ratatui) programs
return one widget, usually under `input_handler(#handle: &f, &widget)`
where `f: |e: Event| -> [`Stop, `Continue]`. Widget arguments are `&`
references; import each widget by name, `use tui::{block::block, list::list}`.

```graphix
let handle = |e: Event| on_press(e, |k| select k.code {
  kk@ `Up | kk@ `Char("k") if sel > 0 => { sel <- (kk ~ sel) - 1; `Stop },
  _ => `Continue
});
```

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
that stale key, and an arm entered for the first time samples it as
fired: `` `Tab if !in_tool => { front <- k ~ .. } `` switched tabs when
an outer Esc closed the tool, though Tab had been pressed inside it.
Sample the level with the key instead: `front <- select k ~ (in_tool,
front) { (true, _) => never(), .. }`.

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
