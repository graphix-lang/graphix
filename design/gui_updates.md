# GUI update delivery

Status: built; tree walking is the accepted routing strategy.
Pins: `test::reconcile_test::a_thousand_widgets_receive_their_updates`,
`a_changed_result_rebuilds_the_root`,
`children_survive_a_failed_recompile_and_retry` in graphix-package-gui.

Each `ToGui::Update` is offered to every window. Each widget compares the
expression id with its own references and passes the update to its
children. A reference compiled while a widget is rebuilt joins this walk
without a separate owner registry. Root and pending-window updates are
handled before the per-window walk.

The delivery cost grows with updates times widgets times properties. The
accepted quick-profile measurement used one column of 1,000 text widgets,
each with its own reference to a shared variable. One write delivered
1,000 updates. The median walk over ten writes was 5.0 ms; the complete
write, including the runtime cycle, was 31 ms (12–57 ms). At 100 widgets
the walk was 80 microseconds. A few hundred updates over 1,000 widgets
therefore cost roughly 1.5 ms in that measurement.

An owner index would need to track every widget's references through
subtree replacement; an omitted reference would silently stop updating.
The measured cost is accepted in exchange for the simpler delivery rule.
The pin sends ten batches through 1,000 text widgets, checks every widget's
value after each batch, and checks a subsequent runtime write. It prints
walk timing without a timing assertion: scheduling and build profile must
not decide whether delivery is correct. A dev-profile run measured
12.36 ms per batch; that is not a replacement for the quick-profile result.

Reproduce the pin without a renderer or window:

```sh
cargo test -p graphix-package-gui --offline a_thousand_widgets_receive_their_updates -- --nocapture
```
