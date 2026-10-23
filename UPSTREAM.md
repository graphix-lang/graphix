# Upstream bugs

Bugs found in crates Graphix depends on, for fixing upstream. Graphix
works around one only when it breaks Graphix's own operation; each entry
says whether it does and where. When a fix lands upstream, bump the
dependency, delete the workaround and its entry.

Each entry: the crate and version, what is wrong, how to reproduce it,
the state of a fix, and Graphix's side.

## cranelift-jit 0.137 / region 3.0.2: the arena commits its whole reservation on Windows

- **Bug.** `ArenaMemoryProvider::new_with_size` reserves through
  `region::alloc`, which on Windows calls `VirtualAlloc` with
  `MEM_COMMIT | MEM_RESERVE` (region `src/os/windows.rs`). Each arena
  charges its full size against the commit limit (RAM plus page file),
  where on Linux it is address space only. cranelift-jit's own comment
  says so and points at region-rs PR #34.
- **Repro.** On Windows 11, 64 `ArenaMemoryProvider::new_with_size(256 <<
  20)` raise the process's `PrivateUsage` by 16,416 MB; the same 64 of
  Graphix's arena raise it by 0 MB. Where the page file cannot grow, the
  reservations fail.
- **Upstream fix.** region's Windows `alloc` reserving only (region-rs
  PR #34), or cranelift-jit's arena reserving with `MEM_RESERVE` and
  committing each segment as it allocates.
- **Graphix's side.** Worked around (a few fusing runtimes exhaust the
  page file and run unfused): `fusion/emit/arena.rs` is Graphix's own
  `JITMemoryProvider`, reserving the arena and committing a page run per
  allocation on every host. Not reported.

## calamine 0.34: the ods reader is dense over a sheet's used area

- **Bug.** calamine reads every ods table when the workbook opens, as a
  dense range over the area its cells span with repeats expanded. A few KB
  of `content.xml` whose rows end in a cell far to the right, or a cell at
  A1 plus one far corner, allocate tens of millions of cells (32.7M from a
  2.6 KB file). xlsx and xlsb have sparse cell readers; ods has none.
- **Repro.** `stdlib/graphix-package-xls/src/lib.rs` tests
  `ods_refuses_many_far_rows`, `ods_refuses_a_far_corner`,
  `ods_refuses_far_rows_valued_by_any_attribute` build such files.
- **Upstream fix.** A sparse ods cells reader like xlsx's
  `worksheet_cells_reader`, or a caller-set cell limit at open.
- **Graphix's side.** Worked around (a crafted file OOM-kills the
  runtime): `check_ods` streams `content.xml` before calamine opens it and
  refuses past `MAX_CELLS`, mirroring by hand which attributes calamine
  reads a value from (`ODS_VALUE_ATTRIBUTES`), so it must follow
  calamine's parser. Not reported.

## handlebars-rust 6.4: template depth recurses without a bound

- **Bug.** Parsing, cloning, rendering and dropping a template recurse
  once per nesting level, and an `{{else if ..}}` chain nests one level
  per link (`HelperTemplate::insert_inverse_node`, template.rs). A
  template of a few thousand nested blocks or chain links overflows the
  stack and aborts the process.
- **Repro.** `stdlib/graphix-tests/src/lib_tests/hbs.rs`
  `hbs_block_nesting_refused`: `{{#if a}}x` + 5000 × `{{else if a}}y` +
  `{{/if}}`.
- **Upstream fix.** An iterative template walk, or a depth limit the
  registry enforces at parse with an error.
- **Graphix's side.** Worked around (a template from data aborts the
  process): `shallow` in `stdlib/graphix-package-hbs/src/lib.rs` scans the
  source and refuses nesting past a bound, which tracks the handlebars
  grammar by hand (else-chain links, comments, raw blocks, string
  literals). Not reported.

## sled 0.34.7: a dropped reservation never wakes its subscriber

- **Bug.** A write that changes nothing drops its `ReservedBroadcast`
  without waking the subscribers it reserved, and `Subscriber::
  next_timeout` saves the dropped oneshot back, so it answers
  `Disconnected` on every later call. A watched tree then wedges writers
  or loses events.
- **Upstream fix.** Prepared, not submitted: `~/proj/sled`, branch
  `subscriber-dropped-reservations` (df42b243 on v0.34.7): a dropped
  reservation wakes its subscribers, `next_timeout` skips one and reports
  a timeout as `Timeout`, two tests. sled's main is the 1.0 rewrite with
  no subscribers, so the PR targets a 0.34 branch (`origin/tyler_0.34.7`
  is v0.34.7).
- **Graphix's side.** No longer reached: the db package keeps its own
  watches (`stdlib/graphix-package-db/src/watch.rs`) and uses no sled
  subscriber.

## sled 0.34.7: collections are created outside transactions

- **Bug.** A tree opened inside a transaction is created at once, not at
  commit, so a rollback or a conflict leaves it behind.
- **Graphix's side.** Worked around (a rolled-back transaction left a
  tree without types): the db package creates such a tree at commit
  under its own creation lock and drops it when the commit fails
  (`stdlib/graphix-package-db/src/txn.rs`). Not reported.

## sled 0.34.7: import into a database holding only a tree's meta entry panics

- **Bug.** `Db::import` panics on that shape and leaves the database
  partly imported.
- **Graphix's side.** Worked around: the db package validates an import
  and inserts its entries atomically itself
  (`stdlib/graphix-package-db/src/tree.rs`). Not reported; a standalone
  repro is still to be written.

## iced_widget 0.14: rich text does not redraw when the hovered link changes

- **Bug.** `iced_widget::text::rich::update` requests no redraw when the
  hovered link index changes, so link underlines flicker between
  adjacent link spans.
- **Graphix's side.** Not worked around (cosmetic); noted in
  `stdlib/graphix-package-gui/src/widgets/markdown.rs`. Not reported.

## netidx (ours): a stream replay reaches every channel of a subscription

- **Bug.** `subscriber/connection.rs::handle_connect_stream` pushes a
  joining channel's `BEGIN_WITH_LAST` replay into the stream batch, and
  `process_updates_batch` broadcasts it to every channel on the
  subscription: existing readers fire again, and several registrations
  in one batch replay duplicates.
- **Upstream fix.** Target the replay at the joining channel, in stream
  order.
- **Graphix's side.** `sys::net` keeps one shared channel per
  subscription and replays `last` itself, which is racy: a late second
  reader of a busy path can see a value twice or out of order (Eric's
  open CRs sys-net-01 and x-concurrency-07 in
  `stdlib/graphix-package-sys/src/netstate.rs`).

## iced_core 0.14: flex layout sums FillPortion weights in a u16

- **Bug.** `iced_core::layout::flex` adds a row's or column's
  `FillPortion` weights in a `u16`: two children at `FillPortion(40000)`
  overflow it, panicking a debug build, and in release the wrapped sum
  can be 0, which the fill share then divides by.
- **Graphix's side.** Worked around: `LengthV` reads a weight only from
  1 to 255 (`stdlib/graphix-package-gui/src/types.rs`), so up to 257
  siblings cannot overflow the sum. Not reported.
