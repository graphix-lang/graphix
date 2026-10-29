# Async outputs across sleep

Status: built 2026-09-05; rewritten for "no partial rebirth" 2026-10-18
Pins: `stdlib/graphix-tests/src/lang/async_restart.rs`; `stdlib/graphix-tests/src/lib_tests/bottom.rs`

A wake is a resume, never a rebirth (`design/wake_catchup.md`, "No
partial rebirth"). What a builtin does across a sleep follows from what
its output is.

- **A call** (`CachedArgsAsync`: file, stream, process, http, db, sqlite
  operations): sleep aborts the call in flight (it cannot be
  paused) and the wake restarts nothing, since a restart would perform
  an action no trigger asked for; only an argument's fire, a caught-up
  one included, starts a call. The result is a level and survives the
  sleep. A call whose eval is not `EvalCachedAsync::CANCEL_SAFE` is not
  aborted: stopping it mid-way would lose what it already did (the bytes
  a read consumed, a partial write, a cursor's advance, the stream a
  handshake or a file operation holds, the work of a db or sqlite op on a
  blocking thread no abort stops, a spawned child, a connection or a
  bound listener). It finishes asleep and nothing answers it: the sleep
  counts a generation in the site's `kept` replies, and a reply of an
  older generation, kept or arriving later, is settled by
  `EvalCachedAsync::reclaim` (a stream read's bytes go back to the
  stream's unread buffer, and the stream's next read waits for every
  earlier read to settle, `ReadTicket`) or dropped. A program that needs
  the outcome calls outside the arm, or in a seq step. Calls not yet
  started are dropped either way.
- **An RPC call** (`RpcCall`) abandons its calls in flight at sleep:
  their requests are aborted (`NetState::abandon_calls`) and the site's
  reply ID remints, so none of them answers, not even `` `Aborted ``, at
  the wake or after it. A new call cancels nothing (calls run side by
  side; one under an id still in flight replaces that call, which answers
  `` `Aborted ``). This does not undo the remote handler's effects: a
  handler already running on the server can finish even though its
  caller stopped waiting. The last completed result survives sleep, and
  wake alone starts no call. Pins:
  `lib_tests::net::net_rpc_sleep_abandons_silently` in `graphix-tests`,
  `netstate::test::an_answer_settles_only_its_own_call` in
  `graphix-package-sys`.
- **An event source** (timers, `after_idle`, `throttle`'s pending
  emission, line readers, net and db subscriptions, watches, servers,
  `iter`/`range` deliveries): it pauses. A timer's run and a pending
  idle wait resume at the wake; a standing interest (a subscription, a
  publication, a listing, a watch, a served path) is detached at sleep
  and re-attached at the wake over the arguments as they stand; a
  `net::write` re-attaches to its path but writes only on a value fire;
  a delivery stream in flight is aborted. Its output is the last event,
  which does not survive the sleep: a select emits its arm's value at a
  re-entry, so a kept event would be re-raised as if it were new. The
  cleared output is bottom at the wake, so state built on the event reads
  bottom until the event fires again, though it kept its value:
  `count(timer)` in a woken arm shows nothing until the timer's next fire,
  then its kept count plus one. `hold` over the event, outside the arm,
  keeps showing the last one (pin `async_restart::woken_event_state_blanks_until_the_next_event`).
  A third state, absent without bottom, would show the state through the
  wake; it would be a new tag in the bottom algebra and the JIT's discs,
  which nothing has needed.
- **State** (the restart builtins, `queue`, `iterq`, `queuefn`,
  `filter`, `hold`):
  kept. A queue takes back what it sent through its reply variable and
  had not received (`InFlight`), and sends it again from the wake.

A builtin starts afresh only where it is born again: `Apply::reborn`,
which a call site runs before an update that follows its sleep under a
born view, a seq step entered for its next run or a call it issues.
`CachedArgsAsync` then holds no answer: its last one was the last run's.

## Audit

| Family | Sleep | Wake |
| --- | --- | --- |
| `CachedArgsAsync` (files, stream I/O, processes, HTTP requests, db, sqlite, other packages on the wrapper) | aborts the call in flight, remints the reply ID; keeps args and result; an eval not cancel-safe (stream reads and writes, cursor reads, tls handshakes, file open/seek/fstat/truncate, db and sqlite ops, process spawn, tcp connect/listen) finishes instead, unanswered, and a one-at-a-time site's next call waits for it; a read's bytes return to the stream | starts a call only on an argument fire |
| rpc `call` | abandons its calls in flight (aborts their requests, remints the reply ID), none answers; keeps the result; remote work may finish unanswered | starts a call only on an argument fire |
| `timer` | cancels the armed fire; keeps the run; clears its output | re-arms the run if fires remain |
| `after_idle` | cancels a pending wait; clears its output | resumes a pending wait |
| `throttle` | pauses the pending emission (its timer starts again at the next update); keeps the held value and the window the last emission opened; clears the output | — |
| `range`, `iter` | remints the delivery ID (the stream in flight is aborted); clears the output | — |
| `queue`, `iterq` | takes back sent-undelivered values; keeps contents and credits | sends again |
| `queuefn` | keeps the queue and its credit; a sleeping wrapper site's calls keep their place, released at its wake | — |
| `once`, `take`, `skip`, `uniq`, `hold`, `count`, `filter` | nothing | — |
| net subscribe, list/list_table, publish, publish_rpc, `write` | detaches; clears the output | re-attaches over standing args (`write` writes only on a value fire) |
| filesystem watch application and event/path accessors | rebuilds the watch or detaches listeners; clears the output | rebuilds over standing args |
| database subscription and event accessors | aborts the subscription or detaches listeners; clears the output | re-attaches |
| HTTP server | stops serving, aborts its connections and handler instances; clears the output | serves again |
| I/O line readers | clears the last line, preserving the reader and its delivery ID | reads on |
| netidx-admin ceremony events | detaches from the ceremony event stream (`paused`) | re-attaches |
| `sys::args`, `sys::dirs::*` | nothing | — (produce at birth only) |

## Line reader identity

`IoLines` retains its reader and its delivery ID across sleep. The
reader owns the stream position; a second reader would race it for
bytes. Lines wait in the reader's queue until the graph takes them, one
a cycle (`lines_batched`: one array per read), and the reader reads no
more until it has: a sleeping listener holds the reader back and loses
nothing. A partial
line stays in the stream's read half (`BufferedRead`), so an operation
on the whole stream that stops the reader (a TLS upgrade, a seek) reads
those bytes first. A reselected listener starts without an old output
and receives later lines from the same reader.

A handle re-delivered after EOF or an error starts a new reader at the
current position, under a fresh reply id. Complete lines already queued
at that position drain before the replacement reads more. A seek or TLS
upgrade changes the stream's stop generation; re-delivery replaces that
reader even if its task has not yet processed the stop notification,
and discards its old queue. A re-delivery while the same reader remains
active starts nothing. Neither EOF nor re-delivery rewinds a file.
Pins: `lib_tests::fs::file_handle::*lines_restart*`,
`io::test::a_stopped_reader_is_inactive_before_its_task_finishes`, and
`io::test::line_batches_wait_until_consumed`.

## Strict consumers and kernels

Resetting a builtin does not invalidate every enclosing expression's
resident by itself. A strict consumer must propagate a consumed bottom
before considering its cached result — `StaleBottom` included, which
invalidates the value channel without creating an event. With several
consumed inputs any bottom makes the result bottom, fresh only if at
least one consumed input triggered. The interpreter's shared strict gate
(`dense_gate!`), unary operators, casts, abstract constructors and field
projections follow this rule; a valid quiet input can refill a bottom
resident without firing, and a wake forces a quiet recomputation
without making a stale production fresh.

The kernel wrapper invokes generated code when a feeder is bottom or
its own resident is bottom. It cannot simply bottom the whole kernel: a
feeder read only by an untaken select arm must not affect the output.
Generated code propagates the tags along consumed paths and the wrapper
stores the resulting value and freshness — branch-local consumption
preserved, and the wrapper's quiet fast path cannot reuse an invalid
output.

None of this touches the history retained by `hold`, by bindings, or
by async operations that stay awake.
