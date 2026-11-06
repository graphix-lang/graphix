# The standard library

Core and `opt` are in `SKILL.md`. Each package's `.gxi` is the full signature list.

`queuefn(#trigger, f)` returns a wrapper with `f`'s signature and a
counting semaphore starting at one permit. Later trigger fires release
waiting invocations or bank permits; unused permits accumulate, so later
calls can run without a new trigger. A wrapper site releases one call per
runtime cycle. `#count: &mut depth` observes queued invocations.

`array`: map filter filter_map fold flatten find find_map concat push
window(#n, arr, @v) len iter iterq sort enumerate zip (`window`'s second
argument is the array windowed, usually `t ~ data`, and its values after
it are variadic). `filter_map`'s callback returns `` [`Keep('b),
null] ``: `null` drops the element and `` `Keep(v) `` keeps `v`, a null
included; a bare `v` is refused. `|x| select x { null as _ => null, v =>
`Keep(v) }` over `[1, null, 2]` gives `[1, 2]`. `list`
(the same over `List<'a>`, plus `cons head tail uncons nth take drop
to_array from_array ..`), `map`, `str` (`contains split trim replace
join len parse ..`), `re`, `rand`, `db` (an embedded key-value store:
`open`, `tree`, `get`, `insert`, cursors, transactions, subscriptions),
`sqlite` (`open`, `exec`, `query`, `begin` /`commit`), `hbs` (`render`,
handlebars templates), `args` (command-line parsing: `positional`,
`option`, `flag`, `command`, `parse`), `xls` (`sheets`, `read`).

`sys::time`: `timer(timeout: [duration, Number], repeat)` (a finite
repeat count ends it; an arm's sleep pauses it), `after_idle(timeout, v)`,
`now(trigger)`, and the pure arithmetic `add`, `sub`, `add_dur`,
`sub_dur`, `diff`, `scale`. `sys::net`: `subscribe(path)`,
`publish(path, v)`, `rpc`, `callq`, `call`, `list`, `write`, every path a
`string`; an unselected arm tears a subscription or publication down and
abandons its rpc calls in flight. `sys::fs`, `sys::tcp`, `sys::tls`, `sys::process`
(`spawn`, `options`, `Proc`, `wait`), `http`. `json` and `toml`:
`read(string | bytes)` (async), `write_str` and `write_bytes` (pure);
`pack`: `read(bytes)` (async) and `write_bytes`. Pack and db values preserve
composite kinds (tuple, struct, variant, list, array and map), including
union members; abstract values still cross through `Cast`. Older unmarked
data remains readable but cannot recover a kind that was erased.

A sync variadic builtin called with no positional argument is a compile
error (`str::concat()`); it could never fire.

## Line reader retries

`sys::io::Lines::lines` and `lines_batched` stop at EOF or an error.
Re-delivering the same handle afterward, or after a seek/TLS upgrade,
starts reading at its current position. Seek explicitly before rereading
a file from the beginning; re-delivery does not rewind. A delivery while
the same reader is active leaves it running.

## RPC calls

`sys::net::callq(path, args)` is the usual call: its result is the
reply or an error, with no id. Each fire of an argument starts a call at
once, any number run side by side, and the answers come in the order the
calls were made, one a cycle (an early answer waits for the earlier
calls'). Nothing aborts a `callq` call; an arm's sleep drops its calls in
flight and the answers waiting behind them. A fan-out over `callq` can run
in parallel; one over `call` runs in order.

`sys::net::call(path, args)` is `(RpcId, result)`: take the pair, `let
(_, r) = sys::net::call(..)`, or its `.1`. Each fire of an argument
starts a call, and calls run side by side: each answers once, its reply,
an error or `` `Aborted ``, in the order the answers come, one a cycle,
tagged with its id. Name a call with `#id: id` (`let id =
sys::net::call_id(e)` mints a fresh id at each fire of `e`) and a new call
under that id replaces the one still in flight, which answers
`` `Aborted ``: one id for a search box keeps only the latest query.
`sys::net::abort_call(id)` aborts a call from anywhere. Aborting stops the
wait, not the server: its handler still runs. An arm's sleep abandons its
calls silently (no `` `Aborted ``); the last completed result survives,
and waking sends nothing. To observe a result, keep the call outside a
sleeping arm or in a seq step that waits for it.
