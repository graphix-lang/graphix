# Builtins

Status: current principle.
Pins: `stdlib/graphix-tests/src/lib_tests/`.

How a builtin is written and classified, and what an async call site does with its invocations.

**Builtins** implement `BuiltIn<R>` (`NAME`, `init()`, `EFFECT`) and
register with `ExecCtx::register_builtin::<T>()`; the Graphix signature
lives in the package's `.gx` with every argument and the return type
annotated. `EFFECT` is the one classification (`effects.rs`):
`Async` (may produce later, autonomously, or never), `Sync` (same cycle
but keeps cross-invocation state, or depends on WHICH args arrived),
`Stateless(Option<FastCall>)` (a pure function of its args, never an
effect; the payload is the direct-call entry the JIT uses, `Plain` or
`Typed` by the site's resolved return type; `None` for partial-delivery
producers and the cached data operations listed in `design/strict_fusion.md`), or `Io(Option<IoCall>)` (stateless, but it acts on the world
at each fire of its trigger argument: the printers, `exit`; an arm
holding one sleeps; `IoCall` is its result fast fn and trigger, and a
kernel queues each call for `BuiltIn::perform`, which the wrapper runs
after the run). A builtin call's facts also join those of every function
argument it is handed (it may call any back at run time, where no
analysis sees it): a definition's own, which its check records from
its body (`analysis::def_body_facts`, carried in its `DefTable`), else
async. A wrong `Stateless` is a semantics bug (the JIT's native
tail loop shares state across iterations); a wrong `Sync` is one too
where it skips a wake's recompute (`CachedArgs` recomputes only a
`Stateless` or `Io` value at a wake, through `EvalCached::quiet`, which
for an `Io` builtin computes the value without the effect), and
otherwise costs the loop. An async call site (`CachedArgsAsync`) runs its
invocations as its `EvalCachedAsync::CALLS` says: one at a time in
order (`Fifo`, the default), the latest only, a new one aborting those in
flight, though one that completed before the abort reached it still
answers, a cycle later if need be (`Latest`: `accept`, `wait`), or each at once, answered in
completion order (`Concurrent`: the tls handshakes); `delete` and `sleep`
abort every call in flight, except that a sleep lets the calls of an eval
not `CANCEL_SAFE` (a stream read or write, a cursor read, a handshake, a
file open or seek, a db or sqlite op, a spawn, a tcp connect or listen:
whatever stopping mid-way would lose) finish; nothing answers a call after
its site's sleep, so such a reply is dropped, or settled by
`EvalCachedAsync::reclaim` (a stream read's bytes return to the stream,
and later reads of it wait for that), and a `Fifo` site's next call waits
for such calls to finish. `sys::net::call` runs its calls side
by side: each answers `(RpcId, result)` once (reply, error or
`` `Aborted ``), in arrival order, one a cycle (its site's reply id
queues them). A call under an id in flight replaces that call, which
answers `` `Aborted ``; `abort_call` aborts one from anywhere (the table
is `NetState`'s, so `call`, `call_id` and `abort_call` are `ORDERED`); a sleep
abandons the site's calls and remints its reply id, so none answers.
Remote effects are not undone. Its completed result survives sleep, and
wake alone starts no call. `sys::net::callq` is `call` (the same
`RpcSite`) with no id in or out, answering in call order (a reorder
buffer of its own, dropped at sleep), and is not `ORDERED`. Bottom never reaches builtin authors: a bottomed arg bottoms the
invocation before `eval`. Line-reader handles re-delivered after EOF,
error, or a stop generation (seek/TLS upgrade) restart at the current
position; active readers ignore the same handle. Re-delivery never
rewinds. Queued lines at an unchanged position drain before the new
reader reads (`design/async_sleep_outputs.md`). Raw `Apply` authors read args through
`seam_arg`/`seam_tick`/`seam_value`. Configuration a fast fn derives
from its args (a regex, a template registry) lives in a bounded
thread-local `FastMemo`, never in state. A type-directed builtin
(`str::parse`, the reads) declares its target `'b: Concrete`; the checker
refuses the target where it settles open or holds a reference (data
can't be one; the run-time cast refuses one too) or an abstract type
with no `Cast` impl, `json::read`'s and `toml::read`'s target is also
`'b: JsonData`/`TomlData`, and so are the json and toml writers' values
(`Type::text_refusal`: every union in the type is one member beside
null, or members each carrying a tag, a variant's or an abstract type's
name (none inside the data a `from_value` reads, where abstract values are
bare), no two the same, an error no member; `Transparent<T>` carries none
and is refused over a `T` known not to be abstract, never over a
definition's own open variable; a typedef growing its parameters
as it recurses is refused), judged of the
call's settled binding (`site_bounds`; refused at the bind, a union
target would narrow to one member), a writer declares its value `'a: Data` (no
reference, function or such abstract type; open is fine), an async
builtin's `DATA_ARGS`/`DATA_RESULT` convert a store's keys and values
both ways (the arguments keep their kind markers, so keys differ and
order as values do), and the builtin's
`typecheck1` only extracts the type, never refuses
(`design/tvar_constraints.md`); a builtin that wraps a function
(`queuefn`) declares it `'a: Function` the same way. A builtin's quantifier that a
call's result and throws do not hold, and no argument the call passes or
omits can carry a value of (no function, no open part), settles to ⊥
whatever its bounds (`CallSite::settle_unread`): `publish`'s handler is
`[fn(v: 'a) -> _, null] = null` under `'a: Concrete`. A definition's call sites settle with
the enclosing statement, the signature's cells exempt.
