# The debugger

Status: designed 2026-11-27, **not built**. Eric's rulings of that day
(D63 in the CR discussion file) decided the shape; the open questions at
the end are his. CR `a-language-08`.

## 1. Why

A Graphix program's usual failure is that nothing happens. Where
something failed the program now says so (`debugging.md`: a failed
operator, an error a `$` dropped). Where nothing failed there is nothing
to log: an arm is asleep, a constant stands, a seq waits on a step, an
input never arrived. The author needs to ask the running program
questions.

A flag on the command line cannot do it. A TUI or a GUI owns its
terminal, and a long-running program has to be attached to after the
fact. So the debugger is a service inside the program, reached over a
byte stream from a second terminal.

## 2. What it is

**A REPL attached to the running program, plus debugger commands.**

The runtime handle already compiles an expression against the live
program (`GXHandle::compile`, what the REPL uses), writes a binding
(`set`, how the fuzzer injects events), reads one by name
(`compile_ref_by_name`) and runs a closure on the runtime thread with the
whole state (`with_ctx`). So "what is `x`", "set `x`", and adding a
`println` to a live program are existing machinery behind a socket. The
new work is the commands that need the runtime's inside view.

Three layers:

| Layer | Where | What |
|---|---|---|
| Hooks | `graphix-rt` | the pause gate in the run loop, the watched set, the cycle log; typed requests answered on the runtime thread between cycles. No I/O, no JSON |
| Service | a new library crate, `graphix-debug` | the wire protocol, the server over any byte stream, and the client |
| CLI | `graphix-shell` | `graphix --debug` to offer a socket, and the `graphix debug` subcommand to attach. No new binary |

### The embedder's API

An embedder turns it on and brings its own transport:

```rust
let debugger = graphix_debug::Debugger::new(handle.clone());
// one connection over anything that reads and writes bytes
debugger.serve(stream).await?;
// or every connection a listener the embedder made accepts
debugger.listen_unix(unix_listener).await?;      // cfg(unix)
debugger.listen_pipe(first_pipe, next_pipe).await?; // cfg(windows)
```

The embedder creates the socket or the pipe, so its path, its
permissions and who may connect are its own business. `serve` over a
plain stream covers the rest: TCP under TLS, stdio, an in-memory pair in
a test. Nothing listens unless the embedder calls one of these, and with
no session attached the hooks cost one branch a cycle.

`GXConfig` gains one field, `paused: bool`: the runtime compiles its
program and waits for a debugger's `continue` before its first cycle, for
a program whose start is what needs debugging.

### The shell

- `graphix --debug[=PATH] prog.gx` offers a socket and prints its path on
  stderr. With no path it is `$XDG_RUNTIME_DIR/graphix/debug-<pid>.sock`,
  in a directory of mode 0700, the socket 0600. On Windows it is a named
  pipe, `\\.\pipe\graphix-debug-<pid>`, behind the same interface.
- `graphix --debug-wait prog.gx` also starts paused.
- `graphix debug [PATH | PID]` attaches: a line editor with history and
  completion, as the REPL has. With no argument it lists the sockets in
  the runtime directory. `graphix debug PATH -c 'why x'` runs commands
  and exits, for scripts and tests.

## 3. The protocol

Lines of JSON, ours. A request is `{"id": n, "cmd": "..", ..}`, a reply
`{"id": n, "ok": ..}` or `{"id": n, "err": ".."}`, and an event (a
watched binding fired, the runtime paused) is a line with no `id`. The
first line each way is a hello carrying a protocol version.

A value crosses as Graphix text, the canonical printed form, in both
directions: the printer and the parser are the codec, so there is no
second encoding to keep in step with the language.

DAP is not the native protocol. It is built around threads, stack frames
and stepping by line, which fit a dataflow program badly. An adapter can
translate later (§5, phase 3).

## 4. Naming what you ask about

- **A binding by name**, where the name is in scope at the top level or
  in a module: `x`, `m::x`.
- **A binding by position**, `file:line[:col]`, for one inside a function
  or a block. The environment records where every binding was introduced.
- **An instance.** A binding inside a function exists once for each call
  site that built an instance. A position with several is answered with
  the list (each by its call path), and a qualifier picks one.
- **A node by position**, for a select or a seq.
- **A fused block** is pure code and is treated as a box (Eric,
  2026-11-27): the debugger shows what goes in (each input's tag and
  value) and what comes out, and inputs can be injected. A binding
  computed inside the block has no stored value; asking for it names the
  block and shows its inputs and output. Nothing more is needed, and a
  session does not turn fusion off.

## 5. Commands

**Phase 1: ask and inject.**

| Command | What it does |
|---|---|
| `eval <expr>` | the attached REPL: compile against the live program and print as the REPL prints. Any Graphix: a read, a `<-`, a `println`. What it compiled is deleted when the session ends |
| `print <target>` | value or "no value", its type, the cycle it was last written |
| `list [scope \| file]` | bindings and where they are |
| `set <target> = <expr>` | inject: deliver a value to a binding as an embedder's write does, in the next cycle |
| `watch <target>` / `unwatch` | an event at each fire: cycle, tag, value |
| `why <target>` | why it has no value, or has not fired (§6) |
| `info` | the cycle number, what is fused, what is pending |

**Phase 2: control, and reasons inside a node.**

| Command | What it does |
|---|---|
| `pause` / `continue` / `step [n]` | stop between cycles, run one or `n` |
| `break <target> [fires \| is bottom \| == v]` | pause at the end of the cycle in which it happens |
| `cycles [n]` | what the last `n` cycles delivered and what fired (a ring kept while a session is attached) |
| `why`, deeper | which arm a select holds and why the others sleep, the step a seq is at and what it waits for, a call with no answer yet, a connect or a sample that has never fired, a fresh block not yet born |

**Phase 3: editors.** The language server already resolves the cursor to
a binding. With a session attached, hover shows the live value and `why`
is a code action. Then a DAP adapter for the editors' debug panes, and a
TUI client written in Graphix.

## 6. `why`

Spreadsheets answer the same question with "trace error": from a blank
cell, walk what it reads back to where the blank starts.

At the binding level the walk needs nothing new: the store says which
bindings have a value and when each was last written, and each binding's
node says what it reads. From the target, follow the reads that have no
value until reaching a binding that has all its inputs and still no
value: that is where the nothing starts. For the review's probe:

```text
r        no value. It is the seq at line 15
  step 1 `let v = a[7]$` has no value, and reads `a`, which has one
  `a[7]` is `ArrayIndexError("array index out of bounds")`, dropped by `$`
```

The last two lines need the node to describe itself: an `explain` method
on `Update` (none by default) that a select, a seq machine, a call site,
a connect, a sample, a `$` and a fresh block implement. That is phase 2;
phase 1 stops at the binding and prints its expression, which for
`a[7]$` or `total / n` is usually enough. The loss without the walk is
the review's item 5 itself.

## 7. Inside the runtime

- **Requests** run on the runtime thread between cycles, as closures over
  the runtime's state (`ToGX::WithCtx` is the existing bridge). Nothing
  is read while a cycle or its forks run.
- **The pause gate** is in `GX::run`: a paused runtime keeps draining
  its sources and queues what arrives, and starts no cycle. Timers and
  replies still arrive while it is paused, so stepping changes timing;
  that is the nature of pausing a reactive program.
- **Watches and breaks** are a set of binding ids checked against what a
  cycle wrote, after the cycle.
- **Instances built later** join a watch by position: a binding
  introduced at a watched position is added when it is bound.
- **Session images, fusion, parallel evaluation and walk pruning** are
  untouched. A debugger that changes what a program computes is a bug
  (§9).

## 8. Security

The service can read every value, secrets included, run any Graphix in
the program and inject events. It is off by default. The shell's socket
is the owner's alone. An embedder decides for itself, which is why it
hands over the listener. The protocol has no authentication of its own:
that is the transport's.

## 9. Testing

- Every command through `serve` over an in-memory stream, no sockets.
- `why` fixtures, one for each cause, and the review's probe.
- Pause and step: a stepped run delivers what a free run does.
- The fuzzer: a program run with a session attached and watching
  everything gives the trace it gives alone.

## 10. What it does not do

No stepping inside a cycle. No view inside a fused block. No time
travel: the cycle log says what fired, and cannot be replayed.

## 11. Open questions

1. **The crate.** A new `graphix-debug` library (protocol, server,
   client), so an embedder depends on it and the runtime crate holds only
   the hooks. The alternative is to put the service in `graphix-rt`.
2. **`eval` can do anything**: write a binding, run an effect, load a
   module, in the live program. That is the point of an attached REPL;
   confirm it is wanted, or say what a session may not do.
3. **`set` on any binding**, or only on one something already writes? A
   `let x = a + b` that is set holds the injected value until `a` or `b`
   fires again.
4. **The flag's name and the socket's place**: `--debug[=PATH]`,
   `$XDG_RUNTIME_DIR/graphix/`.
