# The debugger

Status: **phase 1 is built** (ask and inject); phases 2 and 3 are designed
here and not built. Eric's rulings of 2026-11-27 decided the shape (D63
and D64 in the CR discussion file). CR `a-language-08` tracks what is left.

## 1. Why

A Graphix program's usual failure is that nothing happens. Where
something failed the program says so (`debugging.md`: a failed operator,
an error a `$` dropped). Where nothing failed there is nothing to log: an
arm is asleep, a constant stands, a seq waits on a step, an input never
arrived. The author needs to ask the running program questions.

A flag on the command line cannot do it. A TUI or a GUI owns its
terminal, and a long-running program has to be attached to after the
fact. So the debugger is a service inside the program, reached over a
byte stream from a second terminal.

## 2. What it is

**A REPL attached to the running program, plus debugger commands.**

| Layer | Where | What |
|---|---|---|
| Seek | `graphix-compiler/src/seek.rs` | what gives a binding its value, and the fused blocks that compute a place, both riding `Update::refs`, the one walk that reaches every node |
| Hooks | `graphix-rt/src/gx/debug.rs` | the sessions attached, their watches, and the questions only the runtime's thread can answer. Typed data, no I/O |
| Service | `graphix-rt/src/debug/` | the wire protocol (`proto.rs`), the server over any byte stream (`mod.rs`), the client (`client.rs`) |
| CLI | `graphix-shell/src/debug.rs` | `graphix --debug` offers a socket, the `graphix debug` subcommand attaches. No new binary |

The service is a module of `graphix-rt`, not a crate of its own (Eric:
the runtime crate is small).

### The embedder's API

An embedder turns it on and brings its own transport:

```rust
use graphix_rt::debug::Debugger;

let debugger = Debugger::new(handle.clone());
// one session over anything that reads and writes bytes
debugger.serve(stream).await?;
// or a session for every connection a listener the embedder made accepts
debugger.listen_unix(unix_listener).await?;         // cfg(unix)
debugger.listen_pipe(first_instance, make_next).await?; // cfg(windows)
```

The embedder creates the socket or the pipe, so its path, its
permissions and who may connect are its own business. `serve` over a
plain stream covers the rest: TCP under TLS, stdio, an in-memory pair in
a test. Nothing listens unless the embedder calls one of these, and with
no session attached a cycle pays one branch. `with_types` sets how a
session prints a type (the shell passes the REPL's printer).

`graphix_rt::debug::Client` speaks the protocol from the other end.

### The shell

- `graphix --debug[=PATH] prog.gx` offers a unix socket and prints
  `debugger: graphix debug PATH` on stderr. With no path it is
  `$XDG_RUNTIME_DIR/graphix/debug-<pid>.sock`: the directory is made mode
  0700 and the socket 0600. The socket is removed when the program ends.
  A path that is a live socket, or no socket, is refused.
- `graphix debug [PATH | PID] [-c CMD]..` attaches. With no target it
  attaches to the one program that offers a debugger and lists them when
  there are several. `-c` runs a command and exits (several may be
  given), as lines piped to stdin do; the exit status is 1 if any was
  refused. At a terminal it reads lines with a line editor, and what a
  watch or a shown value reports prints above the prompt.
- A line that starts with a command's name is that command. Any other
  line is Graphix and is evaluated; `eval` says so explicitly.

On Windows the same interface is a named pipe,
`\\.\pipe\graphix-debug-<pid>`. **That path compiles for
`x86_64-pc-windows-gnu` and has never been run** (CR `debugger-01`).

## 3. The protocol

Lines of JSON (`proto.rs`). Each side's first line is a hello,
`{"graphix_debug": 1}`. A request is `{"id": n, "cmd": "..", ..}`, its
answer `{"id": n, "ok": ..}` or `{"id": n, "err": ".."}`, and an event (a
watched binding fired, a shown value changed) is a line with `"event"`
and no `id`.

A value crosses as Graphix text, as the REPL prints it under its type, so
the printer and the parser are the codec.

A binding's id crosses as a short handle (`88`, or `r4250` for one a
session image reserved), never the 62-bit number: a person types it, and
a JSON number that large does not survive every client.

An answer to `eval` or `set` is sent only after the cycle it caused has
run (`Op::Cycle` and `Fired::Cycled` in the hooks), and what that cycle
produced for the session is sent before it. So a `print` after a `set`
sees the write, and an `eval`'s first value arrives before its answer: no
client waits on a timer.

DAP is not the native protocol: it is built around threads, stack frames
and stepping by line, which fit a dataflow program badly. An adapter can
translate later (phase 3).

## 4. Naming what you ask about

A target is one of:

- **A name**, as the program's top level sees it: `x`, `m::x`. A script's
  top-level names are at the root. Failing that, every binding of that
  name anywhere.
- **A place**, `file.gx:12` or `file.gx:12:5`: the bindings introduced
  there. The file is matched by the end of its path.
- **An id**, `#88`, as an answer shows it.

A binding inside a function exists once for each call that built an
instance. A name or a place answers with all of them; a command that
takes one (`why`, `set`) refuses and lists them by id. `list` shows a
function's binding once with its count of instances. The call path of an
instance is not shown yet (phase 2).

Bindings the author did not write are left out: names a lowering made
(`#..`) and the bindings of a package's own functions' instances.

**A fused block is a box** (Eric, 2026-11-27). Fused code keeps no
bindings, so a place inside it has none to print. `print` of such a place
answers with the blocks that compute it, one for each instance fused: the
block's text, its type and the value it last produced, and the bindings
it reads, each of which `set` can write. A block computes a place when
its own expression covers it, or when it calls a function whose `let`
covers it. Nothing more is offered inside fused code, and a session does
not turn fusion off.

An expression restored from a session image does not say where it ends,
so coverage is decided by parsing the file's text again
(`seek::covering`) and matching each node's expression against the
expressions that cover the place.

## 5. Commands

**Phase 1, built.**

| Command | What it does |
|---|---|
| `eval EXPR` | compile against the live program as a REPL input and run it: any Graphix, a read, a `<-`, a `let`. What it binds lives until the session ends. Its value, where its last expression has one, is shown as it changes until the session's next `eval`, as the REPL replaces what it shows |
| `print TARGET` | each binding's type, value and the cycle of its last delivery; for a place inside fused code, the block |
| `list [MODULE \| FILE]` | the program's bindings in file order |
| `set TARGET = EXPR` | evaluate `EXPR` once and deliver its value to one binding as an embedder's write does. Any binding may be set; a `let x = a + b` that is set holds the value until `a` or `b` fires again. A value that is not of the binding's type is refused |
| `watch TARGET` / `unwatch N` | an event at each delivery to the bindings: the cycle and the value, or that it went to none |
| `why TARGET` | §6 |
| `info` | the cycle, the counts of bindings and fused blocks, the timers and tasks pending, the sessions and watches |

`eval` may do anything in the program (Eric: allow it all). It compiles
as a REPL input does, so the rules for a top-level input hold.

**Phase 2, not built: control, and reasons inside a node.**

| Command | What it does |
|---|---|
| `pause` / `continue` / `step [n]` | stop between cycles, run one or `n`; `graphix --debug-wait` and `GXConfig::paused` start a program paused |
| `break TARGET [fires \| is bottom \| == v]` | pause at the end of the cycle in which it happens |
| `cycles [n]` | what the last `n` cycles delivered and what fired (a ring kept while a session is attached) |
| `why`, deeper | which arm a select holds and why the others sleep, the step a seq is at and what it waits for, a call with no answer yet, a connect or a sample that has never fired, a fresh block not yet born; the binder of a parameter or a pattern's name; an instance's call path |

**Phase 3, not built: editors.** The language server already resolves the
cursor to a binding. With a session attached, hover shows the live value
and `why` is a code action. Then a DAP adapter, completion in the CLI (a
`complete` request), and a TUI client written in Graphix.

## 6. `why`

Spreadsheets answer the same question with "trace error": from a blank
cell, walk what it reads back to where the blank starts.

`why` finds the `let` that binds the target (`Refs::seeking`) and the
bindings its value reads from outside itself. A read with no value is
followed to its own `let`; a read with a value is a leaf. A binding that
has no value while everything it reads has one is where the nothing
starts, and its expression is printed. Functions are left out of the
reads, and a name a lowering made is replaced by what it reads.

```text
debug> why total
total: i64 has no value    #4253 prog.gx:5:5, none since cycle 0
  let total = price * qty
  it reads:
    price: i64 = 10    #4251 prog.gx:3:5, written in cycle 0
    qty: i64 has no value    #4252 prog.gx:4:5, none since cycle 0
      let qty = a[7]$
      it reads:
        a: Array<i64> = [1, 2, 3]    #4250 prog.gx:2:5, written in cycle 0
      everything it reads has a value: the nothing starts in this expression
```

It stops at the binding. What inside `a[7]$` produced nothing (here, an
index error the `$` dropped, which the program's stderr names) needs the
node to describe itself: an `explain` method on `Update` that a select, a
seq machine, a call site, a connect, a sample, a `$` and a fresh block
implement. That is phase 2. A binding no `let` binds (a parameter, a
pattern's name) is reported as such.

## 7. Inside the runtime

- **Requests** run on the runtime's thread between cycles: reads of the
  environment and the store through `GXHandle::with_ctx`, and what needs
  the node graph or the sessions through `ToGX::Debug`. Nothing is read
  while a cycle or its forks run.
- **The store already holds what `print` shows**: each binding's last
  production and the cycle that delivered it. A watch is a look at the
  watched bindings' stamps after each cycle (`GX::debug_cycle`).
- **A session's expressions** are roots like a REPL input's. Their values
  are taken out of the embedder's batch and sent to the session, so a
  display never sees them.
- **A session that ends** detaches (`Attached`'s drop), and the handles
  of what it compiled are dropped with it, which deletes them.
- **Session images, fusion, parallel evaluation and walk pruning** are
  untouched. A debugger that changes what a program computes is a bug.

## 8. Security

The service reads every value, secrets included, runs any Graphix in the
program and injects events. It is off by default. The shell's socket is
the owner's alone. An embedder decides for itself, which is why it hands
over the listener. The protocol has no authentication of its own: that is
the transport's.

## 9. Testing

- The service over an in-memory stream, in every mode
  (`stdlib/graphix-tests/tests/shared/debugger.rs`): each command;
  instances told apart; a session's values kept from the embedder and its
  bindings deleted when it closes; a watched program computing what it
  computes alone.
- The shell (`graphix-shell/tests/shell/debugger.rs`): a second process
  asking a running program, cold and from the image, in both engines; the
  fused box; the default socket's place and permissions.
- Not built: a fuzzer lane holding a watched program's trace to the
  unwatched one's.

## 10. What it does not do

No stepping inside a cycle. No view inside a fused block. No time
travel: the cycle log (phase 2) says what fired, and cannot be replayed.
