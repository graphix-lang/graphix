# The debugger

Status: **phases 1 and 2 are built** (ask and inject; hold, step and
break; what a binding's nodes say of why it has no value). Phase 3, the
editor, is designed here and not built. Eric's rulings of 2026-11-27 decided the shape (D63
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
| Seek | `graphix-compiler/src/seek.rs`, `explain.rs` | what binds a binding, the fused blocks that compute a place (both riding `Update::refs`, the one walk that reaches every node), and why an expression has no value |
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
- `graphix --debug-wait prog.gx` offers the socket and holds the program
  before its first cycle: it starts when a session says `continue` or
  `step`.
- `graphix debug [PATH | PID] [-c CMD]..` attaches. With no target it
  attaches to the one program that offers a debugger and lists them when
  there are several. `-c` runs a command and exits (several may be
  given), as lines piped to stdin do; the exit status is 1 if any was
  refused, and a `step` is waited out before the next command. At a
  terminal it reads lines with a line editor, and what a watch or a
  shown value reports prints above the prompt.
- `graphix debug --run FILE [-c CMD].. [-- ARGS]` is one command for an
  agent or a script: it runs the program with its debugger offered on a
  socket of its own (`--held`: before its first cycle), attaches, runs
  the commands, and stops the program. What the program prints is passed
  on, each line marked `[out]` or `[err]`. `--no-fusion`, `--no-cache`,
  `--no-netidx` and `--no-init` given before `debug` are the program's.
  A program that ends by itself is given the time a shell takes to end,
  so its error is heard, and how it ended is said.
- `--json` prints each answer as a line of JSON, `{"cmd": .., "ok": ..}`
  or `{"cmd": .., "err": ..}`, with the events before it as the protocol
  writes them and the program's lines as `{"program": "out", "line": ..}`.
- **Nothing waits for ever where nobody is there to end it.** With
  commands given or piped in, a `wait` gives up after `--timeout`
  seconds (30 by default; `wait SECONDS` for one wait) and is a refusal;
  a `step` that finds no work in that time is ended by a `pause`, so the
  program is held as after a step, and is a refusal too. At a terminal
  there is no limit, and the interrupt key abandons a command that
  waits.
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
client waits on a timer. A write and the marker of its cycle are one
message to the runtime (`Op::Write`): sent as two, a break could hold the
program between them, and the answer would say the write was held when it
had landed.

`wait` is answered by the runtime once the program is held, at once if it
is, and the `Paused` event of the hold it waited for is sent before the
answer. A client that watched events for the pause instead would miss one
that came before it started to watch. A wait does not hold up its
session: the requests sent after it are answered while it is pending
(`Client::send` and `Client::answered` take an answer out of order), so
the same client ends a wait, or a step that finds no work, with a `pause`.

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
function's binding once with its count of instances, and `why` of one
names the calls it is inside.

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

**Phase 2, built: control.**

| Command | What it does |
|---|---|
| `pause` / `continue` | hold the program, let it run. A held program runs no cycle of its own: what timers, tasks and its embedder send waits. It still answers, and an `eval` still runs, against what stands; a `set` waits with the rest, and its answer says so |
| `step [N]` | let it run until `N` (1) more cycles have done its work, then hold it, saying what the last one delivered. A step with nothing to do waits until something arrives |
| `break TARGET [== EXPR \| none]` | a watch that also holds the program after the cycle in which the binding is written, is written a value equal to `EXPR`'s (evaluated once), or goes to no value. `unwatch N` removes it |
| `cycles [N]` | what each of the last `N` (10) cycles delivered to the bindings the author wrote, functions aside. The last 256 cycles that delivered something are kept while a session is attached |
| `wait` | answer once the program is held: for a script after `continue` to a break. Requests after it are served while it is pending, and a `pause` ends it |

`graphix --debug-wait` and `GXConfig::paused` start a program held. The
last session to leave lets a held program run: held with nobody to
continue it, it would never run again.

**Phase 2, built: reasons inside a node** (§6).

**Phase 3, not built: editors.** The language server already resolves the
cursor to a binding. With a session attached, hover shows the live value
and `why` is a code action. Then a DAP adapter, completion in the CLI (a
`complete` request), and a TUI client written in Graphix.

## 6. `why`

Spreadsheets answer the same question with "trace error": from a blank
cell, walk what it reads back to where the blank starts.

`why` finds what binds the target (`Refs::seeking`), asks the nodes of
its value why they have none (`explain.rs`), and follows each binding
that account reads which has no value in turn. The answer for one
binding is

- **what binds it**: a `let`; a parameter, with the call that passes it;
  a name in a select arm's pattern, with the select and the arm it holds
  instead;
- **the calls it is inside**, one for each function instance around it;
- **what its nodes say** (below);
- **the bindings that bear on it**, each with its value or, lacking one,
  its own answer.

```text
debug> why r
r: i64 has no value    #4271 prog.gx:3:5, never written
  let r = seq tick {
      let got = a[7]$;
      got + 1
  }
  the seq is at step 1 of 2: let got = a[7]$
    `$` dropped the error `ArrayIndexError("array index out of bounds")
debug> why shown
shown: string has no value    #4276 prog.gx:10:5, none since cycle 0
  let shown = select mode {
      `Edit => editor(doc),
      `List => "list"
  }
  the select holds arm 1 of 2: `Edit; its argument was last `Edit
    in this call: editor(doc)
  it reads:
    mode: [`Edit, `List] = `Edit    #4272 prog.gx:7:5, written in cycle 0
    doc: string has no value    #4273 prog.gx:8:5, never written
      let doc: string = never()
      `never()` never has a value
```

### What a node says

`Explain::of(node)` is one exhaustive match over `NodeView`, so a new
node kind must say which way it goes:

- **A value when its parts are** (arithmetic, comparison, indexing, data
  literals, casts, `any`): the account is its parts' accounts. Nothing is
  said of the node itself.
- **A read** is a leaf naming the binding. The runtime looks it up: one
  with a value is shown, one without is followed. A read of a name a
  lowering made is replaced by the account of that name's own `let`, so
  the plumbing of a seq or an `on` block does not show.
- **A node that knows something** says it, in its own file, and says
  nothing when its last production is a value (it is not why):

  | Node | What it says |
  |---|---|
  | select | the arm it holds and the last value of its argument; then its argument's account and that arm's, never an arm it does not hold |
  | seq machine | the step it is at, with that statement's text and account; or that it is not running, with what its trigger reads |
  | sample | whether it waits on its trigger or on what it samples |
  | `$`, `?` | the error its operand holds, or the null it strips: the operand's last production, read through `Update::standing` |
  | call | the arguments' accounts; then that the call has not been made, that a builtin has given no value, or, for a function, its body's account with its parameters left to the call |
  | fresh block | that it has not started |
  | fused kernel | what it reads. It is a box: the service says the nothing starts inside it only when nothing it reads lacks a value |
  | `never()` | that it never has a value |

The bindings that bear on a binding with no value are the reads its
account names, not every binding its expression mentions: a select's
other arms and a seq's later steps are left out. A binding with a value
has no account, and shows everything it reads.

The service turns each note into a sentence (`Session::says`). A note
over an expression a lowering wrote keeps its sentence and loses its
text.

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
- **The hold** is one test in `GX::run`: a held runtime treats nothing
  the program has pending as ready, stops polling the embedder's
  sources, and answers no idle waiter. It still serves every request.
  Where a session waits on a cycle (an `eval`'s first value, a `set`'s
  marker) it runs a quiet one (`do_cycle(quiet)`): no delivery is made,
  and the roots no session owns are parked and put back, so the program
  runs nothing while the session's own expressions are born against the
  store as it stands.
- **A step** counts cycles that did the program's work: one that
  delivered something or ran a root no session owns. The cycles that
  queries start count for nothing.
- **The cycle log** is read off the cycle's event after its nodes ran
  (`Layered::own`), so it costs a pass over that cycle's deliveries, and
  only while a session is attached.
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
  computes alone; a held program waiting, stepped, and held at a break.
- The shell (`graphix-shell/tests/shell/debugger.rs`): a second process
  asking a running program, cold and from the image, in both engines; the
  fused box; the default socket's place and permissions; a program
  started held, stepped and run to a break; `--run` with `--json`, and a
  program that fails to compile; a wait and a step that give up at their
  limit.
- `why_says_what_the_nodes_know` there pins each kind of note.
- Not built: a fuzzer lane holding a watched program's trace to the
  unwatched one's.

## 10. What it does not do

No stepping inside a cycle. No view inside a fused block. No time
travel: the cycle log says what fired, and cannot be replayed. A held
program's timers keep running, so what it sees when it continues is what
piled up, not what it would have seen.
