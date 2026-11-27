# Debugging a Running Program

When a Graphix program goes wrong it usually does nothing at all: a value
never arrives, so everything that reads it waits. The debugger lets you
ask a running program what it holds and why, from a second terminal,
without stopping it.

Start the program with `--debug`:

```bash
graphix --debug ./myprogram.gx
```

It prints where its debugger listens (`debugger: graphix debug ...`) and
runs as usual. In another terminal, attach:

```bash
graphix debug
```

With one program offering a debugger that is all it takes. With several,
name the one you mean by its process id or the socket's path.

## Asking

`print` shows a binding's type, its value, and the cycle that last wrote
it. `list` shows every binding your program introduces.

```text
debug> print price
price: i64 = 10    #4251 myprogram.gx:3:5, written in cycle 0
debug> print total
total: i64 has no value    #4253 myprogram.gx:5:5, none since cycle 0
```

`why` is the question to ask of a binding with no value. It shows what
binds it and what that reads, and follows each read that has no value
back to where the nothing starts:

```text
debug> why total
total: i64 has no value    #4253 myprogram.gx:5:5, none since cycle 0
  let total = price * qty
  it reads:
    price: i64 = 10    #4251 myprogram.gx:3:5, written in cycle 0
    qty: i64 has no value    #4252 myprogram.gx:4:5, none since cycle 0
      let qty = a[7]$
      the nothing starts inside fused code: --no-fusion shows where: a[7]$
      it reads:
        a: Array<i64> = [1, 2, 3]    #4250 myprogram.gx:2:5, written in cycle 0
```

Here `total` waits on `qty`, and `qty` is `a[7]$`, which compiled to
native code: the debugger can say the nothing starts there, and that
everything it reads has a value. Run the program with `--no-fusion` and
it says what happened inside:

```text
      let qty = a[7]$
      `$` dropped the error `ArrayIndexError("array index out of bounds"): a[7]$
```

The index is out of range and the `$` drops the error (the program also
said so on its stderr).

Along the way `why` says what each construct knows about itself:

```text
debug> why shown
shown: string has no value    #4276 myprogram.gx:10:5, none since cycle 0
  let shown = select mode {
      `Edit => editor(doc),
      `List => "list"
  }
  the select holds arm 1 of 2: `Edit; its argument was last `Edit
    in this call: editor(doc)
  it reads:
    mode: [`Edit, `List] = `Edit    #4272 myprogram.gx:7:5, written in cycle 0
    doc: string has no value    #4273 myprogram.gx:8:5, never written
      let doc: string = never()
      `never()` never has a value
```

A select names the arm it holds, and only that arm is followed. A `seq`
names the step it is at, or says it is waiting for its trigger. A sample
(`e ~ v`) says whether it waits on `e` or on `v`. A call says whether it
has been made and follows the function's body. For a binding inside a
function, `why` names the call it belongs to, and for a parameter, the
call that passes it.

`watch` reports each time a binding is written, until you `unwatch` it:

```text
debug> watch tick
watch 1: tick: i64 = 41    #4249 myprogram.gx:1:5, written in cycle 96
tick = 42    watch 1, cycle 98
tick = 43    watch 1, cycle 100
debug> unwatch 1
```

## Naming a binding

A command's target is one of

- a name, as the top of your program sees it: `price`, `net::retries`;
- a place: `myprogram.gx:12`, or `myprogram.gx:12:5` for a line and column;
- an id, as an answer shows it: `#4251`.

A binding inside a function exists once for every call of the function,
so a name or a place can stand for several. `print` shows them all; `why`
and `set` take one, and list the ids to choose from when you name several.

## Evaluating and injecting

A line that is not a command is Graphix, evaluated in the running program
exactly as a REPL input is. It can read anything, bind new names, and
write:

```text
debug> price * 3
-: i64
30
debug> let fee = price / 10
debug> price <- 12
```

The value of the last expression you evaluate is shown again each time it
changes, until you evaluate something else. What you bind lasts until you
detach. Start a line with `eval` when its first word is also a command's
name.

`set` delivers a value to any binding, as if the program had written it:

```text
debug> set price = 100
price <- 100
```

A binding computed from others keeps the value you set until one of them
fires again.

## Pausing and stepping

`pause` holds the program: it runs nothing more until you say so. What
arrives for it meanwhile (a timer, a key, a network reply) waits. You can
still ask anything, and evaluate against what stands.

`step` lets it run just until the next cycle that does something, then
holds it again and shows what that cycle wrote. `continue` lets it run.

```text
debug> pause
held before cycle 5
debug> step
held after cycle 6: tick = 1
debug> set price = 5
price <- 5    the program is held: it lands when it runs
debug> step
held after cycle 12: price = 5, total has no value, scaled = 11
debug> continue
running
```

`break` holds the program by itself, at the cycle a binding is written:

```text
debug> break tick == 12
watch 2: tick: i64 = 4    #4249 myprogram.gx:1:5, written in cycle 13
tick = 5    watch 2, cycle 15
...
break 2: held after cycle 29: tick = 12
```

`break NAME` holds at any write, `break NAME == EXPR` at that value, and
`break NAME none` when it loses its value. `unwatch` removes one.
`cycles` shows what each of the last few cycles wrote.

To debug a program's very start, run it with `--debug-wait`: it offers
its debugger and waits, held, before its first cycle, until you `step` or
`continue`.

When you detach, a held program runs again.

## Native code

Code that compiled to native code keeps no bindings, so there are none to
print inside it. Ask about a place there and the debugger shows the fused
block that computes it instead: its text, what it last produced, and the
bindings it reads, which you can `set`.

```text
debug> print myprogram.gx:7
fused code keeps no binding here; it is inside:
a fused block at line 10, column 14: i64 = 21
  scale(price)
  it reads:
    price: i64 = 10    #4251 myprogram.gx:3:5, written in cycle 0
```

Run the program with `--no-fusion` to see inside.

## Scripts and agents

`graphix debug --run` does the whole thing in one command: it runs the
program with its debugger on, asks, and stops it. What the program
prints is passed on, marked `[out]` or `[err]`.

```bash
graphix debug --run ./myprogram.gx -c 'why total'
graphix --no-fusion debug --run ./myprogram.gx -c 'why total'
graphix debug --run ./myprogram.gx --held -c 'break total none' -c continue -c wait -c 'why total'
```

`-c` runs one command and may be given several times; lines piped to
`graphix debug` are run the same way. The exit status is 1 if a command
was refused. `--held` starts the program held, as `--debug-wait` does,
and `--` passes the program its own arguments. `--json` prints each
answer as a line of JSON.

A script never waits for ever. A `step` is waited out before the next
command, and `wait` waits until a break holds the program, but each
gives up after `--timeout` seconds (30 unless you say otherwise; `wait 5`
limits one wait) and counts as a refusal. A step that found nothing to
do is ended, and leaves the program held.

At a terminal nothing times out: press the interrupt key to abandon a
command that is waiting, and `pause` to end a step.

To attach to a program that is already running, leave out `--run`:

```bash
graphix debug -c 'why total' -c 'print price'
```

`--debug=PATH` chooses the socket. By default it is in your runtime
directory (`$XDG_RUNTIME_DIR/graphix/`), named for the process, and only
you can open it. Anyone who can open it can read every value in the
program and run code in it, so treat the socket as you would the
program's own memory.

The debugger speaks lines of JSON over the socket, so another program can
drive it. An application that embeds Graphix offers it over any byte
stream with `graphix_rt::debug::Debugger`.

`help` lists the commands.
