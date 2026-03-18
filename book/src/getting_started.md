# Getting Started

Welcome to Graphix! This tutorial will get you up and running in about 10-15 minutes. You'll learn how to use the interactive REPL, write your first expressions, and create a simple reactive program.

By the end of this guide, you'll understand the basics of Graphix's reactive dataflow model and be ready to explore the language in depth.

## Prerequisites

Make sure you've already [installed Graphix](./install.md). You can verify your installation by running:

```bash
graphix --version
```

## Starting the REPL

The Graphix shell provides an interactive Read-Eval-Print Loop (REPL) where you can experiment with the language. Start it by running `graphix` with no arguments:

```bash
graphix
```

You'll see a prompt that looks like this:

```
Welcome to the graphix shell
Press ctrl-c to cancel, ctrl-d to exit, and tab for help
〉
```

The REPL evaluates expressions and shows you both the type and the value. The output format is:

```
-: Type
value
```

Let's try it!

## Your First Expressions

### Arithmetic

Type some simple arithmetic at the prompt:

```graphix
〉2 + 2
-: i64
4
```

The `-: i64` line tells you the result is a 64-bit integer, and `4` is
the value. You may be wondering why you don't get the 〉 prompt after
running this expression. This is because, being a dataflow language,
expressions are pipelines that can output more than one value, they
will run until you stop them by hitting ctrl-c. Do this now to get the
prompt back.

Since ctrl-c is used to stop the currently running pipeline, if you
want to exit the REPL press ctrl-d.

Try a more complex expression:

```graphix
〉10 * 5 + 3
-: i64
53
〉2.5 * 4.0
-: f64
10.0
```

Notice that integer arithmetic produces `i64` (integer) results, while floating-point arithmetic produces `f64` (float) results.

### Strings

Strings are written in double quotes:

```graphix
〉"Hello, Graphix!"
-: string
"Hello, Graphix!"
```

### String Interpolation

Graphix supports string interpolation using square brackets. Any expression inside `[...]` in a string will be evaluated and inserted:

```graphix
〉"The answer is [2 + 2]"
-: string
"The answer is 4"
〉"2 + 2 = [2 + 2], and 10 * 5 = [10 * 5]"
-: string
"2 + 2 = 4, and 10 * 5 = 50"
```

This is incredibly useful for building dynamic strings!

## Variables with Let Binds

Use `let` to create named bindings:

```graphix
〉let x = 42
〉x
-: i64
42
〉let name = "World"
〉"Hello, [name]!"
-: string
"Hello, World!"
```

You can reuse the same name to create a new binding (this is called shadowing):

```graphix
〉let x = 10
〉let x = x + 5
〉x
-: i64
15
```

The second `let x` creates a new binding that references the previous value of `x`.

## Functions

Functions in Graphix are first-class values. Create them with the lambda syntax `|args| body`:

```graphix
〉let double = |x| x * 2
〉double(21)
-: i64
42
```

You can add type annotations if you want to be explicit:

```graphix
〉let add = |x: i64, y: i64| x + y
〉add(10, 32)
-: i64
42
```

Functions can capture variables from their surrounding scope:

```graphix
〉let multiplier = 3
〉let times_three = |x| x * multiplier
〉times_three(14)
-: i64
42
```

## Creating Your First File

Now let's write a real Graphix program! Create a file called `hello.gx` with this content:

```graphix
use tui;
use tui::text;

let count = 0;
let timer = sys::time::timer(duration:1.s, true);
count <- timer ~ (count + 1);

text(&"Count: [count]")
```

This program demonstrates Graphix's reactive nature:

- We start with `count = 0`
- `sys::time::timer(duration:1.s, true)` creates a timer that fires every second
- The `~` operator samples the right side when the left side updates
- `count <- ...` schedules an update to `count` for the next cycle
- Every second, `count` increments and the text automatically updates
- The last expression creates a text widget displaying the count

## Running Your File

Run your program with:

```bash
graphix hello.gx
```

You'll see a terminal UI that displays the count increasing every second! The screen updates automatically because Graphix tracks dependencies and propagates changes through the dataflow graph.

To stop the program, press `Ctrl+C`.

## A Simpler Example

If you want to see the reactive behavior without the TUI, try this simpler version (`counter.gx`):

```graphix
let count = 0;
let timer = sys::time::timer(duration:1.s, true);
count <- timer ~ (count + 1);
"Count: [count]"
```

Run it with `graphix counter.gx` and you'll see the count printed to the console every second. 

## Understanding the Output

In Graphix programs:

- **The last value** is what determines what the shell displays
  - If it's a `Tui` type (like our text example), then it is rendered as a TUI
  - Otherwise, the value is printed to the console every time it updates
- **Use `print` or `println`** for explicit output during execution
- **Programs run forever** unless they explicitly exit - they're reactive graphs that respond to events

### The Dataflow Model

The key insight: when `timer` updates, it triggers an update to `count` (via the `<-` connect operator), which triggers an update to the text widget. The entire chain reacts automatically. You describe *what* should happen, not *when* or *how* to update things.

This is very different from traditional imperative programming where you'd need loops and manual state management. In Graphix, you build a graph of dependencies and the runtime handles updates for you.

## Try It Yourself

Experiment with these ideas:

1. Modify the counter to count down instead of up
2. Make it count by 2s or 10s instead of 1s
3. Change the timer interval to 0.5 seconds
4. Display multiple values that update independently
5. Try arithmetic on the count (show doubled value, squared value, etc.)

## Next Steps

Now that you've experienced Graphix's reactive nature, you're ready to dive deeper:

- **[Core Language](./core/overview.md)** - Learn the fundamental language constructs
- **[Functions](./functions/overview.md)** - Master functions, closures, and higher-order programming
- **[Building UIs](./ui/overview.md)** - Create rich terminal user interfaces
- **[Standard Library](./stdlib/overview.md)** - Explore built-in functions and modules

The best way to learn Graphix is to experiment! Keep the REPL open as you read through the documentation and try out the examples. Every code snippet in this book is designed to be runnable.
