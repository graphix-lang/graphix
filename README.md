<p align="center">
  <img src="docs/graphix-logo-full.svg" alt="Graphix" height="72">
</p>

Graphix is a statically-typed functional programming language that compiles to
reactive dataflow graphs. Unlike React or Vue where reactivity is a library
bolted onto the language, Graphix is reactive at the language level — every
expression is a node in a live graph, and changes propagate through
automatically.

It feels familiar to anyone who knows Haskell, OCaml, or F#: lexically scoped,
expression-oriented, with algebraic data types, pattern matching, parametric
polymorphism, and first-class functions. It's particularly well-suited for
building terminal UIs and working with streaming network data via
[netidx](https://netidx.github.io/netidx-book).

```graphix
use tui;
use tui::block;
use tui::text;

let counter = 0;
let clock = time::timer(duration:1.s, true);
counter <- clock ~ (counter + 1);

block(
  #border: &`All,
  #title: &line("My First TUI"),
  #style: &style(#fg: `Green),
  &text(&"Counter: [counter]")
)
```

## Install

```bash
cargo install graphix-shell
```

Requires Rust and `clang` / `libkrb5-dev` (Debian/Ubuntu) or `clang-devel` /
`krb5-devel` (Fedora).

## Learn More

**[graphix-lang.github.io/graphix](https://graphix-lang.github.io/graphix)**

[Documentation](https://graphix-lang.github.io/graphix/book/) ·
[Discord](https://discord.gg/bQv4gNR8WK) ·
[GitHub](https://github.com/graphix-lang/graphix)

## License

MIT
