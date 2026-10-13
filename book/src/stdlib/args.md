# args

The `args` module provides command-line argument parsing with support for
positional arguments, options, flags, and subcommands.

## Interface

```graphix
{{#include ../../../stdlib/graphix-package-args/src/graphix/mod.gxi}}
```

`parse` reads the program's own arguments, the ones `sys::args` returns.
In `values` an option's value is its string, and a flag's is `"true"` or
`"false"`. `--help` and `--version` answer `` `ArgHelp `` with the text
to print, and a usage error answers `` `ArgError `` with clap's message,
so a script prints either and exits, 0 for help and non-zero for an
error, as the example does.

## Example

```graphix
{{#include ../examples/net/cli.gx}}
```
