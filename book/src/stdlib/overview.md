# The Standard Library

The Graphix standard library is split into several packages. The `core`
module is always imported with an implicit use statement.

- `core` fundamental functions, types, bitwise operations, and binary encoding/decoding
- `array` functions for manipulating arrays
- `map` functions for manipulating maps
- `str` functions for manipulating strings
- `re` regular expressions
- `rand` random number generator
- `sys` system-level I/O, filesystem, networking, timers, stdio, and directory paths
- `http` HTTP client/server and REST helpers
- `json` JSON serialization and type-directed deserialization
- `toml` TOML serialization and type-directed deserialization
- `pack` native binary serialization via the netidx Pack format
- `xls` read xlsx, xls, ods, and xlsb spreadsheets
- `sqlite` SQLite database access with type-directed query results
- `db` embedded key-value database with transactions, cursors, and reactive subscriptions
- `list` immutable singly-linked lists with structural sharing
- `args` command-line argument parsing with subcommands
- `hbs` handlebars template rendering
