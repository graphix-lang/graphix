#!/bin/sh
# small-pkgs-03: args check_spec misses clap invariants; each spec below panics
# in clap's debug_asserts inside args_parse's update and kills the runtime.
# run: sh small-pkgs-03.sh /path/to/graphix   (a debug build)
# expected: each prints error:["ArgError", ...] (as a bad #short does today)
# observed (HEAD 7324207f, debug): "panicked at clap_builder-4.6.0/src/builder/
#   debug_asserts.rs ..." then "Error: runtime did not respond", for
#   dup: Argument names must be unique ... 'x'
#   help: Argument names must be unique ... 'help'
#   posflag: Argument names must be unique ... 'x'
#   sub: command name `s` is duplicated
#   req: Found non-required positional argument with a lower index than a required one
G=${1:-graphix}
D=$(mktemp -d)
pre='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
p() { printf '%s\nargs::parse(%s)\n' "$pre" "$2" > "$D/$1.gx"; }
p dup 'args::command(#name: "t", [args::positional(#name: "x"), args::positional(#name: "x")])'
p help 'args::command(#name: "t", [args::positional(#name: "help")])'
p posflag 'args::command(#name: "t", [args::positional(#name: "x"), args::flag(#name: "x")])'
p sub 'args::command(#name: "t", #subcommands: [args::command(#name: "s", []), args::command(#name: "s", [])], [])'
p req 'args::command(#name: "t", [args::positional(#name: "a"), args::positional(#name: "b", #required: true)])'
for f in dup help posflag sub req; do
  echo "== $f"
  timeout -s KILL 30 "$G" --no-cache "$D/$f.gx" a b 2>&1 | grep -v RUST_BACKTRACE
done
rm -rf "$D"
