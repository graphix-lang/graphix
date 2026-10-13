#!/bin/sh
# x-fix-stdlib-05: args::check_spec is narrow: duplicate names, duplicate
# subcommands and positional order still panic clap.
# Run: sh design/review-2026-10-12/repro/x-fix-stdlib-05.sh <debug graphix binary>
# Expected: each spec yields error(`ArgError(..)).
# Observed (debug build, HEAD 7324207f):
#   a: panic clap_builder debug_asserts.rs:99 "Argument names must be unique, but 'file' is in use ..."
#   b: same panic (two positionals named file)
#   c: panic debug_asserts.rs:353 "command name `a` is duplicated"
#   d: panic debug_asserts.rs:657 "Found non-required positional argument with a lower index ..."
#   each kills the runtime ("Error: runtime did not respond")
#   e: an option named "" is accepted: {command: [], values: {"" => null}}
B=${1:-graphix}
D=$(mktemp -d)
tail='
sys::exit(sys::time::after_idle(duration:500.ms, 0));
println("[r]")'
printf '%s%s\n' 'let r = args::parse(args::command(#name: "tool", [args::positional(#name: "file"), args::option(#name: "file")]));' "$tail" > $D/a.gx
printf '%s%s\n' 'let r = args::parse(args::command(#name: "tool", [args::positional(#name: "file"), args::positional(#name: "file")]));' "$tail" > $D/b.gx
printf '%s%s\n' 'let r = args::parse(args::command(#name: "tool", #subcommands: [args::command(#name: "a", []), args::command(#name: "a", [])], []));' "$tail" > $D/c.gx
printf '%s%s\n' 'let r = args::parse(args::command(#name: "tool", [args::positional(#name: "a", #required: false), args::positional(#name: "b", #required: true)]));' "$tail" > $D/d.gx
printf '%s%s\n' 'let r = args::parse(args::command(#name: "tool", [args::option(#name: "")]));' "$tail" > $D/e.gx
for f in a b c d e; do
  echo "== $f"
  timeout -s KILL 30 "$B" --no-cache $D/$f.gx 2>&1 | head -4
done
rm -rf "$D"
