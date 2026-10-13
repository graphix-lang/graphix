#!/bin/sh
# gx-stdlib-03: args::parse flattens all command levels: a subcommand's unset arg erases the parent's.
# (gx-stdlib-03.gx in this directory belongs to another finding; this probe is a .sh.)
# run: sh design/review-2026-10-12/repro/gx-stdlib-03.sh [path/to/graphix]
# expected: values{"level"} is "1" (the root's --level 1), or the spec refused
# observed:  {command: ["sub"], values: {"level" => null, "v" => "false"}}
G=${1:-graphix}
F=$(mktemp --suffix=.gx)
cat > "$F" <<'GX'
let cmd = args::command(#name: "t", #subcommands: [args::command(#name: "sub", [args::option(#name: "level")])], [args::option(#name: "level"), args::flag(#name: "v")]);
let r = args::parse(cmd);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
GX
timeout -s KILL 60 "$G" --no-cache "$F" --level 1 sub
rm -f "$F"
