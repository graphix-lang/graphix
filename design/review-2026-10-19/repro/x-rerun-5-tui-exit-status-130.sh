#!/bin/sh
# x-rerun-5: tui::exit ends a script with status 130 (SIGINT's), not 0 (re-run of tui-core-02)
# command: sh design/review-2026-10-19/repro/x-rerun-5-tui-exit-status-130.sh <graphix binary>
# expected: "tui exit status=0" (the program asked to end; nothing interrupted it),
#           as "sys exit status=0" for sys::exit
# observed (HEAD 9445a1d21, debug): "tui exit status=130", "sys exit status=0".
# InputReader::read_line maps a custom display's normal stop (Ok(Ok(()))) to Signal::CtrlC,
# and Shell::run turns Signal::CtrlC in a script into Some(128 + 2).
G=${1:-graphix}
D=$(mktemp -d)
printf 'use tui::text::text;\ntui::exit(sys::time::timer(duration:50.ms, false));\ntext(&"hello")\n' > "$D/tui.gx"
printf 'use tui::text::text;\nsys::exit(sys::time::timer(duration:50.ms, false) ~ 0);\ntext(&"hello")\n' > "$D/sys.gx"
for f in tui sys; do
  timeout -s KILL 10 script -qfec "$G --no-cache $D/$f.gx" /dev/null > /dev/null 2>&1
  echo "$f exit status=$?"
done
rm -rf "$D"
