#!/bin/bash
# A script with an interface (main.gxi), or a module with an empty
# interface, never restores its program entry: the record lists the
# interface as ABSENT (`-main.gxi`), so every start finds the entry
# stale, compiles cold and rewrites the entry. Check with
# RUST_LOG=graphix_shell=info --log-dir: "Program image ... is stale"
# then "Program image written" on every run.
G=${GRAPHIX:-graphix}
t=$(mktemp -d)
printf 'let x = 1;\nsys::exit(sys::time::after_idle(duration:1.s, 0));\nx\n' > $t/main.gx
echo 'val x: i64' > $t/main.gxi
export XDG_CACHE_HOME=$t/cache RUST_LOG=graphix_shell=info,graphix_rt=info
for i in 1 2 3; do
  rm -rf $t/log
  timeout -s KILL 30 $G --log-dir $t/log $t/main.gx >/dev/null
  grep -c "parse time" $t/log/*.log   # 1 = compiled cold; a warm start prints 0
done
head -c 600 $t/cache/graphix/registration/*/*.img | strings | grep '^[+-]'
