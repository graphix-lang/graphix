#!/bin/sh
# f-kernel-04: the shell panics "called after complete" when the runtime dies
# inside the input loop: Shell::run's `exit_rx` arm ignores Err(RecvError)
# (the ExitRequest sender in libstate drops with the runtime) and the next
# select! iteration polls the completed oneshot::Receiver again.
#
# command: sh f-kernel-04.sh /path/to/graphix
# expected: Error: a runaway recursion exceeded the runtime's budget
#           (GRAPHIX_STACK_BUDGET or GRAPHIX_ACTIVATION_BUDGET) and it stopped
#           (what the same program prints when the abort happens during load)
# observed (HEAD 7324207f, debug build, 3/3 runs as a script, 5/5 in the REPL):
#   thread 'graphix-tokio' panicked at .../tokio-1.52.2/src/sync/oneshot.rs:1289:13:
#   called after complete
#   Error: tokio thread panicked
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/t.gx" <<'GX'
let rec f = |n: i64| -> i64 select n < 1 { true => 0, false => f(n - 1) + 1 };
let t = sys::time::after_idle(duration:200.ms, 1000);
f(t)
GX
GRAPHIX_ACTIVATION_BUDGET=100 timeout -s KILL 30 "$G" --no-cache "$D/t.gx"
echo "rc $?"
rm -rf "$D"
