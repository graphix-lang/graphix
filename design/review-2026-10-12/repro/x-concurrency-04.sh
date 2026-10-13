#!/bin/sh
# A budget abort stops the runtime; the shell then panics with tokio's
# "called after complete" (exit_rx polled again after it resolved Err)
# instead of reporting "a runaway recursion exceeded the runtime's budget".
d=$(dirname "$0")
GRAPHIX_ACTIVATION_BUDGET=5000 graphix --no-cache --no-fusion "$d/x-concurrency-04.gx"
GRAPHIX_STACK_BUDGET=4M graphix --no-cache "$d/x-concurrency-04.gx"
