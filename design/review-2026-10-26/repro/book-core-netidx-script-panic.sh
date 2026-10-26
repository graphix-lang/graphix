#!/bin/sh
# Title: `graphix netidx:/path` panics when no netidx config file exists (the zero-config case)
# Command: sh design/review-2026-10-26/repro/book-core-netidx-script-panic.sh
#   (on a machine with no ~/.config/netidx and no /etc/netidx config)
# Expected: the shell subscribes to the script through the machine-local
#   resolver, as every other netidx use does without a config file.
# Observed:
#   thread 'graphix-tokio' panicked at netidx/src/config/mod.rs:542:14:
#   Cannot start a runtime from within a runtime. ...
#   Error: tokio thread panicked
# Cause: NetArgs::pub_sub (graphix-shell/src/main.rs) calls NetArgs::load on the
# async main runtime; Config::load_default_or_local_only falls back to
# Config::local_only, which builds a runtime and block_on's it.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
timeout -s KILL 10 "$G" --no-cache --resolve-timeout 2 netidx:/review/none
