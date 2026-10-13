#!/bin/sh
# every cancelled timer (an arm going to sleep) logs an ERROR
# "a runtime task ended without replying: task N was cancelled"
mkdir -p /tmp/xc01 && RUST_LOG=error graphix --no-cache --log-dir /tmp/xc01 "$(dirname "$0")/x-concurrency-01.gx"
