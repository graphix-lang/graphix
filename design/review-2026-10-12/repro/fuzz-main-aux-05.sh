#!/usr/bin/env bash
# fuzz-main-aux-05: soak.sh stop TERMs the process group a stale pidfile names,
# without checking it is still the campaign's soak.
# Command: timeout -s KILL 60 design/review-2026-10-12/repro/fuzz-main-aux-05.sh
# Expected: the unrelated group leader survives (its timeout wrapper exits 137
#   at 30 s); `soak.sh stop` signals only the campaign binary's processes.
# Observed: "victim's timeout wrapper exited 143": stop_campaign sent SIGTERM
#   to the unrelated process group named by state/soak.pid.
set -u
work=$(mktemp -d)
export GRAPHIX_FUZZ_TARGET=$work
camp=deadcamp
mkdir -p "$work/fuzz/$camp/state"
# An unrelated process-group leader, standing in for whatever reused the
# dead soak's pid (a shell, tmux, another campaign's soak).
timeout -s KILL 30 perl -MPOSIX -e 'POSIX::setsid() or die; exec "sleep", "25"' &
tpid=$!
sleep 0.5
victim=$(pgrep -P "$tpid" sleep)
echo "victim pid=$victim pgid=$(ps -o pgid= -p "$victim" | tr -d ' ')"
echo "$victim" > "$work/fuzz/$camp/state/soak.pid"
"${SOAK:-/home/eric/proj/graphix/graphix-fuzz/soak.sh}" stop "$camp"
wait "$tpid"; rc=$?
echo "victim's timeout wrapper exited $rc (143 = victim got SIGTERM; 137 = survived until timeout)"
rm -rf "$work"
