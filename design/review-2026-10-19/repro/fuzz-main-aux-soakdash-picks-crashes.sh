#!/usr/bin/env bash
# Title: soak-dash.gx's remote probe reports the default `crashes` corpus dir
# as the box's campaign once it is the newest entry under ~/tmp/target/fuzz.
# Command: bash design/review-2026-10-19/repro/fuzz-main-aux-soakdash-picks-crashes.sh
# Expected: "LIVE oct19a yes ..." (the running campaign, which has a live soak.pid)
# Observed: "LIVE crashes no ..." -- a manual `graphix-fuzz fuzz|generate|soak`
#   without GRAPHIX_FUZZ_CORPUS writes findings to ~/tmp/target/fuzz/crashes
#   (main.rs corpus_dir), which bumps that dir's mtime above the campaign's.
# The campaign-selection lines below are copied verbatim from the `remote`
# script in graphix-fuzz/soak-dash.gx, run against a fake HOME.
set -u
fake=$(mktemp -d)
trap 'kill $sleeper 2>/dev/null; rm -rf "$fake"' EXIT
mkdir -p "$fake/tmp/target/fuzz/oct19a/state"
sleep 30 & sleeper=$!
echo $sleeper > "$fake/tmp/target/fuzz/oct19a/state/soak.pid"
sleep 1.1
mkdir -p "$fake/tmp/target/fuzz/crashes"
touch "$fake/tmp/target/fuzz/crashes/divergence_000000.gx"
HOME=$fake bash -c '
  d=$(ls -dt ~/tmp/target/fuzz/*/ 2>/dev/null | head -1)
  c=$(basename "$d")
  pidfile="$d/state/soak.pid"
  pid=$(cat "$pidfile" 2>/dev/null)
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then live=yes; else live=no; fi
  echo "LIVE $c $live"
'
