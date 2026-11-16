#!/bin/bash
# Title: probe-run never reports a shared-pool OOM: a slice's memory.events.local
#        oom_kill stays 0 when its own limit kills a process in a child scope
#
# Command: bash design/review-2026-11-16/repro/ide-tooling-probe-run-pool-oom.sh
#   (one transient, delegated user scope capped at 200M plays the
#   review-probes.slice pool; a child cgroup capped at 1G plays the run's
#   scope; python touches 400M in the child, so the POOL limit kills it)
#
# Expected (what review/probe-run assumes): the pool's memory.events.local
#   oom_kill moves, so `[ "$al" != "$bl" ]` picks the "shared pool ran out"
#   message.
# Observed (Linux 7.3, cgroup v2):
#   before: pool events oom_kill=0 pool events.local oom_kill=0 pool events.local oom=0
#   python exit: 137
#   after:  pool events oom_kill=1 pool events.local oom_kill=0 pool events.local oom=1
#   leaf events.local oom_kill=1
# The kill is counted locally on the victim's cgroup (the leaf, i.e. the
# run's scope), never on the slice, which holds no processes of its own. The
# pool's own limit shows in the slice's LOCAL `oom` (and `max`) counters.
set -euo pipefail
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
cat > "$T/inner.sh" <<'EOF'
#!/bin/bash
CG=/sys/fs/cgroup$(sed -n 's/^0:://p' /proc/self/cgroup)
mkdir "$CG/leaf"
echo $$ > "$CG/leaf/cgroup.procs"
echo +memory > "$CG/cgroup.subtree_control"
echo 1G > "$CG/leaf/memory.max"
echo 0 > "$CG/leaf/memory.swap.max"
k() { awk -v f="$2" '$1==f{print $2}' "$1"; }
s() { echo "pool events oom_kill=$(k $CG/memory.events oom_kill) pool events.local oom_kill=$(k $CG/memory.events.local oom_kill) pool events.local oom=$(k $CG/memory.events.local oom)"; }
echo "before: $(s)"
python3 -c 'b = b"\x01" * (400 * 1024 * 1024); print("allocated")' || echo "python exit: $?"
echo "after:  $(s)"
echo "leaf events.local oom_kill=$(k $CG/leaf/memory.events.local oom_kill)"
EOF
chmod +x "$T/inner.sh"
timeout -s KILL 60 systemd-run --user --scope --quiet --collect -p Delegate=yes \
    -p MemoryMax=200M -p MemorySwapMax=0 -- "$T/inner.sh"
