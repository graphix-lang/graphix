#!/usr/bin/env bash
# fuzz-main-aux-10: fleet.sh verify waits the full LAUNCH_WAIT (+120s) on a
# launch that already failed, then reports the box UNREACHABLE.
#
# command: timeout -s KILL 180 bash design/review-2026-10-05/repro/fuzz-main-aux-10.sh [exists|regress]
#
# Runs the real graphix-fuzz/fleet.sh `launch` and `verify` for one box
# (FLEET_ONLY=aieka) against this machine. Nothing is built or launched:
#   - a fake `ssh` runs the remote script here, under a scratch HOME, with a
#     clean environment (env -i), as a fresh login would;
#   - a fake `perl` stands in for the detach one-liner: it runs the launcher
#     in the foreground (no setsid) and exits 0, as the forking parent does;
#   - a fake `cargo` (first on the launcher's PATH, $HOME/.cargo/bin) answers
#     `metadata` and `build` without building anything.
# Scenarios:
#   exists  (default) the campaign directory is already there: the restart
#           that the FLEET_ONLY comment (fleet.sh:86) advertises. soak.sh:200
#           refuses it.
#   regress the build "succeeds" and its graphix-fuzz `regress` fails the gate
#           and exits 1, as a commit that broke a pinned bug does (soak.sh:278).
# FLEET_LAUNCH_WAIT is 20s instead of the default 5400s.
#
# expected: verify notices that the launcher exited without FLEET_LAUNCH_OK and
#   reports the launch log's failure within seconds.
# observed (HEAD c722befe), both scenarios:
#   the launcher exits 1 in under a second; its log holds
#   "campaign directory already exists: ..." (exists) or
#   "regression corpus: N programs, 1 regressions" (regress), and no marker.
#   verify returns after ~140s (= LAUNCH_WAIT + 120) with
#   "aieka    UNREACHABLE" and "DEPLOY DEGRADED"; the log's error is not shown.
#   The remote loop waits out the budget, prints FLEET_TIMEOUT, then waits up to
#   600s more for counter lines, so the local `timeout LAUNCH_WAIT+120` kills
#   ssh and FLEET_UNREACHABLE is checked first (fleet.sh:388).
#   With the default LAUNCH_WAIT=5400 that is 92 minutes per box, one box after
#   another (fleet.sh:334), so a deploy whose build fails on every box blocks for
#   about 6 hours (4 boxes) or 4.6 hours (3 boxes).
set -euo pipefail
scenario=${1:-exists}
[[ $scenario == exists || $scenario == regress ]] || { echo "usage: $0 [exists|regress]" >&2; exit 2; }
repo=${GRAPHIX_REPO:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)}
[[ -x $repo/graphix-fuzz/fleet.sh ]] || { echo "cannot find graphix-fuzz/fleet.sh under $repo" >&2; exit 2; }
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export FAKE_HOME=$work/home SHIM_BIN=$work/bin
mkdir -p "$FAKE_HOME/tmp/target/fuzz" "$FAKE_HOME/proj" "$FAKE_HOME/.cargo/bin" "$SHIM_BIN"
ln -s "$repo" "$FAKE_HOME/proj/graphix"
camp=reprocamp

cat > "$SHIM_BIN/ssh" <<'EOF'
#!/usr/bin/env bash
shift
exec env -i HOME="$FAKE_HOME" PATH="$SHIM_BIN:/usr/bin:/bin" bash -c "$*"
EOF
cat > "$SHIM_BIN/perl" <<'EOF'
#!/usr/bin/env bash
shift 3
rc=0; "$@" || rc=$?
echo "$rc" > "$HOME/launcher.status"
exit 0
EOF
cat > "$FAKE_HOME/.cargo/bin/cargo" <<'EOF'
#!/usr/bin/env bash
case $1 in
    metadata) printf '{"target_directory":"%s"}\n' "$HOME/tmp/target" ;;
    build) exit 0 ;;
    *) echo "fake cargo: refusing $*" >&2; exit 1 ;;
esac
EOF
chmod +x "$SHIM_BIN/ssh" "$SHIM_BIN/perl" "$FAKE_HOME/.cargo/bin/cargo"

if [[ $scenario == exists ]]; then
    mkdir -p "$FAKE_HOME/tmp/target/fuzz/$camp/state"
else
    mkdir -p "$FAKE_HOME/tmp/target/release"
    n=$(find "$repo/graphix-fuzz/findings" -name '*.gx' | wc -l | tr -d ' ')
    cat > "$FAKE_HOME/tmp/target/release/graphix-fuzz" <<EOF
#!/usr/bin/env bash
[[ \$1 == regress ]] || { echo "stub graphix-fuzz: refusing \$*" >&2; exit 2; }
echo "regression corpus: $n programs, 1 regressions"
echo "  REGRESSION some-pin/00 — interp != jit"
exit 1
EOF
    chmod +x "$FAKE_HOME/tmp/target/release/graphix-fuzz"
fi

export PATH="$SHIM_BIN:$PATH"
cd "$repo"
echo "== fleet.sh launch ($scenario)"
FLEET_ONLY=aieka ./graphix-fuzz/fleet.sh launch "$camp" 14700000000
log=$FAKE_HOME/tmp/fleet-$camp-launch.log
echo "launcher exit status: $(cat "$FAKE_HOME/launcher.status")"
echo "launch log:"
sed 's/^/  | /' "$log"
grep -q FLEET_LAUNCH_OK "$log" && echo "FLEET_LAUNCH_OK present" || echo "no FLEET_LAUNCH_OK in the launch log"

echo "== fleet.sh verify, FLEET_LAUNCH_WAIT=20"
start=$(date +%s)
rc=0
FLEET_LAUNCH_WAIT=20 FLEET_ONLY=aieka ./graphix-fuzz/fleet.sh verify "$camp" || rc=$?
echo "verify exit $rc after $(( $(date +%s) - start ))s"
