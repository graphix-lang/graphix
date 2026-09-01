#!/usr/bin/env bash

# fleet.sh — the soak fleet's deploy procedure as code.
#
# A deploy is five steps that must happen in order and each of which has
# a way of failing silently: pull the round's divergences off every box,
# stop every campaign (and VERIFY it stopped), get the new tree onto
# every box (and VERIFY it arrived), rebuild + relaunch, and verify the
# launch by FACTS the launch itself emits. The loss model is what makes
# this a script instead of a checklist: an unverified launch that failed
# silently costs a whole night of fleet compute, and nobody is watching.
#
# Every verification here is a fact, never a message:
#   - a stop is verified by pgrep, not by "stopped" on stdout
#   - a sync is verified by a CONTENT fingerprint over the build inputs,
#     not by rsync's exit status (hz0's .git is frozen — it is rsync'd
#     with --exclude .git — so `git log` there is a lie by construction)
#   - a launch is verified by the campaign's own startup gate line, whose
#     corpus count comes from pins EMBEDDED at build time, so a stale
#     binary shows up as a count mismatch
#
# Syncthing hosts are NEVER rsync'd into: syncthing is bidirectional and
# a push would propagate stale files back over live work. They are
# waited for and content-verified instead.

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)

# name : sync method : workers : timeout scale : os
# Seeds are allocated in THIS order, 10M apart from the base — the round
# map in memory records the base, so the order is load-bearing.
# Sync method is rsync for EVERY box (2026-08-26, Eric's call): the
# old syncthing-wait path stalled deploys on propagation lag (katana
# sat TREE STALE past SYNC_WAIT while the rest of the fleet verified),
# and a stale tree can strike any box at any deploy. rsync pushes the
# same bytes syncthing would deliver, so the two reconcile without
# conflict copies; the fingerprint check still gates the launch. The
# cost, now fleet-wide: remote .git is excluded, so a box's own
# `git log` says nothing about the tree it runs (long true of hz0).
# Workers = 8x cores, uniform across the fleet where the RAM allows it
# (2026-08-28, Eric's call). aieka proves the ceiling: 288 workers (8x36)
# on 62G runs with headroom, so every box here — all 54-62G except
# katana's 16G — clears 8x at its core count. hz0 20c=160, aieka 36c=288,
# katana 8c=64, ryouko 32c=256, mazikeen 14c=112, washu-chan 16c=128.
# Oversubscription hides the per-subject off-CPU gap (compile/spawn) that
# left a 1:1 box idling at ~50%.
#
# washu-chan is the SESSION box and reaches itself over ssh, so it rides
# the ordinary rsync path with no local special case (2026-08-28, Eric's
# call): its src==dest rsync is a quick-check noop (.git/target excluded).
# It joins a soak only when idle — launch it alone with FLEET_ONLY rather
# than folding it into a full-fleet launch while a session is live.
HOSTS=(
    "hz0:rsync:160:1:linux"
    "aieka:rsync:288:4:linux"
    "katana:rsync:64:4:darwin"
    "ryouko:rsync:256:1:linux"
    "mazikeen:rsync:112:4:linux"
    "washu-chan:rsync:128:1:linux"
)

MIX=${FLEET_MIX:-50:25:25}
SYNC_WAIT=${FLEET_SYNC_WAIT:-600}     # seconds to wait for syncthing
LAUNCH_WAIT=${FLEET_LAUNCH_WAIT:-5400} # seconds to wait for build+gate

f_name()    { echo "${1%%:*}"; }
f_sync()    { echo "$1" | cut -d: -f2; }
f_workers() { echo "$1" | cut -d: -f3; }
f_scale()   { echo "$1" | cut -d: -f4; }
f_os()      { echo "$1" | cut -d: -f5; }

# FLEET_ONLY=<name> restricts every step to one host; FLEET_EXCLUDE=<name>
# holds one host OUT. Both leave the seed math unchanged (each host keeps
# its position-derived seed). FLEET_ONLY adds or restarts a single box
# without touching the rest — bring washu-chan into a running campaign:
# FLEET_ONLY=washu-chan fleet.sh launch <camp> <base>. FLEET_EXCLUDE runs a
# FULL deploy that must skip one box — hold the session box out of a launch
# while a session is live, then add it later with FLEET_ONLY:
# FLEET_EXCLUDE=washu-chan fleet.sh deploy <new> <base> <old>.
#
# FLEET_ASAN=<name>[,<name>...] switches those hosts' campaigns to
# AddressSanitizer binaries (soak.sh SOAK_ASAN=1: nightly build with an
# explicit --target; children run WITHOUT the address-space rlimit —
# ASan's ~20TB shadow reservation — under an RSS cap instead). The
# other knobs adjust automatically: workers/4 (ASan is ~3x RSS per
# child; floor 8) and timeout scale x2 (~2x slowdown). Darwin hosts
# refuse loudly (LSan is off on macOS and soak-start has no knob).
# Composes with FLEET_ONLY/FLEET_EXCLUDE; the seed math is unchanged.
asan_host() {
    [[ -n ${FLEET_ASAN:-} && ",${FLEET_ASAN}," == *",$1,"* ]]
}

skip_host() {
    [[ -n ${FLEET_ONLY:-} && $FLEET_ONLY != "$1" ]] && return 0
    [[ -n ${FLEET_EXCLUDE:-} && $FLEET_EXCLUDE == "$1" ]] && return 0
    return 1
}

say()  { printf '%s\n' "$*"; }
warn() { printf '%s\n' "$*" >&2; }
die()  { printf '%s\n' "$*" >&2; exit 1; }

usage() {
    cat >&2 <<'USAGE'
usage: fleet.sh deploy <new-campaign> <base-seed> [old-campaign]
       fleet.sh pull <campaign>     pull divergences into fuzz/pending-triage/
       fleet.sh stop <campaign>     stop every box, verified by pgrep
       fleet.sh sync                push/await the tree, verify by fingerprint
       fleet.sh launch <campaign> <base-seed>
       fleet.sh verify <campaign>   wait for every launch, then check facts
       fleet.sh status [campaign]
       fleet.sh hosts
USAGE
    exit 2
}

# The authority for the startup gate's expected corpus count: build.rs
# walks exactly this set. Never carry the number in prose — a literal
# goes stale the next time a pin lands, and it fails in the worst
# direction (too high reads every HEALTHY box as a stale binary).
corpus_count() { find "$repo/graphix-fuzz/findings" -name '*.gx' | wc -l | tr -d ' '; }

# A content fingerprint over the inputs that determine the fuzz binary.
# Deliberately NOT the whole tree: book/, docs/ and stray editor files
# differ per box for reasons that cannot affect a build.
FINGERPRINT='cd ~/proj/graphix && find graphix-compiler graphix-rt graphix-package \
    graphix-derive graphix-shell graphix-fuzz stdlib Cargo.toml -type f \
    \( -name "*.rs" -o -name "*.gx" -o -name "*.gxi" -o -name "*.toml" -o -name "*.sh" \) \
    ! -path "*/target/*" ! -name "#*" ! -name ".#*" \
    | LC_ALL=C sort | xargs sha256sum | sha256sum | cut -c1-16'

local_fingerprint() { bash -c "${FINGERPRINT/\~\/proj\/graphix/$repo}"; }
host_fingerprint()  { timeout 120 ssh "$1" "bash -c '$FINGERPRINT'" 2>/dev/null || echo unreachable; }

campaign_dir_of() { # host campaign -> remote path, home-relative for rsync
    printf 'tmp/target/fuzz/%s' "$2"
}

# ---------------------------------------------------------------- pull

pull() {
    local camp=$1 dest total=0 h name n
    [[ -n $camp ]] || usage
    dest="$repo/fuzz/pending-triage/$camp"
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h")
        skip_host "$name" && continue
        mkdir -p "$dest/$name"
        if rsync -a "$name:$(campaign_dir_of "$name" "$camp")/corpus/" \
                 "$dest/$name/" 2>/dev/null; then
            n=$(find "$dest/$name" -name '*.gx' | wc -l | tr -d ' ')
        else
            warn "$name: no campaign corpus for $camp (box unreachable, or never ran it)"
            n=0
        fi
        rmdir "$dest/$name" 2>/dev/null || true
        say "$(printf '%-8s %s divergences' "$name" "$n")"
        total=$((total + n))
    done
    say "pulled $total divergences into $dest"
}

# ---------------------------------------------------------------- stop

stop() {
    local camp=$1 h name os rc=0 left
    [[ -n $camp ]] || usage
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h"); os=$(f_os "$h")
        skip_host "$name" && continue
        if [[ $os == darwin ]]; then
            timeout 300 ssh "$name" bash -s "$camp" <<'EOF' || true
camp=$1
~/bin/soak-stop "$camp" 2>&1 | tail -2 || true
EOF
        else
            timeout 300 ssh "$name" bash -s "$camp" <<'EOF' || true
camp=$1
cd ~/proj/graphix && ./graphix-fuzz/soak.sh stop "$camp" 2>&1 | tail -2 || true
EOF
        fi
        # The stop is a CLAIM; this is the verification. katana's own
        # stop script lied for weeks while leaving ~70 orphans behind.
        left=$(timeout 120 ssh "$name" bash -s "$camp" <<'EOF' || echo unreachable
camp=$1
pkill -KILL -f "fuzz/$camp/graphix-fuzz" 2>/dev/null || true
sleep 2
pgrep -f "fuzz/$camp/graphix-fuzz" | wc -l | tr -d ' '
EOF
)
        if [[ $left == 0 ]]; then
            say "$(printf '%-8s stopped, 0 survivors' "$name")"
        else
            warn "$(printf '%-8s STOP FAILED: %s processes left' "$name" "$left")"
            rc=1
        fi
    done
    return $rc
}

# ---------------------------------------------------------------- sync

# THE DIRTY-TREE GUARD (2026-09-01). The loss model, lived once at the
# aug31f deploy: the local fingerprint is captured ONCE, so a commit
# made while the sync loop walked the fleet gave the later boxes a
# NEWER tree than `want` — each then sat the full SYNC_WAIT in silent
# 10s polls before a misleading per-box TREE STALE, and the fleet
# ended half old tree, half new. Two facts close both doors:
#   - at entry, the build inputs must be CLEAN in git (a fingerprinted
#     file that git doesn't have is a tree no redeploy can reproduce);
#     netidx ships too, so its build inputs are checked the same way
#     even though the fingerprint can't see them (a dirty netidx would
#     ship silently — worse than a stall);
#   - per box, the local fingerprint is re-read BEFORE the rsync and a
#     drift from `want` dies loudly at once, naming the real cause,
#     instead of stalling box after box against a reference that no
#     longer exists.
# FLEET_ALLOW_DIRTY=1 skips the entry check for a deliberate
# uncommitted-tree soak; nothing skips the drift check — a tree that
# changes MID-DEPLOY is never deliberate.
FP_ROOTS=(graphix-compiler graphix-rt graphix-package graphix-derive
          graphix-shell graphix-fuzz stdlib Cargo.toml)

require_clean_inputs() {
    [[ ${FLEET_ALLOW_DIRTY:-0} == 1 ]] && return 0
    local dirty
    dirty=$(git -C "$repo" status --porcelain -- "${FP_ROOTS[@]}")
    [[ -z $dirty ]] || die "refusing to sync: uncommitted build inputs in graphix \
(commit/stash, or FLEET_ALLOW_DIRTY=1 for a deliberate dirty soak):
$dirty"
    dirty=$(git -C "$repo/../netidx" status --porcelain -- \
        ':(glob)**/*.rs' ':(glob)**/*.toml' 2>/dev/null || true)
    [[ -z $dirty ]] || die "refusing to sync: uncommitted build inputs in ../netidx \
(commit/stash, or FLEET_ALLOW_DIRTY=1 for a deliberate dirty soak):
$dirty"
}

sync_tree() {
    local want now h name method got waited rc=0
    require_clean_inputs
    want=$(local_fingerprint)
    say "local build-input fingerprint: $want"
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h"); method=$(f_sync "$h")
        skip_host "$name" && continue
        now=$(local_fingerprint)
        [[ $now == "$want" ]] || die "local tree CHANGED during the sync \
($want -> $now) — the fleet would end half old, half new; commit and redeploy"
        if [[ $method == rsync ]]; then
            # Both repos: graphix builds against the sibling netidx.
            rsync -a --delete --exclude target --exclude .git \
                "$repo/" "$name:proj/graphix/"
            rsync -a --delete --exclude target --exclude .git \
                "$repo/../netidx/" "$name:proj/netidx/"
        fi
        waited=0
        while :; do
            got=$(host_fingerprint "$name")
            [[ $got == "$want" ]] && break
            (( waited >= SYNC_WAIT )) && break
            sleep 10; waited=$((waited + 10))
        done
        if [[ $got == "$want" ]]; then
            local after=""
            (( waited > 0 )) && after=" after ${waited}s"
            say "$(printf '%-8s tree verified (%s)%s' "$name" "$got" "$after")"
        else
            warn "$(printf '%-8s TREE STALE: %s != %s' "$name" "$got" "$want")"
            rc=1
        fi
    done
    return $rc
}

# -------------------------------------------------------------- launch

# Detached so an ssh drop cannot kill a build mid-flight, and every
# launch ENDS by printing a marker the verifier can key on — soak.sh
# itself prints lane lines, not a marker (a verifier keyed on a marker
# the launch never emits false-negatives a healthy box).
launch() {
    local camp=$1 base=$2 h name os workers scale seed i=0 asan
    [[ -n $camp && -n $base ]] || usage
    [[ $base =~ ^[0-9]+$ ]] || die "base-seed must be an unsigned integer"
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h"); os=$(f_os "$h")
        workers=$(f_workers "$h"); scale=$(f_scale "$h")
        seed=$((base + i * 10000000)); i=$((i + 1))
        skip_host "$name" && continue
        asan=0
        if asan_host "$name"; then
            [[ $os == darwin ]] && die \
                "FLEET_ASAN: $name is darwin — LSan is off on macOS and soak-start has no asan knob"
            asan=1
            workers=$((workers / 4)); ((workers >= 8)) || workers=8
            scale=$((scale * 2))
        fi
        say "$(printf '%-8s launching %s seed=%s workers=%s scale=%s%s' \
             "$name" "$camp" "$seed" "$workers" "$scale" \
             "$([[ $asan == 1 ]] && echo ' ASAN' || true)")"
        if [[ $os == darwin ]]; then
            timeout 120 ssh "$name" bash -s "$camp" "$seed" "$workers" "$scale" "$MIX" <<'EOF'
camp=$1; seed=$2; workers=$3; scale=$4; mix=$5
log=~/tmp/fleet-$camp-launch.log
nohup bash -lc "
    set -e
    df -h /Volumes/Games | tail -1
    cd ~/proj/graphix
    cargo build --release -p graphix-fuzz
    cp /Volumes/Games/cargo/release/graphix-fuzz ~/tmp/target/release/graphix-fuzz
    GRAPHIX_FUZZ_TIMEOUT_SCALE=$scale ~/bin/soak-start $camp $seed $workers $mix
    echo FLEET_LAUNCH_OK
" > "$log" 2>&1 < /dev/null &
disown || true
EOF
        else
            timeout 120 ssh "$name" bash -s "$camp" "$seed" "$workers" "$scale" "$MIX" "$asan" <<'EOF'
camp=$1; seed=$2; workers=$3; scale=$4; mix=$5; asan=$6
log=~/tmp/fleet-$camp-launch.log
setsid nohup bash -lc "
    set -e
    export PATH=\$HOME/.cargo/bin:\$PATH
    export GRAPHIX_FUZZ_TIMEOUT_SCALE=$scale
    export SOAK_ASAN=$asan
    cd ~/proj/graphix
    ./graphix-fuzz/soak.sh start $camp $workers $seed $mix
    echo FLEET_LAUNCH_OK
" > "$log" 2>&1 < /dev/null &
disown || true
EOF
        fi
    done
    say "launched; run 'fleet.sh verify $camp' (it waits for the builds)"
}

# -------------------------------------------------------------- verify

verify() {
    local camp=$1 h name want rc=0
    [[ -n $camp ]] || usage
    want=$(corpus_count)
    say "expected regression corpus: $want programs"
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h")
        skip_host "$name" && continue
        verify_host "$name" "$camp" "$want" || rc=1
    done
    if (( rc == 0 )); then
        say "fleet up: every box verified"
    else
        warn "DEPLOY DEGRADED — see the failures above"
    fi
    return $rc
}

verify_host() {
    local name=$1 camp=$2 want=$3 out gate n bad a b
    out=$(timeout $((LAUNCH_WAIT + 120)) ssh "$name" bash -s "$camp" "$LAUNCH_WAIT" <<'EOF' || echo "FLEET_UNREACHABLE"
camp=$1; budget=$2
log=~/tmp/fleet-$camp-launch.log
dir=~/tmp/target/fuzz/$camp
waited=0
while :; do
    grep -q FLEET_LAUNCH_OK "$log" 2>/dev/null && break
    [ "$waited" -ge "$budget" ] && { echo "FLEET_TIMEOUT after ${waited}s"; break; }
    sleep 10; waited=$((waited + 10))
done
# The broad failure grep, minus the two benign families: cargo progress
# and the campaign's own log-everywhere ERROR lines (pinned limit-class
# corpus programs log "exceeds ... limit" BY DESIGN, and that pattern
# once false-positived three healthy boxes as LAUNCH FAILED).
echo "=== FAILURES ==="
grep -aE "panicked|command not found|No such file|gate failed|failed to launch|error\[|error:|cannot find|Permission denied|not a directory" "$log" 2>/dev/null \
    | grep -av " ERROR graphix_" | grep -av "^warning:" | head -5 || true
echo "=== GATE ==="
grep -am1 "regression corpus:" "$log" "$dir/soak.log" 2>/dev/null | head -1 || true
echo "=== PROC ==="
pgrep -f "fuzz/$camp/graphix-fuzz" | wc -l | tr -d ' '
# Sample ONE source's counter twice: the log interleaves sources, so
# "the last line" can name a different source each time and appear to
# go backwards.
sample() { grep -aoE "^  fuzz…[0-9]+ run" "$dir/soak.log" 2>/dev/null | tail -1 || true; }
echo "=== COUNT1 ==="
sample
sleep 20
echo "=== COUNT2 ==="
sample
EOF
)
    if [[ $out == *FLEET_UNREACHABLE* ]]; then
        warn "$(printf '%-8s UNREACHABLE' "$name")"; return 1
    fi
    if [[ $out == *FLEET_TIMEOUT* ]]; then
        warn "$(printf '%-8s LAUNCH TIMED OUT (still building, or wedged)' "$name")"; return 1
    fi
    bad=$(sed -n '/=== FAILURES ===/,/=== GATE ===/p' <<<"$out" | grep -v '===' || true)
    gate=$(sed -n '/=== GATE ===/,/=== PROC ===/p' <<<"$out" | grep 'regression corpus' || true)
    n=$(sed -n '/=== PROC ===/,/=== COUNT1 ===/p' <<<"$out" | grep -E '^[0-9]+$' | head -1 || true)
    a=$(sed -n '/=== COUNT1 ===/,/=== COUNT2 ===/p' <<<"$out" | grep -oE '[0-9]+ run' | head -1 || true)
    b=$(sed -n '/=== COUNT2 ===/,$p'                  <<<"$out" | grep -oE '[0-9]+ run' | head -1 || true)
    local ok=0
    [[ -n $bad ]] && { warn "$(printf '%-8s LAUNCH LOG ERRORS:' "$name")"; warn "$bad"; ok=1; }
    if [[ $gate != *"corpus: $want programs, 0 regressions"* ]]; then
        warn "$(printf '%-8s GATE MISMATCH (want %s programs, 0 regressions): %s' \
             "$name" "$want" "${gate:-<no gate line>}")"
        ok=1
    fi
    [[ ${n:-0} -ge 1 ]] || { warn "$(printf '%-8s NO CAMPAIGN PROCESS' "$name")"; ok=1; }
    # A missing sample is not a pass: the SECOND one must exist and be
    # non-zero (proof the campaign checked something), and when both
    # exist the second must exceed the first.
    if [[ -z $b || ${b%% *} -eq 0 ]]; then
        warn "$(printf '%-8s NO SUBJECTS CHECKED (samples: %s then %s)' \
             "$name" "${a:-<none>}" "${b:-<none>}")"
        ok=1
    elif [[ -n $a && ${a%% *} -ge ${b%% *} ]]; then
        warn "$(printf '%-8s COUNTERS NOT ADVANCING (%s then %s)' "$name" "$a" "$b")"
        ok=1
    fi
    if (( ok == 0 )); then
        say "$(printf '%-8s UP  gate ok, %s procs, %s → %s' \
             "$name" "$n" "${a:-?}" "${b:-?}")"
    fi
    return $ok
}

# -------------------------------------------------------------- status

status() {
    local camp=${1:-} h name
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h")
        skip_host "$name" && continue
        say "=== $name ==="
        timeout 60 ssh "$name" bash -s "$camp" <<'EOF' || warn "  unreachable"
camp=$1
if [ -n "$camp" ]; then dirs=~/tmp/target/fuzz/$camp; else dirs=$(ls -d ~/tmp/target/fuzz/*/ 2>/dev/null); fi
for d in $dirs; do
    [ -d "$d" ] || continue
    n=$(find "$d/corpus" -name '*.gx' 2>/dev/null | wc -l | tr -d ' ')
    p=$(pgrep -f "$d/graphix-fuzz" | wc -l | tr -d ' ')
    printf '  %-28s %s procs, %s divergences\n' "$(basename "$d")" "$p" "$n"
    tail -3 "$d/soak.log" 2>/dev/null | sed 's/^/    /'
done
EOF
    done
}

# -------------------------------------------------------------- deploy

deploy() {
    local new=$1 base=$2 old=${3:-}
    [[ -n $new && -n $base ]] || usage
    if [[ -n $old ]]; then
        say "== pull $old =="; pull "$old"
        say "== stop $old ==";  stop "$old" || die "refusing to deploy over a fleet that would not stop"
    fi
    say "== sync ==";   sync_tree || die "refusing to launch on a stale tree"
    say "== launch =="; launch "$new" "$base"
    say "== verify =="; verify "$new"
}

cmd=${1:-}; shift || true
case $cmd in
    deploy) deploy "${1:-}" "${2:-}" "${3:-}" ;;
    pull)   pull "${1:-}" ;;
    stop)   stop "${1:-}" ;;
    sync)   sync_tree ;;
    launch) launch "${1:-}" "${2:-}" ;;
    verify) verify "${1:-}" ;;
    status) status "${1:-}" ;;
    hosts)  printf '%s\n' "${HOSTS[@]}" ;;
    *)      usage ;;
esac
