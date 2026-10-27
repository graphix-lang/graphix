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
#     not by rsync's exit status (a box's .git is never synced, so `git
#     log` there is a lie by construction)
#   - a launch is verified by the campaign's own startup gate line, whose
#     corpus count comes from pins EMBEDDED at build time, so a stale
#     binary shows up as a count mismatch

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)

# name : workers : timeout scale : os
# Seeds are allocated in THIS order, 10M apart from the base, so the
# order is load-bearing.
#
# Every box is pushed the tree by rsync and the fingerprint check gates
# the launch. washu-chan, the session box, reaches itself over ssh and
# joins a soak only when idle: launch it alone with FLEET_ONLY.
#
# Workers are sized by memory: a worker runs a subject's plain, cold and
# warm sessions and measures about 375MB on average and 575MB at the top,
# so a 62GB box takes 80 and katana (16GB) 32.
HOSTS=(
    "katana:32:4:darwin"
    "washu-chan:64:1:linux"
    "aieka:80:4:linux"
    "ryouko:80:1:linux"
)

MIX=${FLEET_MIX:-}                   # empty: the binary's DEFAULT_MIX
LAUNCH_WAIT=${FLEET_LAUNCH_WAIT:-5400} # seconds to wait for build+gate

f_name()    { echo "${1%%:*}"; }
f_workers() { echo "$1" | cut -d: -f2; }
f_scale()   { echo "$1" | cut -d: -f3; }
f_os()      { echo "$1" | cut -d: -f4; }

# FLEET_ONLY=<name> restricts every step to one host;
# FLEET_EXCLUDE=<name>[,<name>...] holds those hosts OUT. Both leave the
# seed math unchanged (each host keeps its position-derived seed).
# FLEET_ONLY adds a single box that has not run the campaign, without
# touching the rest (soak.sh start refuses an existing campaign
# directory, so a restart takes a new campaign name) — bring washu-chan
# into a running campaign:
# FLEET_ONLY=washu-chan fleet.sh launch <camp> <base>. FLEET_EXCLUDE runs a
# FULL deploy that must skip a box — hold the session box out of a launch
# while a session is live, then add it later with FLEET_ONLY:
# FLEET_EXCLUDE=washu-chan fleet.sh deploy <new> <base> <old>.
#
# FLEET_ASAN=<name>[,<name>...] switches those hosts' campaigns to
# AddressSanitizer binaries (soak.sh SOAK_ASAN=1: nightly build with an
# explicit --target; children run WITHOUT the address-space rlimit —
# ASan's ~20TB shadow reservation — under an RSS cap instead). The
# other knobs adjust automatically: workers/4 (ASan is ~3x RSS per
# child; floor 8) and timeout scale x2 (~2x slowdown). Darwin hosts
# refuse loudly (LSan is off on macOS).
# Composes with FLEET_ONLY/FLEET_EXCLUDE; the seed math is unchanged.
asan_host() {
    [[ -n ${FLEET_ASAN:-} && ",${FLEET_ASAN}," == *",$1,"* ]]
}

skip_host() {
    [[ -n ${FLEET_ONLY:-} && $FLEET_ONLY != "$1" ]] && return 0
    [[ -n ${FLEET_EXCLUDE:-} && ",${FLEET_EXCLUDE}," == *",$1,"* ]] && return 0
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
       fleet.sh sync                push the tree, verify by fingerprint
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
FINGERPRINT='cd ~/proj/graphix && find graphix-types graphix-compiler graphix-rt \
    graphix-package graphix-derive graphix-ast-pack graphix-shell graphix-fuzz stdlib \
    Cargo.toml Cargo.lock -type f \
    \( -name "*.rs" -o -name "*.gx" -o -name "*.gxi" -o -name "*.toml" -o -name "*.sh" \
       -o -name "*.manifest" -o -name Cargo.lock \) \
    ! -path "*/target/*" ! -name "#*" ! -name ".#*" \
    | LC_ALL=C sort | xargs sha256sum | sha256sum | cut -c1-16'

local_fingerprint() { bash -c "${FINGERPRINT/\~\/proj\/graphix/$repo}"; }
is_this_machine()   { [[ $(timeout 120 ssh "$1" hostname 2>/dev/null) == "$(hostname)" ]]; }
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

# `soak.sh stop` takes everything of a campaign on any host (a launch
# still building, the soak's process group, every process running the
# campaign's binary) and ends by counting what is left: the stop is a
# claim, the count its proof.
stop() {
    local camp=$1 h name rc=0 left
    [[ -n $camp ]] || usage
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h")
        skip_host "$name" && continue
        left=$(timeout 300 ssh "$name" bash -s "$camp" <<'EOF' || true
cd ~/proj/graphix && ./graphix-fuzz/soak.sh stop "$1" 2>&1 | tail -1
EOF
)
        left=$(sed -n 's/^stopped .*: \([0-9]*\) survivors$/\1/p' <<<"$left")
        if [[ $left == 0 ]]; then
            say "$(printf '%-8s stopped, 0 survivors' "$name")"
        else
            warn "$(printf '%-8s STOP FAILED: %s processes left' "$name" "${left:-unreachable}")"
            rc=1
        fi
    done
    return $rc
}

# ---------------------------------------------------------------- sync

# THE DIRTY-TREE GUARD. The local fingerprint is captured once, so a
# commit made while the sync loop walks the fleet would give the later
# boxes a newer tree than `want`, and the fleet would end half old, half
# new. Two facts close both doors:
#   - at entry, the build inputs must be CLEAN in git (a fingerprinted
#     file that git doesn't have is a tree no redeploy can reproduce);
#     netidx ships too, so its build inputs are checked the same way
#     even though the fingerprint can't see them;
#   - per box, the local fingerprint is re-read BEFORE the rsync and a
#     drift from `want` dies loudly at once, naming the real cause,
#     instead of stalling box after box against a reference that no
#     longer exists.
# FLEET_ALLOW_DIRTY=1 skips the entry check for a deliberate
# uncommitted-tree soak; nothing skips the drift check — a tree that
# changes MID-DEPLOY is never deliberate.
FP_ROOTS=(graphix-types graphix-compiler graphix-rt graphix-package graphix-derive
          graphix-ast-pack graphix-shell graphix-fuzz stdlib Cargo.toml Cargo.lock)

require_clean_inputs() {
    [[ ${FLEET_ALLOW_DIRTY:-0} == 1 ]] && return 0
    local dirty
    dirty=$(git -C "$repo" status --porcelain -- "${FP_ROOTS[@]}")
    [[ -z $dirty ]] || die "refusing to sync: uncommitted build inputs in graphix \
(commit/stash, or FLEET_ALLOW_DIRTY=1 for a deliberate dirty soak):
$dirty"
    local sib
    for sib in netidx; do
        dirty=$(git -C "$repo/../$sib" status --porcelain -- \
            ':(glob)**/*.rs' ':(glob)**/*.toml' 2>/dev/null || true)
        [[ -z $dirty ]] || die "refusing to sync: uncommitted build inputs in ../$sib \
(commit/stash, or FLEET_ALLOW_DIRTY=1 for a deliberate dirty soak):
$dirty"
    done
}

sync_tree() {
    local want now h name got rc=0
    require_clean_inputs
    want=$(local_fingerprint)
    say "local build-input fingerprint: $want"
    for h in "${HOSTS[@]}"; do
        name=$(f_name "$h")
        skip_host "$name" && continue
        now=$(local_fingerprint)
        [[ $now == "$want" ]] || die "local tree CHANGED during the sync \
($want -> $now) — the fleet would end half old, half new; commit and redeploy"
        # this machine's checkout is ~/proj/graphix itself: verified, never
        # written; graphix builds against the sibling netidx
        if ! is_this_machine "$name"; then
            # XCR claude for claude: [perf/low] This pushes the whole checkout, untracked
            # files included: today .claude/worktrees is 2.0 of the 3.3 GB synced, to
            # every box on every deploy, and none of it is a build input. Sync the
            # fingerprinted roots only (`--files-from` of `git ls-files --
            # "${FP_ROOTS[@]}"`), or exclude .claude and fuzz. Every host is rsync now,
            # so the syncthing machinery is dead: the SYNC_WAIT poll after a synchronous
            # rsync waits up to 600 s for a mismatch that cannot heal. Drop it and the
            # syncthing/hz0 prose, and fail the box at once. Found by measuring the
            # tree; the deploy was not run. (fuzz-main-aux-05)
            # 2026-10-26 claude: sync_tree pushes the tree without /.claude, /fuzz
            # and /vendor (graphix; vendor.py's 1.2 GB output, not a soak build
            # input) and /.claude (netidx); what is left untracked is under 200 MB;
            # the syncthing machinery is gone (the sync column of HOSTS, SYNC_WAIT
            # and its poll, the syncthing/hz0 prose): a box is pushed once,
            # fingerprinted once and fails at once on a mismatch. A whole-repo
            # --files-from of git ls-files was not taken: --delete does not reach a
            # file list, and the workspace needs graphix-lsp and
            # ide/tree-sitter-graphix, which the fingerprint roots leave out (so the
            # fingerprint misses them too; not changed here). No pin: a deploy
            # script; bash -n only, the deploy was not run.
            rsync -a --delete --exclude target --exclude .git --exclude /.claude \
                --exclude /fuzz --exclude /vendor "$repo/" "$name:proj/graphix/"
            rsync -a --delete --exclude target --exclude .git --exclude /.claude \
                "$repo/../netidx/" "$name:proj/netidx/"
        fi
        got=$(host_fingerprint "$name")
        if [[ $got == "$want" ]]; then
            say "$(printf '%-8s tree verified (%s)' "$name" "$got")"
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
    local camp=$1 base=$2 h name os workers scale seed i=0 asan rc=0
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
                "FLEET_ASAN: $name is darwin — LSan is off on macOS"
            asan=1
            workers=$((workers / 4)); ((workers >= 8)) || workers=8
            scale=$((scale * 2))
        fi
        say "$(printf '%-8s launching %s seed=%s workers=%s scale=%s%s' \
             "$name" "$camp" "$seed" "$workers" "$scale" \
             "$([[ $asan == 1 ]] && echo ' ASAN' || true)")"
        timeout 120 ssh "$name" bash -s "$camp" "$seed" "$workers" "$scale" "$asan" $MIX <<'EOF' \
            || { warn "$(printf '%-8s LAUNCH FAILED' "$name")"; rc=1; }
camp=$1; seed=$2; workers=$3; scale=$4; asan=$5; mix=${6:-}
log=~/tmp/fleet-$camp-launch.log
perl -MPOSIX -e 'exit 0 if fork; POSIX::setsid() or die "setsid: $!"; exec @ARGV or die "exec: $!"' \
    bash -lc "
    trap 'rc=\$?; [ \$rc -eq 0 ] || echo FLEET_LAUNCH_FAILED rc=\$rc' EXIT
    set -e
    export PATH=\$HOME/.cargo/bin:\$PATH
    export GRAPHIX_FUZZ_TIMEOUT_SCALE=$scale
    export SOAK_ASAN=$asan
    cd ~/proj/graphix
    ./graphix-fuzz/soak.sh start $camp $workers $seed $mix
    echo FLEET_LAUNCH_OK
" > "$log" 2>&1 < /dev/null
EOF
    done
    say "launched; run 'fleet.sh verify $camp' (it waits for the builds)"
    return $rc
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
    # the remote waits LAUNCH_WAIT for the launch, then up to 600s for
    # the counters
    out=$(timeout $((LAUNCH_WAIT + 720)) ssh "$name" bash -s "$camp" "$LAUNCH_WAIT" <<'EOF' || echo "FLEET_UNREACHABLE"
camp=$1; budget=$2
log=~/tmp/fleet-$camp-launch.log
dir=~/tmp/target/fuzz/$camp
waited=0
while :; do
    grep -q FLEET_LAUNCH_OK "$log" 2>/dev/null && break
    if grep -q FLEET_LAUNCH_FAILED "$log" 2>/dev/null; then
        echo "FLEET_LAUNCHER_EXITED"; tail -5 "$log"; exit 0
    fi
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
~/proj/graphix/graphix-fuzz/soak.sh procs "$camp" 2>/dev/null || echo 0
# A sample is the sum of every source's last counter: the log
# interleaves sources, so "the last line" can name a different source
# each time and appear to go backwards, and a mix may give any one source
# no share. A source's counter line lands every 1000 subjects and at
# least every 30 s of batches completing, a while after the gate and
# minutes apart right after launch (a fresh JIT, six
# sessions per subject), so the first sample waits for a line and the
# second waits for a DIFFERENT sum; only five quiet minutes is a stall.
sample() {
    grep -aoE "^  [a-z]+…[0-9]+ run" "$dir/soak.log" 2>/dev/null |
        awk -F'…' '{ last[$1] = $2 + 0; seen = 1 }
                   END { if (seen) { for (k in last) s += last[k]; print s " run" } }' || true
}
waited=0
while [ -z "$(sample)" ] && [ "$waited" -lt 300 ]; do sleep 10; waited=$((waited + 10)); done
echo "=== COUNT1 ==="
first=$(sample)
echo "$first"
waited=0
while [ "$(sample)" = "$first" ] && [ "$waited" -lt 300 ]; do sleep 10; waited=$((waited + 10)); done
echo "=== COUNT2 ==="
sample
EOF
)
    if [[ $out == *FLEET_UNREACHABLE* ]]; then
        warn "$(printf '%-8s UNREACHABLE' "$name")"; return 1
    fi
    if [[ $out == *FLEET_LAUNCHER_EXITED* ]]; then
        warn "$(printf '%-8s LAUNCH FAILED:' "$name")"
        warn "$(sed -n '/FLEET_LAUNCHER_EXITED/,$p' <<<"$out" | tail -n +2)"
        return 1
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
    # XCR claude for claude: [bug/low] The remote sample() counts only the `fuzz…`
    # counter line. run_aggregator never launches a source with a zero share, and the
    # mix allows one, so FLEET_MIX=0:50:50:10 fails every healthy box with NO SUBJECTS
    # CHECKED. Sample every source's `<tag>…N run` line and sum the counts, so any
    # source that checks something counts as progress. Found by reading; not run.
    # (fuzz-main-aux-04)
    # 2026-10-26 claude: verify_host's remote sample() sums every source's last
    # counter (awk keyed by the tag before the ellipsis), printing nothing until some
    # source logged a line, so a mix with a zero share for any source, fuzz included,
    # still counts progress and the sum only grows. No pin: a deploy script; checked
    # by hand over a fake soak.log (generate 1000 then 2000 plus reactive 500 gives
    # 2500 run, an empty log gives nothing); the deploy was not run.
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
    d=${d%/}
    [ -d "$d" ] || continue
    n=$(find "$d/corpus" -name '*.gx' 2>/dev/null | wc -l | tr -d ' ')
    p=$(~/proj/graphix/graphix-fuzz/soak.sh procs "$(basename "$d")" 2>/dev/null || echo 0)
    printf '  %-28s %s procs, %s divergences\n' "$(basename "$d")" "$p" "$n"
    tail -3 "$d/soak.log" 2>/dev/null | sed 's/^/    /'
done
EOF
    done
}

# -------------------------------------------------------------- deploy

# The sync precedes the stop: the stop is the synced tree's `soak.sh
# stop`, and a running campaign runs its own copy of the binary.
deploy() {
    local new=$1 base=$2 old=${3:-}
    [[ -n $new && -n $base ]] || usage
    [[ -n $old ]] && { say "== pull $old =="; pull "$old"; }
    say "== sync ==";   sync_tree || die "refusing to launch on a stale tree"
    if [[ -n $old ]]; then
        say "== stop $old =="
        stop "$old" || die "refusing to deploy over a fleet that would not stop"
    fi
    say "== launch =="; launch "$new" "$base" || warn "some launches failed; verifying the rest"
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
