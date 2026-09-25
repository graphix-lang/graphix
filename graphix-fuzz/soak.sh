#!/usr/bin/env bash

# Launch/stop/inspect a soak campaign, on Linux and macOS alike.
#
# ONE process, not one per source. The campaign's four work sources
# (corpus mutation, generated programs, generated scheduled programs,
# typemorph acceptance probes) share a single pool that divides the box
# by MEASURED CPU — see `soak` in main.rs. Three separate lane processes
# could only divide a box through the OS scheduler, which arbitrates
# between runnable processes, so equal worker counts bought wildly
# unequal CPU: measured 13/19/66 on a three-lane box, the reactive lane
# taking two thirds while looking evenly provisioned. `workers` is the
# whole box's in-flight checks and `mix` is where their CPU goes.
#
# A campaign is its binary's processes: the soak leads its own process
# group, and every child runs the campaign's copy of the binary (Linux
# spawns them as /proc/self/exe, so no argv pattern names them — they
# are found by executable).

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)
fuzz_root="${GRAPHIX_FUZZ_TARGET:-"$HOME/tmp/target"}/fuzz"
nice_level=${GRAPHIX_FUZZ_NICE:-19}

usage() {
    echo "usage: $0 start <campaign> [workers] [base-seed] [fuzz:generate:reactive:typemorph]" >&2
    echo "       $0 stop <campaign>" >&2
    echo "       $0 status <campaign>" >&2
    exit 2
}

ncpu() { getconf _NPROCESSORS_ONLN; }

campaign_dir() {
    local campaign=$1
    [[ $campaign =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || {
        echo "invalid campaign name: $campaign" >&2
        exit 2
    }
    printf '%s/%s\n' "$fuzz_root" "$campaign"
}

soak_pid() {
    local dir=$1 pidfile="$dir/state/soak.pid"
    [[ -r $pidfile ]] || return 1
    local pid
    read -r pid < "$pidfile"
    [[ $pid =~ ^[0-9]+$ ]] || return 1
    printf '%s\n' "$pid"
}

# Every process whose executable is the campaign's binary (a deleted
# binary reads as "<path> (deleted)" on Linux, hence the prefix match).
campaign_procs() {
    local bin=$1/graphix-fuzz
    if [[ -d /proc/self ]]; then
        find /proc -mindepth 2 -maxdepth 2 -name exe -lname "$bin*" 2>/dev/null |
            cut -d/ -f3
    else
        ps -axo pid=,comm= | awk -v bin="$bin" 'index($2, bin) == 1 { print $1 }'
    fi
}

# Not `campaign_procs | grep -q`: grep's early exit kills the writer and
# pipefail reads that as a dead soak.
soak_live() {
    local dir=$1 pid
    pid=$(soak_pid "$dir") || return 1
    grep -qx "$pid" <<<"$(campaign_procs "$dir")"
}

campaign_live() {
    [[ -n $(campaign_procs "$1") ]]
}

# The launcher of a campaign still building or gating: `soak.sh start`
# and whatever it runs (cargo, the regress gate) share its process group.
# Matched as the script itself, run directly or by a shell, never as a
# command line that merely mentions it.
launcher_groups() {
    local campaign=$1 own pid pgid
    local script="^(([^ ]*/)?(ba)?sh )?[^ ]*soak\\.sh start $campaign( |$)"
    own=$(ps -o pgid= -p $$ | tr -d ' ')
    for pid in $(pgrep -f "$script" || true); do
        pgid=$(ps -o pgid= -p "$pid" | tr -d ' ')
        [[ -n $pgid && $pgid != "$own" ]] && echo "$pgid"
    done | sort -u
}

stop_campaign() {
    local dir=$1 pid
    if pid=$(soak_pid "$dir"); then
        kill -TERM -- "-$pid" 2>/dev/null || true
    fi
    for _ in {1..100}; do
        campaign_live "$dir" || return 0
        sleep 0.1
    done
    [[ -n ${pid:-} ]] && kill -KILL -- "-$pid" 2>/dev/null || true
    for pid in $(campaign_procs "$dir"); do
        kill -KILL "$pid" 2>/dev/null || true
    done
}

# setsid(1) is util-linux; perl's is on every box the fleet runs.
detach() {
    perl -MPOSIX -e 'exit 0 if fork; POSIX::setsid() or die "setsid: $!"; exec @ARGV or die "exec: $!"' "$@"
}

launch() {
    local dir=$1 seed=$2 workers=$3 mix=$4
    local pidfile="$dir/state/soak.pid"
    detach /bin/sh -c '
        pidfile=$1
        nice_level=$2
        workers=$3
        corpus=$4
        binary=$5
        shift 5
        printf "%s\n" "$$" > "$pidfile"
        renice -n "$nice_level" -p "$$" >/dev/null
        exec env \
            GRAPHIX_FUZZ_PAR="$workers" \
            GRAPHIX_FUZZ_CORPUS="$corpus" \
            "$binary" "$@"
    ' soak-lane "$pidfile" "$nice_level" "$workers" "$dir/corpus" \
        "$dir/graphix-fuzz" soak forever "$seed" "$mix" > "$dir/soak.log" 2>&1 < /dev/null
    for _ in {1..100}; do
        soak_live "$dir" && return 0
        sleep 0.1
    done
    echo "soak failed to launch" >&2
    tail -n 20 "$dir/soak.log" >&2 || true
    return 1
}

# The campaign must clear its own regression corpus before it is allowed
# to hunt: a build that broke a fixed bug would otherwise spend the night
# re-finding it.
wait_for_gate() {
    local dir=$1 log="$dir/soak.log"
    # The launched soak re-runs the full regression gate before it
    # hunts: a parallel pass, then every non-ran agreement (refusal,
    # limit and error pins) again sequentially at 4x budget, so the
    # gate's wall time is a few minutes at scale 1 and grows with the
    # corpus and with GRAPHIX_FUZZ_TIMEOUT_SCALE. The wait is the
    # scaled gate with generous headroom: giving up early kills a
    # healthy soak (aug27a aieka at 114s, sep11a ryouko at 120s).
    local scale=${GRAPHIX_FUZZ_TIMEOUT_SCALE:-1}
    [[ $scale =~ ^[0-9]+$ ]] && ((scale >= 1)) || scale=1
    local max=$(( 3000 * scale ))
    local _i
    for ((_i = 0; _i < max; _i++)); do
        soak_live "$dir" || {
            echo "soak exited during its startup gate" >&2
            tail -n 20 "$log" >&2 || true
            return 1
        }
        local result
        result=$(grep -m1 '^regression corpus:' "$log" || true)
        if [[ -n $result ]]; then
            [[ $result == *', 0 regressions' ]] || {
                echo "startup gate failed: $result" >&2
                tail -n 20 "$log" >&2 || true
                return 1
            }
            if grep -q '^soak: iters=forever ' "$log"; then
                echo "soak: $result"
                return 0
            fi
        fi
        sleep 0.2
    done
    echo "startup gate timed out" >&2
    tail -n 20 "$log" >&2 || true
    return 1
}

verify_campaign() {
    local dir=$1 pid ni
    soak_live "$dir" || {
        echo "soak is not running" >&2
        return 1
    }
    pid=$(soak_pid "$dir")
    for ni in $(ps -o ni= -p "$(pgrep -g "$pid" | paste -sd, -)"); do
        [[ $ni == - ]] && continue
        [[ $ni == "$nice_level" ]] || {
            echo "soak group $pid contains process at nice $ni" >&2
            return 1
        }
    done
}

start() {
    [[ $# -ge 1 && $# -le 4 ]] || usage
    local campaign=$1 dir workers seed mix
    dir=$(campaign_dir "$campaign")
    [[ ! -e $dir ]] || {
        echo "campaign directory already exists: $dir" >&2
        exit 1
    }
    # The WHOLE box: one pool, so the 8x oversubscription is claimed once.
    workers=${2:-$(( $(ncpu) * 8 ))}
    seed=${3:-$(date +%s)}
    mix=${4:-50:25:25:10}
    [[ $workers =~ ^[1-9][0-9]*$ ]] || {
        echo "workers must be positive" >&2
        exit 2
    }
    # A seed passed in the workers position launches billions of
    # children and OOM-kills the box (it happened — twice, 2026-07-19).
    (( workers <= $(ncpu) * 16 )) || {
        echo "workers $workers exceeds $(ncpu)*16 — arguments are" \
             "<campaign> [workers] [base-seed] [mix]; did you pass the" \
             "seed as workers?" >&2
        exit 2
    }
    [[ $seed =~ ^[0-9]+$ ]] || {
        echo "base-seed must be an unsigned integer" >&2
        exit 2
    }
    [[ $mix =~ ^[0-9]+(\.[0-9]+)?(:[0-9]+(\.[0-9]+)?){3}$ ]] || {
        echo "mix must be fuzz:generate:reactive:typemorph, e.g. 50:25:25:10" >&2
        exit 2
    }
    [[ $nice_level =~ ^-?[0-9]+$ ]] && ((nice_level >= -20 && nice_level <= 19)) || {
        echo "GRAPHIX_FUZZ_NICE must be between -20 and 19" >&2
        exit 2
    }

    # Every in-flight check holds several descriptors (three pipes and a
    # verdict file), so the default 1024 (256 on macOS) caps `par` — and
    # the campaign does not degrade at the cap, it DIES: the harness
    # treats a spawn error as broken-environment and aborts, which it did
    # right after passing its gate ("child spawn failed: Too many open
    # files").
    ulimit -n 10240 2>/dev/null || true

    # Built where this box's cargo builds (~/tmp/target on the Linux
    # boxes, /Volumes/Games/cargo on katana).
    local build binary
    build=$(cargo metadata --no-deps --format-version 1 --manifest-path "$repo/Cargo.toml" |
        sed -n 's/.*"target_directory":"\([^"]*\)".*/\1/p')
    [[ -n $build ]] || {
        echo "cannot find cargo's target directory" >&2
        exit 2
    }
    # SOAK_ASAN=1 (fleet.sh FLEET_ASAN): build the campaign binary under
    # AddressSanitizer — nightly plus an explicit --target so host
    # proc-macros/build scripts stay uninstrumented (E0463 otherwise).
    # Children must run WITHOUT the address-space rlimit (ASan RESERVES
    # ~20TB of shadow VA, so RLIMIT_AS kills them at spawn); the RSS cap
    # is the containment instead. Reports ride child stderr into the
    # harness's crash findings; LSan runs at each child exit.
    if [[ ${SOAK_ASAN:-0} == 1 ]]; then
        [[ $(uname -s) == Linux ]] || {
            echo "SOAK_ASAN=1 is Linux only: LSan is off on macOS" >&2
            exit 2
        }
        grep -q '^nightly' <<<"$(rustup toolchain list 2>/dev/null)" || {
            echo "SOAK_ASAN=1 needs a rustup nightly toolchain" >&2
            exit 2
        }
        local triple
        triple=$(rustc -vV | awk '/^host:/{print $2}')
        RUSTFLAGS="-Zsanitizer=address" \
            cargo +nightly build --release --target "$triple" -p graphix-fuzz \
            --manifest-path "$repo/Cargo.toml"
        binary="$build/$triple/release/graphix-fuzz"
        export GRAPHIX_FUZZ_MEM_LIMIT=0
        export ASAN_OPTIONS="hard_rss_limit_mb=${SOAK_ASAN_RSS_MB:-2048}"
    else
        cargo build --release -p graphix-fuzz --manifest-path "$repo/Cargo.toml"
        binary="$build/release/graphix-fuzz"
    fi
    "$binary" regress

    mkdir -p "$dir/state" "$dir/corpus"
    cp "$binary" "$dir/graphix-fuzz"
    printf 'workers=%s\nmix=%s\nnice=%s\nbase_seed=%s\nasan=%s\n' \
        "$workers" "$mix" "$nice_level" "$seed" "${SOAK_ASAN:-0}" \
        > "$dir/state/config"

    trap 'stop_campaign "$dir"' ERR INT TERM
    launch "$dir" "$seed" "$workers" "$mix"
    wait_for_gate "$dir"
    verify_campaign "$dir"
    trap - ERR INT TERM
    status "$campaign"
}

# Stops everything of the campaign, a launch still in progress included,
# then counts what is left: the stop is a claim, the count its proof
# (katana's old stop script printed success over ~70 orphans for weeks).
stop() {
    [[ $# == 1 ]] || usage
    local campaign=$1 dir pgid left
    dir=$(campaign_dir "$campaign")
    for pgid in $(launcher_groups "$campaign"); do
        kill -KILL -- "-$pgid" 2>/dev/null || true
    done
    stop_campaign "$dir"
    sleep 1
    left=$(( $(campaign_procs "$dir" | wc -l) + $(launcher_groups "$campaign" | wc -l) ))
    echo "stopped $campaign: $left survivors"
    (( left == 0 ))
}

status() {
    [[ $# == 1 ]] || usage
    local campaign=$1 dir pid
    dir=$(campaign_dir "$campaign")
    [[ -d $dir ]] || {
        echo "campaign does not exist: $dir" >&2
        exit 1
    }
    echo "$dir"
    if soak_live "$dir"; then
        pid=$(soak_pid "$dir")
        ps -p "$pid" -o pid=,pgid=,ni=,stat=,etime=,args= | sed 's/^/soak: /'
        # The per-source CPU split, which is the number the mix controls.
        # A soak seconds old has logged no counter line yet, and under
        # `pipefail` that empty grep failed the whole pipeline — so
        # `start` exited non-zero on a launch that was perfectly healthy,
        # which is the one signal a deploy verifier must be able to trust.
        grep -aoE '^  [a-z]*….*% cpu' "$dir/soak.log" 2>/dev/null |
            awk -F'…' '{ last[$1] = $0 } END { for (k in last) print "  " last[k] }' |
            sort || true
    elif campaign_live "$dir"; then
        echo "soak: $(campaign_procs "$dir" | wc -l | tr -d ' ') orphaned processes"
    else
        echo "soak: stopped"
    fi
}

command=${1:-}
shift || true
case $command in
    start) start "$@" ;;
    stop) stop "$@" ;;
    status) status "$@" ;;
    *) usage ;;
esac
