#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)
target=${GRAPHIX_FUZZ_TARGET:-"$HOME/tmp/target"}
fuzz_root="$target/fuzz"
binary="$target/release/graphix-fuzz"
nice_level=${GRAPHIX_FUZZ_NICE:-19}

usage() {
    echo "usage: $0 start <campaign> [workers-per-lane] [base-seed]" >&2
    echo "       $0 stop <campaign>" >&2
    echo "       $0 status <campaign>" >&2
    exit 2
}

campaign_dir() {
    local campaign=$1
    [[ $campaign =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || {
        echo "invalid campaign name: $campaign" >&2
        exit 2
    }
    printf '%s/%s\n' "$fuzz_root" "$campaign"
}

lane_pid() {
    local dir=$1 lane=$2 pidfile
    pidfile="$dir/state/$lane.pid"
    [[ -r $pidfile ]] || return 1
    local pid
    read -r pid < "$pidfile"
    [[ $pid =~ ^[0-9]+$ ]] || return 1
    printf '%s\n' "$pid"
}

lane_live() {
    local dir=$1 lane=$2 pid exe
    pid=$(lane_pid "$dir" "$lane") || return 1
    [[ -e /proc/$pid/exe ]] || return 1
    exe=$(readlink -f "/proc/$pid/exe") || return 1
    [[ $exe == "$dir/graphix-fuzz" ]]
}

session_live() {
    local dir=$1 lane=$2 pid process exe
    pid=$(lane_pid "$dir" "$lane") || return 1
    while read -r process; do
        [[ -e /proc/$process/exe ]] || continue
        exe=$(readlink -f "/proc/$process/exe") || continue
        [[ $exe == "$dir/graphix-fuzz" ]] && return 0
    done < <(ps --sid "$pid" -o pid= 2>/dev/null)
    return 1
}

stop_session() {
    local dir=$1 lane=$2 signal=$3 pid
    session_live "$dir" "$lane" || return 0
    pid=$(lane_pid "$dir" "$lane")
    /usr/bin/pkill "-$signal" -s "$pid" 2>/dev/null || true
}

stop_campaign() {
    local dir=$1
    for lane in fuzz generate reactive; do
        stop_session "$dir" "$lane" TERM
    done
    for _ in {1..100}; do
        local live=0
        for lane in fuzz generate reactive; do
            session_live "$dir" "$lane" && live=1
        done
        ((live == 0)) && return 0
        sleep 0.1
    done
    for lane in fuzz generate reactive; do
        stop_session "$dir" "$lane" KILL
    done
}

launch_lane() {
    local dir=$1 lane=$2 mode=$3 seed=$4 reactive=$5 workers=$6
    local pidfile="$dir/state/$lane.pid"
    local log="$dir/$lane.log"
    local corpus="$dir/$lane"
    local -a args=("$mode" forever "$seed")
    [[ $reactive == true ]] && args+=(--reactive)
    /usr/bin/setsid --fork /bin/sh -c '
        pidfile=$1
        nice_level=$2
        workers=$3
        corpus=$4
        binary=$5
        shift 5
        printf "%s\n" "$$" > "$pidfile"
        /usr/bin/renice -n "$nice_level" -p "$$" >/dev/null
        exec /usr/bin/env \
            GRAPHIX_FUZZ_PAR="$workers" \
            GRAPHIX_FUZZ_CORPUS="$corpus" \
            "$binary" "$@"
    ' soak-lane "$pidfile" "$nice_level" "$workers" "$corpus" \
        "$dir/graphix-fuzz" "${args[@]}" > "$log" 2>&1
    for _ in {1..100}; do
        [[ -s $pidfile ]] && break
        sleep 0.1
    done
    lane_live "$dir" "$lane" || {
        echo "$lane failed to launch" >&2
        tail -n 20 "$log" >&2 || true
        return 1
    }
}

wait_for_gate() {
    local dir=$1 lane=$2 mode=$3 log
    log="$dir/$lane.log"
    for _ in {1..600}; do
        lane_live "$dir" "$lane" || {
            echo "$lane exited during its startup gate" >&2
            tail -n 20 "$log" >&2 || true
            return 1
        }
        local result
        result=$(grep -m1 '^regression corpus:' "$log" || true)
        if [[ -n $result ]]; then
            [[ $result == *', 0 regressions' ]] || {
                echo "$lane startup gate failed: $result" >&2
                tail -n 20 "$log" >&2 || true
                return 1
            }
            if grep -q "^$mode: iters=forever " "$log"; then
                echo "$lane: $result"
                return 0
            fi
        fi
        sleep 0.2
    done
    echo "$lane startup gate timed out" >&2
    tail -n 20 "$log" >&2 || true
    return 1
}

signal_lane() {
    local dir=$1 lane=$2 signal=$3 pid
    pid=$(lane_pid "$dir" "$lane")
    /usr/bin/kill "-$signal" -- "-$pid"
}

verify_campaign() {
    local dir=$1 lane pid ni
    for lane in fuzz generate reactive; do
        lane_live "$dir" "$lane" || {
            echo "$lane is not running" >&2
            return 1
        }
        pid=$(lane_pid "$dir" "$lane")
        while read -r ni; do
            [[ $ni == "$nice_level" ]] || {
                echo "$lane session $pid contains process at nice $ni" >&2
                return 1
            }
        done < <(ps --sid "$pid" -o ni=)
    done
}

start() {
    [[ $# -ge 1 && $# -le 3 ]] || usage
    local campaign=$1 dir workers seed
    dir=$(campaign_dir "$campaign")
    [[ ! -e $dir ]] || {
        echo "campaign directory already exists: $dir" >&2
        exit 1
    }
    workers=${2:-$(( $(nproc) * 8 / 3 ))}
    seed=${3:-$(date +%s)}
    [[ $workers =~ ^[1-9][0-9]*$ ]] || {
        echo "workers-per-lane must be positive" >&2
        exit 2
    }
    # A seed passed in the workers position launches billions of
    # children and OOM-kills the box (it happened — twice, 2026-07-19).
    (( workers <= $(nproc) * 8 )) || {
        echo "workers-per-lane $workers exceeds $(nproc)*8 — arguments are" \
             "<campaign> [workers-per-lane] [base-seed]; did you pass the" \
             "seed as workers?" >&2
        exit 2
    }
    [[ $seed =~ ^[0-9]+$ ]] || {
        echo "base-seed must be an unsigned integer" >&2
        exit 2
    }
    [[ $nice_level =~ ^-?[0-9]+$ ]] && ((nice_level >= -20 && nice_level <= 19)) || {
        echo "GRAPHIX_FUZZ_NICE must be between -20 and 19" >&2
        exit 2
    }

    CARGO_TARGET_DIR="$target" cargo build --release -p graphix-fuzz \
        --manifest-path "$repo/Cargo.toml"
    "$binary" regress

    mkdir -p "$dir/state" "$dir/fuzz" "$dir/generate" "$dir/reactive"
    cp "$binary" "$dir/graphix-fuzz"
    printf 'workers=%s\nnice=%s\nbase_seed=%s\n' \
        "$workers" "$nice_level" "$seed" > "$dir/state/config"

    trap 'stop_campaign "$dir"' ERR INT TERM
    launch_lane "$dir" fuzz fuzz "$seed" false "$workers"
    wait_for_gate "$dir" fuzz fuzz
    signal_lane "$dir" fuzz STOP

    launch_lane "$dir" generate generate "$((seed + 1000))" false "$workers"
    wait_for_gate "$dir" generate generate
    signal_lane "$dir" generate STOP

    launch_lane "$dir" reactive generate "$((seed + 2000))" true "$workers"
    wait_for_gate "$dir" reactive generate

    signal_lane "$dir" fuzz CONT
    signal_lane "$dir" generate CONT
    verify_campaign "$dir"
    trap - ERR INT TERM
    status "$campaign"
}

stop() {
    [[ $# == 1 ]] || usage
    local campaign=$1 dir
    dir=$(campaign_dir "$campaign")
    [[ -d $dir ]] || {
        echo "campaign does not exist: $dir" >&2
        exit 1
    }
    stop_campaign "$dir"
    status "$campaign"
}

status() {
    [[ $# == 1 ]] || usage
    local campaign=$1 dir lane pid
    dir=$(campaign_dir "$campaign")
    [[ -d $dir ]] || {
        echo "campaign does not exist: $dir" >&2
        exit 1
    }
    echo "$dir"
    for lane in fuzz generate reactive; do
        if lane_live "$dir" "$lane"; then
            pid=$(lane_pid "$dir" "$lane")
            ps -p "$pid" -o pid=,sid=,ni=,stat=,etime=,cmd= | \
                sed "s/^/$lane: /"
        elif session_live "$dir" "$lane"; then
            pid=$(lane_pid "$dir" "$lane")
            echo "$lane: orphaned session $pid"
        else
            echo "$lane: stopped"
        fi
    done
}

command=${1:-}
shift || true
case $command in
    start) start "$@" ;;
    stop) stop "$@" ;;
    status) status "$@" ;;
    *) usage ;;
esac
