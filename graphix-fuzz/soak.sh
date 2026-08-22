#!/usr/bin/env bash

# Launch/stop/inspect a soak campaign.
#
# ONE process, not three. The campaign's three work sources (corpus
# mutation, generated programs, generated scheduled programs) share a
# single pool that divides the box by MEASURED CPU — see `soak` in
# main.rs. Three separate lane processes could only divide a box through
# the OS scheduler, which arbitrates between runnable processes, so equal
# worker counts bought wildly unequal CPU: measured 13/19/66 on a
# three-lane box, the reactive lane taking two thirds while looking
# evenly provisioned. `workers` is now the whole box's in-flight checks
# and `mix` is where their CPU goes.

set -euo pipefail

if [[ $(uname -s) != Linux ]]; then
    echo "soak.sh requires Linux (nproc, setsid, /proc)." >&2
    echo "On macOS (katana): build, cp the binary to" >&2
    echo "~/tmp/target/release/, then ~/bin/soak-start <campaign>" >&2
    echo "<base-seed> [workers] [mix] — note the different arg order." >&2
    exit 2
fi

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)
target=${GRAPHIX_FUZZ_TARGET:-"$HOME/tmp/target"}
fuzz_root="$target/fuzz"
binary="$target/release/graphix-fuzz"
nice_level=${GRAPHIX_FUZZ_NICE:-19}

usage() {
    echo "usage: $0 start <campaign> [workers] [base-seed] [fuzz:generate:reactive]" >&2
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

soak_pid() {
    local dir=$1 pidfile="$dir/state/soak.pid"
    [[ -r $pidfile ]] || return 1
    local pid
    read -r pid < "$pidfile"
    [[ $pid =~ ^[0-9]+$ ]] || return 1
    printf '%s\n' "$pid"
}

soak_live() {
    local dir=$1 pid exe
    pid=$(soak_pid "$dir") || return 1
    [[ -e /proc/$pid/exe ]] || return 1
    exe=$(readlink -f "/proc/$pid/exe") || return 1
    [[ $exe == "$dir/graphix-fuzz" ]]
}

session_live() {
    local dir=$1 pid process exe
    pid=$(soak_pid "$dir") || return 1
    while read -r process; do
        [[ -e /proc/$process/exe ]] || continue
        exe=$(readlink -f "/proc/$process/exe") || continue
        [[ $exe == "$dir/graphix-fuzz" ]] && return 0
    done < <(ps --sid "$pid" -o pid= 2>/dev/null)
    return 1
}

stop_campaign() {
    local dir=$1 pid
    session_live "$dir" || return 0
    pid=$(soak_pid "$dir")
    /usr/bin/pkill -TERM -s "$pid" 2>/dev/null || true
    for _ in {1..100}; do
        session_live "$dir" || return 0
        sleep 0.1
    done
    /usr/bin/pkill -KILL -s "$pid" 2>/dev/null || true
}

launch() {
    local dir=$1 seed=$2 workers=$3 mix=$4
    local pidfile="$dir/state/soak.pid"
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
    ' soak-lane "$pidfile" "$nice_level" "$workers" "$dir/corpus" \
        "$dir/graphix-fuzz" soak forever "$seed" "$mix" > "$dir/soak.log" 2>&1
    for _ in {1..100}; do
        [[ -s $pidfile ]] && break
        sleep 0.1
    done
    soak_live "$dir" || {
        echo "soak failed to launch" >&2
        tail -n 20 "$dir/soak.log" >&2 || true
        return 1
    }
}

# The campaign must clear its own regression corpus before it is allowed
# to hunt: a build that broke a fixed bug would otherwise spend the night
# re-finding it.
wait_for_gate() {
    local dir=$1 log="$dir/soak.log"
    for _ in {1..600}; do
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
    while read -r ni; do
        [[ $ni == - ]] && continue
        [[ $ni == "$nice_level" ]] || {
            echo "soak session $pid contains process at nice $ni" >&2
            return 1
        }
    done < <(ps --sid "$pid" -o ni=)
}

start() {
    [[ $# -ge 1 && $# -le 4 ]] || usage
    local campaign=$1 dir workers seed mix
    dir=$(campaign_dir "$campaign")
    [[ ! -e $dir ]] || {
        echo "campaign directory already exists: $dir" >&2
        exit 1
    }
    # The WHOLE box now, not a third of it per lane: one pool, so the
    # 8x oversubscription is claimed once.
    workers=${2:-$(( $(nproc) * 8 ))}
    seed=${3:-$(date +%s)}
    mix=${4:-50:25:25}
    [[ $workers =~ ^[1-9][0-9]*$ ]] || {
        echo "workers must be positive" >&2
        exit 2
    }
    # A seed passed in the workers position launches billions of
    # children and OOM-kills the box (it happened — twice, 2026-07-19).
    (( workers <= $(nproc) * 16 )) || {
        echo "workers $workers exceeds $(nproc)*16 — arguments are" \
             "<campaign> [workers] [base-seed] [mix]; did you pass the" \
             "seed as workers?" >&2
        exit 2
    }
    [[ $seed =~ ^[0-9]+$ ]] || {
        echo "base-seed must be an unsigned integer" >&2
        exit 2
    }
    [[ $mix =~ ^[0-9]+(\.[0-9]+)?:[0-9]+(\.[0-9]+)?:[0-9]+(\.[0-9]+)?$ ]] || {
        echo "mix must be fuzz:generate:reactive, e.g. 50:25:25" >&2
        exit 2
    }
    [[ $nice_level =~ ^-?[0-9]+$ ]] && ((nice_level >= -20 && nice_level <= 19)) || {
        echo "GRAPHIX_FUZZ_NICE must be between -20 and 19" >&2
        exit 2
    }

    # Every in-flight check holds several descriptors (three pipes and a
    # verdict file), so the default 1024 caps `par` near 200 — and the
    # campaign does not degrade at the cap, it DIES: the harness treats a
    # spawn error as broken-environment and aborts, which it did after
    # passing its gate ("child spawn failed: Too many open files"). The
    # macOS launcher has raised this from the start.
    ulimit -n 10240 2>/dev/null || true

    # Every in-flight check holds several descriptors (three pipes and a
    # verdict file), so the default 1024 caps `par` near 200 — and the
    # campaign does not degrade at the cap, it DIES: the harness treats a
    # spawn error as broken-environment and aborts, which it did right
    # after passing its gate ("child spawn failed: Too many open files").
    # The macOS launcher has raised this from the start.
    ulimit -n 10240 2>/dev/null || true

    CARGO_TARGET_DIR="$target" cargo build --release -p graphix-fuzz \
        --manifest-path "$repo/Cargo.toml"
    "$binary" regress

    mkdir -p "$dir/state" "$dir/corpus"
    cp "$binary" "$dir/graphix-fuzz"
    printf 'workers=%s\nmix=%s\nnice=%s\nbase_seed=%s\n' \
        "$workers" "$mix" "$nice_level" "$seed" > "$dir/state/config"

    trap 'stop_campaign "$dir"' ERR INT TERM
    launch "$dir" "$seed" "$workers" "$mix"
    wait_for_gate "$dir"
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
    local campaign=$1 dir pid
    dir=$(campaign_dir "$campaign")
    [[ -d $dir ]] || {
        echo "campaign does not exist: $dir" >&2
        exit 1
    }
    echo "$dir"
    if soak_live "$dir"; then
        pid=$(soak_pid "$dir")
        ps -p "$pid" -o pid=,sid=,ni=,stat=,etime=,cmd= | sed 's/^/soak: /'
        # The per-source CPU split, which is the number the mix controls.
        # A soak seconds old has logged no counter line yet, and under
        # `pipefail` that empty grep failed the whole pipeline — so
        # `start` exited non-zero on a launch that was perfectly healthy,
        # which is the one signal a deploy verifier must be able to trust.
        grep -aoE '^  [a-z]*….*% cpu' "$dir/soak.log" 2>/dev/null |
            awk -F'…' '{ last[$1] = $0 } END { for (k in last) print "  " last[k] }' |
            sort || true
    elif session_live "$dir"; then
        pid=$(soak_pid "$dir")
        echo "soak: orphaned session $pid"
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
