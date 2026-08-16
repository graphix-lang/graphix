#!/bin/bash
# Collect fleet soak status into a flat file the dashboard reads.
#
# One line per lane, plus one `host` line per box, because the dashboard
# must be able to say "this box is DOWN" — which is a fact about the
# host, not about any lane. Written to a temp file and renamed, so a
# reader never sees a half-written table.
#
#   host <name> <campaign|-> <up|down|nolanes> <lanes>
#   lane <name> <lane> <runs> <divergences> <crashes> <corpus> <shapes>
#
# Counters come from each lane's own log line, which soak.sh writes at
# the campaign root (`<camp>/<lane>.log`) and katana's soak-start writes
# per lane (`<camp>/<lane>/lane.log`) — both are checked.
#
# usage: soak-collect.sh <out-file> [interval-seconds] [host...]
set -u

out=${1:?usage: soak-collect.sh <out-file> [interval] [host...]}
interval=${2:-15}
shift 2 2>/dev/null || shift $#
hosts=("$@")
[ ${#hosts[@]} -gt 0 ] || hosts=(ryouko hz0 hz1 aieka katana)

# Everything below runs on the remote box. Finds the newest campaign
# directory, counts live lanes, and prints the last counter line of each
# lane log. Kept POSIX-ish and quiet: an unreachable box or a missing
# directory must produce nothing rather than noise.
remote_probe() {
    cat <<'PROBE'
d=$(ls -dt ~/tmp/target/fuzz/*/ 2>/dev/null | head -1)
camp=$(basename "$d" 2>/dev/null)
[ -n "$camp" ] || camp=-
lanes=$(pgrep -f 'graphix-fuzz (fuzz|generate) forever' 2>/dev/null | wc -l | tr -d ' ')
echo "CAMP $camp $lanes"
for l in fuzz generate reactive; do
    f="$d$l.log"
    [ -f "$f" ] || f="$d$l/lane.log"
    [ -f "$f" ] || continue
    # …3944000 run, 2 divergences, 0 crashes, 2 in corpus, 367984 novel shapes
    tail -200 "$f" 2>/dev/null | grep -a 'run,' | tail -1 |
        sed -e 's/[^0-9]*\([0-9]*\) run, \([0-9]*\) divergence[s]*, \([0-9]*\) crash[es]*, \([0-9]*\) in corpus, \([0-9]*\) novel.*/LANE '"$l"' \1 \2 \3 \4 \5/'
done
PROBE
}

while :; do
    tmp="$out.tmp.$$"
    : > "$tmp"
    for h in "${hosts[@]}"; do
        # A box that does not answer within the timeout is DOWN — the
        # dashboard shows that rather than silently dropping the row.
        raw=$(timeout 25 ssh -o BatchMode=yes -o ConnectTimeout=10 "$h" \
                  "bash -s" <<< "$(remote_probe)" 2>/dev/null)
        if [ -z "$raw" ]; then
            echo "host $h - down 0" >> "$tmp"
            continue
        fi
        camp=$(echo "$raw" | awk '$1=="CAMP"{print $2}')
        lanes=$(echo "$raw" | awk '$1=="CAMP"{print $3}')
        [ -n "$camp" ] || camp=-
        [ -n "$lanes" ] || lanes=0
        # katana's lane count includes worker CHILDREN, so >=3 is up.
        if [ "$lanes" -ge 3 ]; then state=up; else state=nolanes; fi
        echo "host $h $camp $state $lanes" >> "$tmp"
        echo "$raw" | awk -v h="$h" '$1=="LANE"{print "lane", h, $2, $3, $4, $5, $6, $7}' >> "$tmp"
    done
    echo "stamp $(date +%H:%M:%S)" >> "$tmp"
    mv -f "$tmp" "$out"
    sleep "$interval"
done
