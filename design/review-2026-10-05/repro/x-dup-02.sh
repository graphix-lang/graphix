#!/usr/bin/env bash
# x-dup-02: the netidx module loader does not lay modules out as files do.
# NetidxResolver::resolve (stdlib/graphix-package-sys/src/loader.rs:99)
# tries only `{base}/{name}.gx`, never `{base}/{name}/mod.gx`;
# for_source (loader.rs:119) bases a module's submodules at the module's
# own implementation path (`/s/m.gx` -> `/s/m.gx/n.gx`, a file gives
# `<dir>/m/`); and graphix-shell/src/main.rs:409 bases a `netidx:`
# script's modules at the script's own path, where a file script gets its
# parent directory. book/src/modules/implementation.md:153 says netidx
# hierarchies "work the same as in the file system".
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-dup-02.sh
#   (needs the `netidx` CLI on PATH and `ss`; every graphix run passes
#   --config, so no machine-local resolver daemon is started: the
#   publisher process hosts a private process-internal netidx)
#
# expected (each layout loads over netidx as the same tree on disk):
#   book  m/mod.gx + m/n.gx, GRAPHIX_MODPATH=netidx:/lib/graphix   file 1  netidx 1
#   sub   m.gx + m/n.gx,     GRAPHIX_MODPATH=netidx:/s             file 3  netidx 3
#   flat  m.gx beside test.gx, no GRAPHIX_MODPATH                  file 2  netidx 2
# observed (HEAD c722befe, debug build):
#   book  file 1  netidx "Error: could not resolve module m ... not found; not found"
#   sub   file 3  netidx "Error: could not resolve module n ... not found; not found; not found"
#   flat  file 2  netidx "Error: could not resolve module m ... ~/.local/share/graphix/m.gx ... no such file"
#   ctrl  /c/test.gx/m.gx + /c/test.gx/m.gx/n.gx (children under the
#         file's own path, what the loader actually requires)  netidx 4
set -u
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cd "$T"
export XDG_CACHE_HOME=$T/cache
unset GRAPHIX_MODPATH
EX='sys::exit(sys::time::after_idle(duration:100.ms, 0));'

# the same trees on disk
mkdir -p book/m sub/m flat
printf 'mod m; %s m::n::hello\n' "$EX" > book/test.gx
printf 'mod n\n' > book/m/mod.gx
printf 'let hello = 1\n' > book/m/n.gx
printf 'mod m; %s m::n::hello\n' "$EX" > sub/test.gx
printf 'mod n\n' > sub/m.gx
printf 'let hello = 3\n' > sub/m/n.gx
printf 'mod m; %s m::hello\n' "$EX" > flat/test.gx
printf 'let hello = 2\n' > flat/m.gx

# the publisher: its own process-internal resolver + publisher
cat > pub.gx <<'GX'
let ex = "sys::exit(sys::time::after_idle(duration:100.ms, 0));";
let book = [
  sys::net::publish("/lib/graphix/test.gx", "mod m; [ex] m::n::hello"),
  sys::net::publish("/lib/graphix/m/mod.gx", "mod n"),
  sys::net::publish("/lib/graphix/m/n.gx", "let hello = 1")
];
let sub = [
  sys::net::publish("/s/test.gx", "mod m; [ex] m::n::hello"),
  sys::net::publish("/s/m.gx", "mod n"),
  sys::net::publish("/s/m/n.gx", "let hello = 3")
];
let flat = [
  sys::net::publish("/a/test.gx", "mod m; [ex] m::hello"),
  sys::net::publish("/a/m.gx", "let hello = 2")
];
let ctrl = [
  sys::net::publish("/c/test.gx", "mod m; [ex] m::n::hello"),
  sys::net::publish("/c/test.gx/m.gx", "mod n"),
  sys::net::publish("/c/test.gx/m.gx/n.gx", "let hello = 4")
];
(book, sub, flat, ctrl)
GX
timeout -s KILL 150 "$G" --config "$T/none.json" --no-netidx --no-cache "$T/pub.gx" \
  > pub.out 2>&1 &
PUB=$!

# find the publisher's resolver: one of its listening ports answers a list
CFG=
for _ in $(seq 1 60); do
  sleep 0.5
  GPID=$(pgrep -f -n "graphix --config $T/none.json")
  [ -z "$GPID" ] && continue
  for port in $(ss -ltnp | grep "pid=$GPID," | awk '{print $4}' | sed 's/.*://'); do
    printf '{"base":"/","addrs":[["127.0.0.1:%s","Anonymous"]],"default_auth":"Anonymous","default_bind_config":"local"}\n' \
      "$port" > "cfg_$port.json"
    if timeout -s KILL 5 netidx resolver -c "cfg_$port.json" list '/c/test.gx/m.gx/*' \
      2>/dev/null | grep -q n.gx; then
      CFG=$T/cfg_$port.json
      break 2
    fi
  done
done
[ -n "$CFG" ] || { echo "publisher's resolver not found"; cat pub.out; kill -TERM $PUB; wait $PUB; exit 2; }

file() { timeout -s KILL 25 "$G" --config "$T/none.json" --no-netidx --no-cache "$T/$1/test.gx" 2>&1 | head -4; }
net() { # modpath, script path
  if [ -n "$1" ]; then export GRAPHIX_MODPATH=$1; else unset GRAPHIX_MODPATH; fi
  timeout -s KILL 25 "$G" --config "$CFG" --no-cache --resolve-timeout 5 "netidx:$2" 2>&1 \
    | grep -v '^$' | head -4
  unset GRAPHIX_MODPATH
}
echo "=== book: m/mod.gx + m/n.gx"
echo "--- file:   graphix book/test.gx";  file book
echo "--- netidx: GRAPHIX_MODPATH=netidx:/lib/graphix graphix netidx:/lib/graphix/test.gx"
net netidx:/lib/graphix /lib/graphix/test.gx
echo "=== sub: m.gx + m/n.gx"
echo "--- file:   graphix sub/test.gx";  file sub
echo "--- netidx: GRAPHIX_MODPATH=netidx:/s graphix netidx:/s/test.gx"
net netidx:/s /s/test.gx
echo "=== flat: m.gx beside test.gx, no GRAPHIX_MODPATH"
echo "--- file:   graphix flat/test.gx";  file flat
echo "--- netidx: graphix netidx:/a/test.gx"
net "" /a/test.gx
echo "=== ctrl: /c/test.gx/m.gx + /c/test.gx/m.gx/n.gx, no GRAPHIX_MODPATH"
echo "--- netidx: graphix netidx:/c/test.gx"
net "" /c/test.gx
kill -TERM $PUB
wait $PUB
exit 0
