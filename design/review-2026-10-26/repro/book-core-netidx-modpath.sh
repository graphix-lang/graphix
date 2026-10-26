#!/bin/sh
# Title: book/src/shell.md says a netidx script's `mod utils` is looked up at
#   <script path>/utils; the shell looks for <script's parent>/utils.gx
# Command: sh design/review-2026-10-26/repro/book-core-netidx-modpath.sh
#   (needs a netidx resolver on 127.0.0.1:59200, e.g. a machine-local one,
#   and a --config, since without one `graphix netidx:..` panics: see
#   book-core-netidx-script-panic.sh)
# Expected per shell.md "Running from Netidx": "book path"
# Observed: "beside prog" (the code's rule: a script's modules sit beside it,
#   with the .gx / mod.gx layouts, as on disk)
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf '{"addrs": [["127.0.0.1:59200", "Anonymous"]], "base": "/", "default_auth": "Anonymous", "default_bind_config": "local"}\n' > $D/cfg.json
cat > $D/pub.gx <<'GX'
sys::net::publish("/rvbk/app/prog", "mod utils;\nutils::x")$;
sys::net::publish("/rvbk/app/prog/utils.gx", "let x = \"under prog\";")$;
sys::net::publish("/rvbk/app/utils.gx", "let x = \"beside prog\";")$;
sys::net::publish("/rvbk/app/prog/utils", "let x = \"book path\";")$;
sys::exit(sys::time::timer(duration:12.s, false) ~ 0);
"published"
GX
timeout -s KILL 15 "$G" --no-cache --config $D/cfg.json $D/pub.gx > /dev/null 2>&1 &
sleep 3
timeout -s KILL 8 "$G" --no-cache --config $D/cfg.json --resolve-timeout 3 netidx:/rvbk/app/prog
wait
rm -rf $D
