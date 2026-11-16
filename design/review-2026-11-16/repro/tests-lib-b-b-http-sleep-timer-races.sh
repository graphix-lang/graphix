#!/bin/sh
# Title: two http fixtures put an arm to sleep on a wall-clock timer that does
# not derive from the event the sleep must follow, so load decides the outcome.
#
# Fixtures: stdlib/graphix-tests/src/lib_tests/http.rs
#   http_serve_forgets_its_server_asleep  (n <- timer(300ms) ~ 1; n <- timer(600ms) ~ 2)
#   http_request_held_at_the_sleep_is_500 (up <- timer(300ms) ~ false)
# Each program below is the fixture's text with an exit added. The 1 ms variant
# stands in for a box whose server start and first request take longer than
# 300 ms (debug build, the suite's load).
#
# Command: sh tests-lib-b-b-http-sleep-timer-races.sh
# Expected (if the fixtures were causal): one outcome, whatever the timer.
# Observed: the 300 ms variants pass on a quiet box. The 1 ms variants' outcome
# changes from run to run (two invocations of this script, 6 runs each):
#   serve_forgets 1ms: "-" (the sleep aborted the request) 11 of 12 runs,
#                      "hello" "-" once
#   held_500     1ms: error:`HTTPError("request failed: error sending request ...")
#                     3 of 12 runs, "500 Internal Server Error" the other 9
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/forgets.gx" <<'EOF'
let n = 0;
n <- sys::time::timer(duration:SLEEP, false) ~ 1;
n <- sys::time::timer(duration:600.ms, false) ~ 2;
let addr = select n { 0 => "127.0.0.1:0", _ => never() };
let handler = |req: http::Request| req ~ {body: "hello", headers: [], status: u16:200};
let v = select n % 2 == 0 {
    true => http::server_addr(http::serve(#addr: addr, #handler: handler)$),
    false => "asleep"
};
let client = http::default_client(null)$;
let out = select v {
    "asleep" => "-",
    a => {
        let resp = http::request(#timeout: duration:200.ms, client, "http://[a]/");
        select resp { error as _ => "error", rs => rs.body }
    }
};
sys::exit(sys::time::after_idle(duration:1.s, out) ~ 0);
out
EOF
cat > "$D/held.gx" <<'EOF'
let up = true;
up <- sys::time::timer(duration:SLEEP, false) ~ false;
let handler = |req: http::Request| {
    let t = sys::time::timer(duration:10.s, false);
    t ~ {body: "late [req.path]", headers: [], status: u16:200}
};
let addr: string = never();
addr <- select up {
    true => http::server_addr(http::serve(#addr: "127.0.0.1:0", #handler: handler)$),
    false => never()
};
let client = http::default_client(null)$;
let r = http::request(#timeout: duration:5.s, client, "http://[addr]/");
let out = select r { error as e => "[e]", r => "[r.status] [r.body]" };
sys::exit(sys::time::after_idle(duration:500.ms, out) ~ 0);
out
EOF
for p in forgets held; do
  for s in 300.ms 1.ms; do
    sed "s/SLEEP/$s/" "$D/$p.gx" > "$D/$p-$s.gx"
    echo "== $p $s"
    for i in 1 2 3 4 5 6; do
      timeout -s KILL 60 "$G" --no-cache "$D/$p-$s.gx" 2>&1 | grep -v WARNING \
        | grep -v '^$' | cut -c1-70 | tr '\n' ' '
      echo
    done
  done
done
rm -rf "$D"
