#!/usr/bin/env bash
# tests-lib-b1-02: the committed test server cert expires 2028-03-31
# 23:14:33 GMT; from then on https_round_trip (lib_tests/http.rs) and
# tls_round_trip / socket_union_dispatch (lib_tests/tls.rs) fail in every
# mode with `timeout after 30s waiting for result`.
#
# certs/gen.sh:21 signs server.pem for 730 days; the CA has 7300. The
# script moves the wall clock with an LD_PRELOAD shim over clock_gettime
# (CLOCK_REALTIME only: tokio's timers are monotonic) and runs the
# https_round_trip fixture body with the shipped certs just before and just
# after notAfter. The fixture's `http::request(..)$` turns the TLS failure
# into bottom, so the result never fires; surfaced, the error is reqwest's
# "error sending request", which does not name the certificate. With
# GRAPHIX_TESTS_BIN set to a built graphix-tests lib test binary it also
# runs the 12 real tests at the later clock.
#
# command: GRAPHIX=/path/to/graphix [GRAPHIX_TESTS_BIN=~/tmp/target/debug/deps/graphix_tests-<hash>] \
#   bash design/review-2026-10-05/repro/tests-lib-b1-02.sh
#
# expected: "hello GET" at both clocks (a test fixture carries no expiry
# date), and the tests pass at both clocks.
#
# observed (HEAD c722befe, debug build, run on 2026-10-06; the test
# binary built 2026-10-05, the fixtures unchanged since 2026-09-16):
#   == clock 2028-03-31T23:00:00Z
#   openssl verify: /home/eric/proj/graphix/stdlib/graphix-tests/certs/server.pem: OK
#   fixture body: "hello GET"
#   == clock 2028-04-01T00:00:00Z
#   openssl verify: error 10 at 0 depth lookup: certificate has expired
#   fixture body: (nothing in 20 s)
#   error surfaced: error:["HTTPError", "request failed: error sending request for url (https://127.0.0.1:46337/)"]
#        20 certificate expired: verification time T (UNIX), but certificate is not valid after 1838157273
#        12 Error: timeout after 30s waiting for result
#         1 test result: FAILED. 0 passed; 12 failed; 0 ignored; 0 measured; 5987 filtered out; finished in 30.09s
#   (at the real clock the same 12 tests pass in 0.32 s; each failing
#   https_round_trip section shows only the timeout, the certificate is
#   named only by log lines and by the tls tests' unhandled-error prints)
set -u
GRAPHIX=${GRAPHIX:-graphix}
repo=$(cd "$(dirname "$0")/../../.." && pwd)
certs=$repo/stdlib/graphix-tests/certs
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/shift.c" <<'EOF'
#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdlib.h>
#include <time.h>

static long off;
static int (*real_cg)(clockid_t, struct timespec *);

__attribute__((constructor)) static void init(void) {
    const char *s = getenv("SHIFT_SECS");
    off = s ? atol(s) : 0;
    real_cg = dlsym(RTLD_NEXT, "clock_gettime");
}

int clock_gettime(clockid_t clk, struct timespec *ts) {
    int r = real_cg(clk, ts);
    if (r == 0 && (clk == CLOCK_REALTIME || clk == CLOCK_REALTIME_COARSE))
        ts->tv_sec += off;
    return r;
}
EOF
cc -shared -fPIC -O2 -o "$dir/shift.so" "$dir/shift.c" -ldl || exit 1

# the body of https_round_trip (http.rs:28) with the shipped certs
fixture() {
    cat <<EOF
let cd = "$certs";
let result = {
    let cert = sys::fs::read_all_bin("[cd]/server.pem")\$;
    let key = sys::fs::read_all_bin("[cd]/server.key")\$;
    let handler = |req: http::Request| {
        body: "hello [req.method]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #cert: cert, #key: key, #handler: handler)\$;
    let addr = http::server_addr(server);
    let ca = sys::fs::read_all_bin("[cd]/ca.pem")\$;
    let client = http::client(#ca_cert: ca, server)\$;
    $1
};
result
EOF
}
fixture 'let resp = http::request(client, "https://[addr]/")$; resp.body' > "$dir/https.gx"
fixture 'select http::request(client, "https://[addr]/") { error as e => e, r => r.body }' \
    > "$dir/https_err.gx"

# run SECS CMD..: CMD under a SECS kill timeout with the clock moved by $SHIFT
run() { timeout -s KILL "$1" env SHIFT_SECS="$SHIFT" LD_PRELOAD="$dir/shift.so" "${@:2}"; }

for at in 2028-03-31T23:00:00Z 2028-04-01T00:00:00Z; do
    echo "== clock $at"
    SHIFT=$(( $(date -d "$at" +%s) - $(date +%s) ))
    echo "openssl verify: $(openssl verify -attime "$(date -d "$at" +%s)" \
        -CAfile "$certs/ca.pem" "$certs/server.pem" 2>&1 | grep -e OK -e error | head -1)"
    out=$(run 20 "$GRAPHIX" --no-cache "$dir/https.gx" 2>/dev/null | head -1)
    echo "fixture body: ${out:-(nothing in 20 s)}"
    if [ -z "$out" ]; then
        out=$(run 20 "$GRAPHIX" --no-cache "$dir/https_err.gx" 2>/dev/null | head -1)
        echo "error surfaced: $out"
        if [ -n "${GRAPHIX_TESTS_BIN:-}" ]; then
            run 120 "$GRAPHIX_TESTS_BIN" --test-threads=12 https_round_trip tls:: 2>&1 |
                grep -o -e 'Error: timeout after 30s waiting for result' \
                    -e 'test result: .*' -e 'certificate expired: [^"]*' |
                sed 's/verification time [0-9]*/verification time T/; s/([0-9]* seconds ago)//' |
                sort | uniq -c | sort -rn
        fi
    fi
done
