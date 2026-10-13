#!/bin/sh
# x-security-08: http::client #ca_cert ADDS a CA to the platform roots,
# while sys::tls::connect #ca_cert REPLACES them.
# Needs network access to example.com and openssl.
# Run: sh design/review-2026-10-12/repro/x-security-08.sh <path-to-graphix>
# Expected (one meaning for #ca_cert, pinning): both connections refused,
#   since example.com's certificate is not issued by the private CA.
# Observed:
#   tls: refused: error:["TLSError", "TLS handshake failed: invalid peer certificate: UnknownIssuer"]
#   http: ACCEPTED example.com, status 200
set -e
GX=${1:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
openssl req -x509 -newkey rsa:2048 -nodes -keyout "$D/ca.key" -out "$D/ca.pem" \
  -days 30 -subj "/CN=Private Test CA" 2>/dev/null
cat > "$D/p.gx" <<EOF
let ca = sys::fs::read_all_bin("$D/ca.pem")\$;
let http_res = select http::client(#ca_cert: ca, null) {
  error as e => "http client error: [e]",
  c => select http::get(c, "https://example.com/") {
    error as e => "http: refused: [e]",
    r => "http: ACCEPTED example.com, status [r.status]"
  }
};
let tls_res = select sys::tcp::connect("example.com:443") {
  error as e => "tcp error: [e]",
  t => select sys::tls::connect(#ca_cert: ca, "example.com", t) {
    error as e => "tls: refused: [e]",
    _ => "tls: ACCEPTED example.com"
  }
};
println(http_res);
println(tls_res);
sys::exit(sys::time::after_idle(duration:5.s, (http_res, tls_res) ~ 0));
(http_res, tls_res)
EOF
timeout -s KILL 60 "$GX" --no-cache "$D/p.gx"
