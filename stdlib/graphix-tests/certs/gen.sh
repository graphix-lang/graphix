#!/bin/bash
set -e
cd "$(dirname "$0")"

# CA key + self-signed cert
if ! test -f ca.key; then
  openssl genrsa -out ca.key 4096
fi
openssl req -new -key ca.key -x509 -sha512 -out ca.pem -days 7300 \
  -subj "/CN=graphix-test-ca/O=graphix" \
  -addext "basicConstraints=critical, CA:TRUE" \
  -addext "keyUsage=critical, cRLSign, digitalSignature, keyCertSign"

# Server key + cert signed by CA
if ! test -f server.key; then
  openssl genrsa -out server.key 4096
fi
openssl req -new -key server.key -sha512 -out server.csr \
  -subj "/CN=127.0.0.1/O=graphix"
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key \
  -CAcreateserial -out server.pem -days 730 -extfile <(cat <<EOF
basicConstraints=critical, CA:FALSE
keyUsage=nonRepudiation,digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=IP:127.0.0.1
EOF
)
rm -f server.csr ca.srl
openssl verify -trusted ca.pem server.pem
