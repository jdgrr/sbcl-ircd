#!/bin/sh
# Generate self-signed TLS certificates for sbcl-ircd.
# Uses ECDSA prime256v1 for maximum performance on Apple Silicon.
# Output: cert.pem (certificate) and key.pem (private key) in the current directory.

set -eu

CERT_DIR="$(cd "$(dirname "$0")" && pwd)"
CERT_FILE="${CERT_DIR}/cert.pem"
KEY_FILE="${CERT_DIR}/key.pem"
DAYS=3650
SUBJECT="/CN=localhost/O=sbcl-ircd/OU=Development"

if [ -f "${CERT_FILE}" ] && [ -f "${KEY_FILE}" ]; then
    echo "[generate-certs] Certificates already exist:"
    echo "  cert: ${CERT_FILE}"
    echo "  key:  ${KEY_FILE}"
    echo "[generate-certs] Remove them manually to regenerate."
    exit 0
fi

echo "[generate-certs] Generating ECDSA prime256v1 private key..."
openssl ecparam -genkey -name prime256v1 -noout -out "${KEY_FILE}"
chmod 600 "${KEY_FILE}"

echo "[generate-certs] Generating self-signed certificate (${DAYS} days)..."
openssl req -new -x509 \
    -key "${KEY_FILE}" \
    -out "${CERT_FILE}" \
    -days "${DAYS}" \
    -subj "${SUBJECT}" \
    -addext "subjectAltName=DNS:localhost,IP:127.0.0.1"

echo "[generate-certs] Done."
echo "  cert: ${CERT_FILE}"
echo "  key:  ${KEY_FILE}"
openssl x509 -in "${CERT_FILE}" -noout -text | head -15
