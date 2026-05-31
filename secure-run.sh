#!/bin/sh
# Run sbcl-ircd behind stunnel TLS termination.

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "${SCRIPT_DIR}"

IRCD_PID=""
STUNNEL_PID=""

cleanup() {
    code="${1:-0}"
    echo
    echo "[secure-run] Shutting down..."
    if [ -n "${STUNNEL_PID}" ] && kill -0 "${STUNNEL_PID}" 2>/dev/null; then
        echo "[secure-run] Stopping stunnel (PID ${STUNNEL_PID})..."
        kill "${STUNNEL_PID}" 2>/dev/null || true
        wait "${STUNNEL_PID}" 2>/dev/null || true
    fi
    if [ -n "${IRCD_PID}" ] && kill -0 "${IRCD_PID}" 2>/dev/null; then
        echo "[secure-run] Stopping sbcl-ircd (PID ${IRCD_PID})..."
        kill "${IRCD_PID}" 2>/dev/null || true
        wait "${IRCD_PID}" 2>/dev/null || true
    fi
    echo "[secure-run] All processes stopped."
    exit "${code}"
}

trap cleanup INT TERM

echo "[secure-run] Checking TLS certificates..."
sh "${SCRIPT_DIR}/generate-certs.sh"

if ! command -v stunnel >/dev/null 2>&1; then
    echo "[secure-run] ERROR: stunnel is not installed."
    echo "[secure-run] Install it with: brew install stunnel"
    exit 1
fi

echo "[secure-run] Starting sbcl-ircd on 127.0.0.1:6667..."
sbcl --script "${SCRIPT_DIR}/run.lisp" 6667 127.0.0.1 &
IRCD_PID=$!
sleep 2

if ! kill -0 "${IRCD_PID}" 2>/dev/null; then
    echo "[secure-run] ERROR: sbcl-ircd failed to start."
    exit 1
fi
echo "[secure-run] sbcl-ircd running (PID ${IRCD_PID})."

echo "[secure-run] Starting stunnel TLS proxy on port 6697..."
stunnel "${SCRIPT_DIR}/stunnel.conf" &
STUNNEL_PID=$!
sleep 1

if ! kill -0 "${STUNNEL_PID}" 2>/dev/null; then
    echo "[secure-run] ERROR: stunnel failed to start."
    cleanup 1
fi
echo "[secure-run] stunnel running (PID ${STUNNEL_PID})."

echo
echo "sbcl-ircd TLS mode"
echo "  Plaintext: 127.0.0.1:6667"
echo "  TLS:       0.0.0.0:6697"
echo "  Press Ctrl+C to stop."
echo

wait
