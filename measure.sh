#!/usr/bin/env bash

mkdir -p measurements/
DUR=300
DATE=$(date +%s)
DIR=$1

measure() {
    local BIN="${1}"
    local FILE_NAME="measurements/${DATE}-${BIN}"

    local CMD=".stack-work/dist/x86_64-linux-nix/Cabal-3.2.1.0/build/${BIN}-bin/${BIN}-bin ${DIR}"

    $CMD &
    PID=$!
    psrecord "${PID}" --include-children --plot "${FILE_NAME}.png" --log "${FILE_NAME}.txt" --duration "${DUR}" &&
    kill "${PID}"
    firefox "${FILE_NAME}.png"
}

measure unix-recursive-bytestring
measure unix-recursive-string
measure dir-traverse
measure dirstream
