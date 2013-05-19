#!/bin/bash -e

FILES=$(cat files)

set +e

ls ./src/crypto/c*-key.clj

[ $? -ne 0 ] && set -e && echo "Encrypt using one-time pad." && lein run -m crypto.cipher/-main -E "$FILES" || echo "Already encrypted, do nothing."
