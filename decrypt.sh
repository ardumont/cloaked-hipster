#!/bin/bash -e

FILES=$(cat files-decrypt)

set +e

ls ./src/crypto/c*-key.clj

[ $? -eq 0 ] && set -e && lein run -m crypto.cipher/-main -D "$FILES"

# if decrypted, then we remove the key
rm ./src/crypto/c*-key.clj
