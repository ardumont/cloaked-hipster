#!/bin/bash -xe

FILES=$(cat files-decrypt)

lein run -m crypto.cipher/-main -D "$FILES"

# if decrypted, then we remode the key
rm ./src/crypto/c*-key.clj
