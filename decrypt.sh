#!/bin/bash -xe

FILES=$(cat files-decrypt)

lein run -m crypto.cipher/-main -D "$FILES"
