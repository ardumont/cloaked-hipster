#!/bin/bash -xe

FILES=$(cat files)

lein run -m crypto.cipher/-main -E "$FILES"
