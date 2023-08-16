#!/bin/zsh

# Import key from Keybase into gpg
keybase pgp export -s | gpg --allow-secret-key-import --import
