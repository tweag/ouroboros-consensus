#!/usr/bin/env bash
# Client doesn't seem to need a DB path argument based on usage help
echo "Starting Syncing Client (Node)..."
cabal run immdb-client -- \
  --config ouroboros-consensus-cardano/test/tools-test/disk/config/config.json \
  --addr 127.0.0.1 \
  --port 3001
