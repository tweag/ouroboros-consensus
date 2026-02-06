#!/usr/bin/env bash
# Start Mock CDN on port 8080
echo "Starting Mock CDN at http://localhost:8080..."
python3 -m http.server 8080 --directory test-data/synthetic-chain/immutable
