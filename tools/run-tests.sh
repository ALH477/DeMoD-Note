#!/usr/bin/env bash
# Run tests for DeMoD-Note

set -e

echo "Running DeMoD-Note test suite..."
cabal test

echo ""
echo "All tests passed!"
