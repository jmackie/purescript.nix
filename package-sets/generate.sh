#!/usr/bin/env bash
set -euo pipefail

readonly REPO_DIR=__package-sets

git clone https://github.com/purescript/package-sets $REPO_DIR

trap cleanup EXIT
function cleanup() {
    echo "Removing package-sets repo"
    rm -rf "$REPO_DIR"
}

for tag in $(cd $REPO_DIR && git tag); do

    # Only build package sets for >0.12, and ignore release candidates
    if [[ -f "$tag.json" || $tag == *"0.10"* || $tag == *"0.11"* || $tag == *"rc"* ]]; then
        continue
    fi

    (cd $REPO_DIR && git checkout --quiet "$tag")
    echo "Elaborating $tag..."
    nix-shell ../elaborator/default.nix --run "elaborate-purescript-packages --package-set $REPO_DIR/packages.json >$tag.json"
done
