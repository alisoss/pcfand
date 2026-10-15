#!/usr/bin/env bash
# tools/build.sh [fpc options] -> bin/fand, FAND.RES, FAND.CFG; builds examples/*
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root/pas"
rm -f FAND                    # a failed compile must not leave the last binary to pass
fpc -Mtp -B "$@" FAND.PAS | grep -E "Error|Fatal|lines compiled" || true
[[ -x FAND ]] || { echo "build.sh: compile failed" >&2; exit 1; }
mkdir -p "$repo_root/bin"
rm -f "$repo_root/bin/fand"   # a new inode: macOS kills an overwritten signed binary
cp FAND "$repo_root/bin/fand"
cp "$repo_root/fandres/FAND.RES" "$repo_root/fandcfg/FAND.CFG" "$repo_root/bin/"
for d in "$repo_root"/examples/*/; do
  t=$(basename "$d")
  (cd "$d" && "$repo_root/bin/fand" "$t" --source-in src </dev/null) \
    || { echo "build.sh: examples/$t does not build" >&2; exit 1; }
done
