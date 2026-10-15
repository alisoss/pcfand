#!/usr/bin/env bash
# tools/win-build.sh [fpc options] -> bin/fand.exe (x86_64-win64, needs Docker)
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
image=fpcwin64
units=/opt/fpcwin/units/x86_64-win64
docker image inspect "$image" >/dev/null 2>&1 ||
  docker build -f "$repo_root/tools/win-cross.Dockerfile" -t "$image" "$repo_root/tools"
out="$repo_root/build/win64"
mkdir -p "$out/units" "$repo_root/bin"
docker run --rm -v "$repo_root:/src:ro" -v "$out:/out" "$image" \
  sh -c "cd /src/pas && exec ppcrossx64 -Twin64 -Px86_64 -Mtp -B \
    -Fu$units/rtl -Fu$units/rtl-console -FU/out/units -FE/out $* FAND.PAS"
cp "$out/FAND.exe" "$repo_root/bin/fand.exe"
