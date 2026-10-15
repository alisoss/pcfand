#!/usr/bin/env bash
# tools/package.sh PLATFORM [VERSION] -> build/dist/pcfand-VERSION-PLATFORM.tar.gz (.zip for windows-*)
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
platform=$1
version=${2:-$(git -C "$repo_root" describe --tags --always --dirty)}
case $platform in windows-*) exe=fand.exe ;; *) exe=fand ;; esac
name="pcfand-$version-$platform"
dist="$repo_root/build/dist"
rm -rf "${dist:?}/$name"
mkdir -p "$dist/$name"
cp "$repo_root/bin/$exe" "$repo_root/fandres/FAND.RES" "$repo_root/fandcfg/FAND.CFG" \
   "$repo_root/help/FANDHLP.000" "$repo_root/help/FANDHLP.T00" \
   "$repo_root/LICENSE" "$repo_root/README.md" "$repo_root/HOWTO.txt" "$dist/$name/"
cd "$dist"
case $platform in
  windows-*) rm -f "$name.zip"; zip -qr "$name.zip" "$name" ;;
  *)         COPYFILE_DISABLE=1 tar czf "$name.tar.gz" "$name" ;;   # no ._ files from macOS tar
esac
