#!/usr/bin/env bash
# Fetch Freedoom 0.13.0 (BSD-licensed IWADs) as stand-ins for the commercial
# IWADs, named the way linuxdoom's IdentifyVersion looks for them:
#   wads/freedoom2/doom2.wad   (commercial: DOOM II's maps, monsters, finale)
#   wads/freedoom1/doomu.wad   (retail: four episodes)
# Run the game or the oracle with DOOMWADDIR set to one of the directories.
# The zip and both WADs are hash-checked.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
url=https://github.com/freedoom/freedoom/releases/download/v0.13.0/freedoom-0.13.0.zip
zip_sha256=3f9b264f3e3ce503b4fb7f6bdcb1f419d93c7b546f4df3e874dd878db9688f59
fd1_sha256=7323bcc168c5a45ff10749b339960e98314740a734c30d4b9f3337001f9e703d
fd2_sha256=a8772e088847032510d97ba2312406a6998f21cbab44d4ff10696faa9c0ecd4b
fd1="$root/wads/freedoom1/doomu.wad"
fd2="$root/wads/freedoom2/doom2.wad"

if [[ -f $fd1 && -f $fd2 ]] &&
   echo "$fd1_sha256  $fd1" | sha256sum -c --status &&
   echo "$fd2_sha256  $fd2" | sha256sum -c --status; then
  echo "freedoom already present"; exit 0
fi
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
curl -fsSL -o "$tmp/freedoom.zip" "$url"
echo "$zip_sha256  $tmp/freedoom.zip" | sha256sum -c --quiet
unar -q -f -o "$tmp/x" "$tmp/freedoom.zip"
mkdir -p "$root/wads/freedoom1" "$root/wads/freedoom2"
cp "$tmp"/x/freedoom-0.13.0/freedoom1.wad "$fd1"
cp "$tmp"/x/freedoom-0.13.0/freedoom2.wad "$fd2"
cp "$tmp"/x/freedoom-0.13.0/COPYING.txt "$root/wads/freedoom1/"
cp "$tmp"/x/freedoom-0.13.0/COPYING.txt "$root/wads/freedoom2/"
echo "$fd1_sha256  $fd1" | sha256sum -c --quiet
echo "$fd2_sha256  $fd2" | sha256sum -c --quiet
echo "fetched wads/freedoom1/doomu.wad and wads/freedoom2/doom2.wad (Freedoom 0.13.0)"
