#!/usr/bin/env bash
# Fetch the DOOM v1.9 shareware WAD into wads/doom1.wad.
# Source is id's official doom19s.zip (freely redistributable shareware);
# its DEICE disk parts are extracted with unar. Both the zip and the WAD are
# hash-checked.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/wads/doom1.wad"
zip_sha256=cacf0142b31ca1af00796b4a0339e07992ac5f21bc3f81e7532fe1b5e1b486e6
wad_md5=f0cefca49926d00903cf57551d901abe
mirrors=(
  https://youfailit.net/pub/idgames/idstuff/doom/doom19s.zip
  https://www.quaddicted.com/files/idgames/idstuff/doom/doom19s.zip
  https://ftpmirror1.infania.net/pub/idgames/idstuff/doom/doom19s.zip
)

if [[ -f $out ]] && echo "$wad_md5  $out" | md5sum -c --status; then
  echo "wads/doom1.wad already present"; exit 0
fi
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
for m in "${mirrors[@]}"; do
  if curl -fsSL -o "$tmp/doom19s.zip" "$m" &&
     echo "$zip_sha256  $tmp/doom19s.zip" | sha256sum -c --status; then
    break
  fi
  rm -f "$tmp/doom19s.zip"
done
[[ -f $tmp/doom19s.zip ]] || { echo "no mirror served a valid doom19s.zip" >&2; exit 1; }

unar -q -f -D -o "$tmp/zip" "$tmp/doom19s.zip"
cat "$tmp"/zip/DOOMS_19.1 "$tmp"/zip/DOOMS_19.2 > "$tmp/dooms.exe"
unar -q -f -o "$tmp/sfx" "$tmp/dooms.exe" DOOM1.WAD
mkdir -p "$root/wads"
cp "$(find "$tmp/sfx" -name DOOM1.WAD)" "$out"
echo "$wad_md5  $out" | md5sum -c --quiet
echo "fetched wads/doom1.wad (v1.9 shareware)"
