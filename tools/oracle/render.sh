#!/usr/bin/env bash
# Oracle frames for tests/render.ss: for each shareware map, render every
# camera in build/oracle/cameras/E1Mn.txt (tools/oracle/cameras.py) right
# after the level loads, into OUT/E1Mn/cNNNN.raw.
#   tools/oracle/render.sh camera-dir out-dir
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
cams=$(realpath "$1") out=$(realpath -m "$2")
home="$root/build/oracle/home"; mkdir -p "$home"
for m in 1 2 3 4 5 6 7 8 9; do
  d="$out/E1M$m"; rm -rf "$d"; mkdir -p "$d"
  env -i HOME="$home" DOOMWADDIR="$root/wads" ORACLE_TRACE="$d/trace" \
    ORACLE_CAMERAS="$cams/E1M$m.txt" ORACLE_RAW_DIR="$d" \
    setarch -R "$root/build/oracle/doom-oracle" -warp 1 "$m" > "$d/log" 2>&1
done
touch "$out/.stamp"
echo "render: $(ls "$out"/E1M*/c*.raw | wc -l) frames -> $2"
