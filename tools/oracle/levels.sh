#!/usr/bin/env bash
# Oracle reference data for each shareware map (tests/level.ss, tests/automap.ss):
#   build/oracle/levels/E1Mn/level.txt   the structures right after P_GroupLines
#   build/oracle/levels/E1Mn/fNNNN.raw   screens[0] after frame N, where the
#     key script below opens the automap with the iddt cheat (frame 2),
#     zooms out to the whole map ('0', frame 3) and turns on the grid
#     ('g', frame 4). The player stands still at the start spot throughout.
#   tools/oracle/levels.sh out-dir
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
out=$(realpath -m "$1")
home="$root/build/oracle/home"; mkdir -p "$home"
# Keys: TAB i d d t ; 0 ; g
keys="1:9,105,100,100,116;2:48;3:103"
for m in 1 2 3 4 5 6 7 8 9; do
  d="$out/E1M$m"; rm -rf "$d"; mkdir -p "$d"
  env -i HOME="$home" DOOMWADDIR="$root/wads" ORACLE_TRACE="$d/trace" \
    ORACLE_LEVELDUMP="$d/level.txt" ORACLE_RAW_DIR="$d" ORACLE_KEYS="$keys" \
    ORACLE_MAX_FRAMES=4 \
    setarch -R "$root/build/oracle/doom-oracle" -warp 1 "$m" > "$d/log" 2>&1
done
touch "$out/.stamp"
echo "levels: $(ls -d "$out"/E1M* | wc -l) maps -> $1"
