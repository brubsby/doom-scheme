#!/usr/bin/env bash
# The oracle's title loop (no arguments: title, DEMO1, credits, DEMO2,
# HELP2, DEMO3, and round again) with ORACLE_FAKETIME, so it runs fast and
# the same every time, traced until gametic N.
#   tools/oracle/attract.sh out.trace N
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
out=$(realpath -m "$1")
home="$root/build/oracle/home"; mkdir -p "$home"
env -i HOME="$home" DOOMWADDIR="$root/wads" ORACLE_TRACE="$out" ORACLE_FAKETIME=1 \
  ORACLE_MAX_GAMETIC="$2" setarch -R "$root/build/oracle/doom-oracle" > "${out%.trace}.log" 2>&1
echo "attract: $(wc -l < "$out") frames, to gametic $(tail -1 "$out" | cut -d' ' -f1) -> $1"
