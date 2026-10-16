#!/usr/bin/env bash
# Run the linuxdoom oracle on one demo and write its per-tic trace.
#   tools/oracle/run.sh demo1 out.trace [ppm-dir [every-n-tics]]
# ASLR is disabled (setarch -R): vanilla reads a few bytes of heap pointers
# when rendering overruns a sprite post (see DESIGN.md, "Garbage reads"), and
# fixed addresses make those frames reproducible run to run.
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
demo=$1 out=$(realpath -m "$2")
home="$root/build/oracle/home"; mkdir -p "$home"
env=(HOME="$home" DOOMWADDIR="$root/wads" ORACLE_TRACE="$out")
if [[ $# -ge 3 ]]; then
  mkdir -p "$3"; env+=(ORACLE_PPM_DIR="$(realpath "$3")" ORACLE_PPM_EVERY="${4:-35}")
fi
# -timedemo ends with I_Error("timed N gametics..."), exit status 255.
env -i "${env[@]}" setarch -R "$root/build/oracle/doom-oracle" -timedemo "$demo" \
  > "${out%.trace}.log" 2>&1 || grep -q "^Error: timed" "${out%.trace}.log"
echo "$demo: $(wc -l < "$out") tics -> $2"
