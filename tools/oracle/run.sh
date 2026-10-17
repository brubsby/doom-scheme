#!/usr/bin/env bash
# Run the linuxdoom oracle on one demo and write its per-tic trace.
#   tools/oracle/run.sh demo1 out.trace [ppm-dir [every-n-tics]]
# The demo is a lump in the IWAD, or with DEMODIR=dir, the file dir/NAME.lmp
# (the oracle is run from that directory, since -timedemo takes a bare
# name).
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
# ORACLE_SNDSERVER (the lockstep sound server) and its ORACLE_AUDIO and
# ORACLE_AUDIO_RAW pass through.
for v in ORACLE_AUDIO ORACLE_AUDIO_RAW ORACLE_SNDSERVER; do
  [[ -n ${!v:-} ]] && env+=("$v=$(realpath -m "${!v}")")
done
# -timedemo ends with I_Error("timed N gametics..."), exit status 255.
log="${out%.trace}.log"
(cd "${DEMODIR:-.}" && env -i "${env[@]}" setarch -R "$root/build/oracle/doom-oracle" \
   -timedemo "$demo" > "$log" 2>&1) || grep -q "^Error: timed" "$log"
echo "$demo: $(wc -l < "$out") tics -> $2"
