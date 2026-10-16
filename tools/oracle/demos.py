#!/usr/bin/env python3
"""Synthetic demos for the playsim tests: demos.py OUTDIR writes
OUTDIR/wNSS.lmp (map N, seed SS) for each shareware map.

Each is a v1.9-format demo (version 109) of a single player on skill 3
with -nomonsters, wandering for TICS tics: runs of a random forward / side
speed and turn rate, lasting 5-60 tics, so the player walks, runs,
strafes, turns, slides along walls, climbs and drops off steps. No fire or
use buttons. The oracle plays them with -timedemo (tools/oracle/run.sh).
"""
import os
import random
import struct
import sys

TICS = 1400
SEEDS = (1, 2, 3, 4)


def demo(m, seed):
    rng = random.Random(m * 100 + seed)
    # version, skill, episode, map, deathmatch, respawn, fast, nomonsters,
    # consoleplayer, playeringame[4]
    out = bytearray([109, 2, 1, m, 0, 0, 0, 1, 0, 1, 0, 0, 0])
    t = 0
    while t < TICS:
        fwd = rng.choice([50, 50, 50, 25, 25, 0, -25, -50])
        side = rng.choice([0, 0, 0, 24, -24, 40, -40])
        turn = rng.choice([0, 0, 0, 2, -2, 5, -5, 1, -1])
        for _ in range(min(rng.randint(5, 60), TICS - t)):
            out += struct.pack("<bbBB", fwd, side, turn & 0xFF, 0)
            t += 1
    out.append(0x80)                    # DEMOMARKER
    return bytes(out)


def main():
    outdir = sys.argv[1]
    os.makedirs(outdir, exist_ok=True)
    for m in range(1, 10):
        for seed in SEEDS:
            with open(os.path.join(outdir, "w%d%02d.lmp" % (m, seed)), "wb") as f:
                f.write(demo(m, seed))
    print("demos: %s" % outdir)


main()
