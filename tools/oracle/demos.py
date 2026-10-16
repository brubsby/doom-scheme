#!/usr/bin/env python3
"""Synthetic demos for the playsim tests: demos.py OUTDIR writes, for each
shareware map N, OUTDIR/wNSS.lmp and OUTDIR/fNSS.lmp (seed SS).

All are v1.9-format demos (version 109) of a single player wandering: runs
of a random forward / side speed and turn rate lasting 5-60 tics, so the
player walks, runs, strafes, turns, slides along walls, climbs and drops
off steps.
- w demos: skill 3, -nomonsters, no buttons, TICS tics.
- f demos ("fight"): monsters on, a skill that varies with map and seed
  (all five occur), FTICS tics, and buttons: fire held for some runs, use
  pressed now and then, and weapon changes.
The oracle plays them with -timedemo (tools/oracle/run.sh).
"""
import os
import random
import struct
import sys

TICS = 1400
SEEDS = (1, 2, 3, 4)
FTICS = 2100
FSEEDS = (1, 2)

BT_ATTACK, BT_USE, BT_CHANGE, BT_WEAPONSHIFT = 1, 2, 4, 3


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


def fight_demo(m, seed):
    rng = random.Random(1000 + m * 100 + seed)
    skill = (m + seed) % 5
    out = bytearray([109, skill, 1, m, 0, 0, 0, 0, 0, 1, 0, 0, 0])
    t = 0
    while t < FTICS:
        fwd = rng.choice([50, 50, 25, 25, 0, 0, -25, -50])
        side = rng.choice([0, 0, 0, 24, -24, 40, -40])
        turn = rng.choice([0, 0, 0, 2, -2, 5, -5, 1, -1])
        fire = rng.random() < 0.4
        n = min(rng.randint(5, 60), FTICS - t)
        change = rng.random() < 0.15
        weapon = rng.randint(0, 7)
        for i in range(n):
            buttons = BT_ATTACK if fire else 0
            if i == 0 and change:
                buttons = BT_CHANGE | (weapon << BT_WEAPONSHIFT)
            elif i % 17 == 3 and rng.random() < 0.3:
                buttons |= BT_USE
            out += struct.pack("<bbBB", fwd, side, turn & 0xFF, buttons)
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
        for seed in FSEEDS:
            with open(os.path.join(outdir, "f%d%02d.lmp" % (m, seed)), "wb") as f:
                f.write(fight_demo(m, seed))
    print("demos: %s" % outdir)


main()
