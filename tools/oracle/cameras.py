#!/usr/bin/env python3
"""Camera lists for the renderer tests: cameras.py WAD OUTDIR writes
OUTDIR/E1Mn.txt for each shareware map, one camera per line:

    x y angle blocks detail extralight fixedcolormap

(the format of oracle_camera_probe in oracle.c). Cameras stand on the spots
of a spread of the map's things, plus player 1's start, each looking along
the thing's angle and along a pseudo-random angle. Most use the full
screen at high detail; some cycle through the smaller view sizes, low
detail, gun-flash extralight, and the invulnerability / light-amp colormaps.
"""
import os
import struct
import sys

ANG45 = 0x20000000
PER_MAP = 13


def lumps(wad):
    data = open(wad, "rb").read()
    n, ofs = struct.unpack_from("<ii", data, 4)
    out = []
    for i in range(n):
        pos, size, name = struct.unpack_from("<ii8s", data, ofs + 16 * i)
        out.append((name.rstrip(b"\0").decode(), data[pos:pos + size]))
    return out


def main():
    wad, outdir = sys.argv[1], sys.argv[2]
    os.makedirs(outdir, exist_ok=True)
    directory = lumps(wad)
    names = [n for n, _ in directory]
    for m in range(1, 10):
        name = "E1M%d" % m
        things = directory[names.index(name) + 1][1]
        rows = [struct.unpack_from("<hhhhh", things, o) for o in range(0, len(things) - 9, 10)]
        picks = [r for r in rows if r[3] == 1]
        step = max(1, len(rows) // PER_MAP)
        picks += rows[::step][:PER_MAP]
        cams = []
        for j, (x, y, angle, _type, _opts) in enumerate(picks):
            base = (ANG45 * (angle // 45)) & 0xFFFFFFFF
            for a in (base, (base + 0x9E3779B9 * (j + 1)) & 0xFFFFFFFF):
                c = len(cams)
                blocks = 3 + (c // 6) % 8 if c % 6 == 5 else 11
                detail = 1 if c % 7 == 6 else 0
                extralight = 1 + (c // 5) % 2 if c % 5 == 4 else 0
                fixed = 32 if c % 11 == 10 else (1 if c % 13 == 12 else 0)
                cams.append("%d %d %d %d %d %d %d" % (x, y, a, blocks, detail, extralight, fixed))
        with open(os.path.join(outdir, name + ".txt"), "w") as f:
            f.write("\n".join(cams) + "\n")
    print("cameras: %s" % outdir)


main()
