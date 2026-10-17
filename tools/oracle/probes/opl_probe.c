// Runs Chocolate Doom's Nuked OPL3 (reference/chocolate-doom/opl/opl3.c) over
// a register script, for cross-checking the Scheme port (tests/opl.ss).
// Built with the host cc, no SDL:
//   cc -O2 -I reference/chocolate-doom/opl -o build/opl/opl_probe \
//      tools/oracle/probes/opl_probe.c reference/chocolate-doom/opl/opl3.c
// Usage: opl_probe script.txt out.raw
// Script: one command per line, decimal numbers, '#' starts a comment line.
//   r RATE      OPL3_Reset(chip, RATE)
//   w REG VAL   OPL3_WriteReg(chip, REG, VAL)
//   b REG VAL   OPL3_WriteRegBuffered(chip, REG, VAL)
//   g N         OPL3_GenerateStream(chip, buf, N)
// Output: every generated frame as interleaved L R int16, little-endian.
#include <stdio.h>
#include <stdlib.h>
#include "opl3.h"

static opl3_chip chip;
static Bit16s buf[2 * 65536];

int main (int argc, char** argv)
{
    FILE *in, *out;
    char line[256];
    unsigned a, b;
    if (argc != 3)
    {
        fprintf (stderr, "usage: opl_probe script.txt out.raw\n");
        return 2;
    }
    in = fopen (argv[1], "r");
    out = fopen (argv[2], "wb");
    if (!in || !out)
    {
        perror ("opl_probe");
        return 2;
    }
    OPL3_Reset (&chip, 49716);
    while (fgets (line, sizeof line, in))
    {
        switch (line[0])
        {
        case 'r':
            sscanf (line + 1, "%u", &a);
            OPL3_Reset (&chip, a);
            break;
        case 'w':
            sscanf (line + 1, "%u %u", &a, &b);
            OPL3_WriteReg (&chip, (Bit16u) a, (Bit8u) b);
            break;
        case 'b':
            sscanf (line + 1, "%u %u", &a, &b);
            OPL3_WriteRegBuffered (&chip, (Bit16u) a, (Bit8u) b);
            break;
        case 'g':
            sscanf (line + 1, "%u", &a);
            while (a > 0)
            {
                unsigned n = a > 65536 ? 65536 : a, i;
                OPL3_GenerateStream (&chip, buf, n);
                for (i = 0; i < 2 * n; i++)
                {
                    fputc (buf[i] & 0xff, out);
                    fputc ((buf[i] >> 8) & 0xff, out);
                }
                a -= n;
            }
            break;
        default:
            break;
        }
    }
    fclose (out);
    return 0;
}
