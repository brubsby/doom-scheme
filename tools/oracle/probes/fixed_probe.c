// Prints FixedMul/FixedDiv results from the oracle's own m_fixed.o, for
// cross-checking the Scheme port (tests/fixed-cases.txt).
// Usage: fixed_probe [n-random]   -> lines "a b FixedMul(a,b) FixedDiv(a,b)"
// Includes adversarial FixedDiv cases where a*65536/b sits just below an
// integer, which is where x87 extended vs. double precision can disagree.
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

typedef int fixed_t;
fixed_t FixedMul (fixed_t a, fixed_t b);
fixed_t FixedDiv (fixed_t a, fixed_t b);
static int div_errors;
void I_Error (char* fmt, ...) { div_errors++; }

static uint64_t s = 88172645463325252ull;
static uint32_t rnd (void) { s ^= s << 13; s ^= s >> 7; s ^= s << 17; return (uint32_t) s; }

static void emit (int32_t a, int32_t b)
{
    int e = div_errors;
    fixed_t d = b ? FixedDiv (a, b) : 0;
    printf ("%d %d %d %d%s\n", a, b, FixedMul (a, b), d, div_errors != e ? " E" : "");
}

int main (int argc, char** argv)
{
    int n = argc > 1 ? atoi (argv[1]) : 100000, i;
    int32_t edge[] = { 0, 1, -1, 2, -2, 65535, 65536, -65536, 65537, 0x3fff, 0x4000,
                       0x7fffffff, -0x7fffffff - 1, 0x7fff0000, 123456789, -987654321 };
    int ne = sizeof edge / sizeof edge[0], j;
    for (i = 0; i < ne; i++) for (j = 0; j < ne; j++) emit (edge[i], edge[j]);
    for (i = 0; i < n; i++)
    {
        // Mix of magnitudes: full 32-bit, map-coordinate-ish, and small.
        int shift_a = rnd () % 31, shift_b = rnd () % 31;
        emit ((int32_t) rnd () >> shift_a, (int32_t) rnd () >> shift_b);
    }
    for (i = 0; i < n; i++)
    {
        // a*65536 = k*b - r for small r: quotient just below integer k.
        int64_t b = (rnd () & 0x7fffffff) | 1, k, a, r = 1 + rnd () % 3;
        if (b < 65536) b += 65536;
        k = 1 + rnd () % ((1ll << 31) / ((b >> 14) + 1) + 1);
        a = (k * b - r) >> 16;
        if (a > 0x7fffffff) continue;
        emit ((int32_t) a, (int32_t) b);
        emit ((int32_t) -a, (int32_t) b);
    }
    return 0;
}
