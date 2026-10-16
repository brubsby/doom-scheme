// Dumps the oracle's compiled lookup tables and SlopeDiv samples, one value
// per line as "name index value", for tests/tables.ss.
#include <stdio.h>
#include <stdint.h>

extern int finesine[10240], finetangent[4096];
extern unsigned tantoangle[2049];
extern unsigned char rndtable[256];
extern unsigned char gammatable[5][256];
int SlopeDiv (unsigned num, unsigned den);

// Link stubs for v_video.o (which holds gammatable).
unsigned char* I_AllocLow (int n) { return 0; }
void M_AddToBox (int* b, int x, int y) {}
void I_Error (char* e, ...) {}

static uint64_t s = 1234567;
static uint32_t rnd (void) { s ^= s << 13; s ^= s >> 7; s ^= s << 17; return (uint32_t) s; }

int main (void)
{
    int i;
    for (i = 0; i < 10240; i++) printf ("finesine %d %d\n", i, finesine[i]);
    for (i = 0; i < 4096; i++) printf ("finetangent %d %d\n", i, finetangent[i]);
    for (i = 0; i < 2049; i++) printf ("tantoangle %d %u\n", i, tantoangle[i]);
    for (i = 0; i < 256; i++) printf ("rndtable %d %d\n", i, rndtable[i]);
    for (i = 0; i < 1280; i++) printf ("gammatable %d %d\n", i, gammatable[i / 256][i % 256]);
    for (i = 0; i < 20000; i++)
    {
        unsigned num = rnd () >> (rnd () % 32), den = rnd () >> (rnd () % 32);
        printf ("slopediv %u %u %d\n", num, den, SlopeDiv (num, den));
    }
    return 0;
}
