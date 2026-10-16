// per-tic state trace for the linuxdoom-1.10 oracle.
//
// oracle_tic() runs once per loop iteration of D_DoomLoop, after D_Display,
// so under -timedemo (singletics) there is exactly one line per game tic.
// The Scheme port must emit byte-identical lines; see TRACE FORMAT below.
//
// Environment:
//   ORACLE_TRACE=path        write the trace here (required)
//   ORACLE_PPM_DIR=dir       also dump screens[0] as dir/tNNNNN.ppm ...
//   ORACLE_PPM_EVERY=n       ... every n tics (default 35)
//
// TRACE FORMAT (one line per tic, space separated, decimal):
//   gametic gamestate leveltime prndindex rndindex
//   px py pz pangle pmomx pmomy health armor   (player 0; zeros if no mobj)
//   nmobjs mobjhash sectorhash framehash palettehash
//
// Hashes are FNV-1a over 32-bit words: h = 2166136261; for each word w:
// h ^= (uint32)w; h *= 16777619. Signed values are taken as two's complement.
//   mobjhash:   for each mobj in thinker-list order:
//               x y z angle momx momy momz type (state-states) tics health flags
//   sectorhash: for each sector: floorheight ceilingheight lightlevel special
//   framehash:  screens[0] as 16000 little-endian uint32 words
//   palettehash: the 768-byte palette last passed to I_SetPalette, as 192
//               little-endian uint32 words

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "doomdef.h"
#include "doomstat.h"
#include "d_player.h"
#include "p_local.h"
#include "m_random.h"
#include "info.h"
#include "r_state.h"
#include "v_video.h"

extern byte oracle_palette[768];
extern int prndindex, rndindex;

static FILE* tracefile;
static char* ppmdir;
static int   ppmevery = 35;

typedef uint32_t u32;
#define FNV_INIT 2166136261u
static u32 fnv (u32 h, int32_t w) { h ^= (u32) w; return h * 16777619u; }

static u32 words_hash (const byte* p, int nbytes)
{
    u32 h = FNV_INIT;
    int i;
    for (i = 0; i < nbytes; i += 4)
        h = fnv (h, p[i] | (p[i+1] << 8) | (p[i+2] << 16) | ((u32) p[i+3] << 24));
    return h;
}

static void dump_ppm (void)
{
    char path[1024];
    FILE* f;
    int i;
    snprintf (path, sizeof path, "%s/t%05d.ppm", ppmdir, gametic);
    f = fopen (path, "wb");
    if (!f) return;
    fprintf (f, "P6\n%d %d\n255\n", SCREENWIDTH, SCREENHEIGHT);
    for (i = 0; i < SCREENWIDTH*SCREENHEIGHT; i++)
        fwrite (&oracle_palette[screens[0][i]*3], 1, 3, f);
    fclose (f);
}

void oracle_tic (void)
{
    thinker_t* th;
    player_t* p = &players[0];
    mobj_t* mo = p->mo;
    u32 mh = FNV_INIT, sh = FNV_INIT;
    int nmobjs = 0, i;

    if (!tracefile)
    {
        char* path = getenv ("ORACLE_TRACE");
        if (!path) { fprintf (stderr, "ORACLE_TRACE not set\n"); exit (2); }
        tracefile = fopen (path, "w");
        ppmdir = getenv ("ORACLE_PPM_DIR");
        if (getenv ("ORACLE_PPM_EVERY")) ppmevery = atoi (getenv ("ORACLE_PPM_EVERY"));
    }

    if (gamestate == GS_LEVEL)
    {
        for (th = thinkercap.next; th != &thinkercap; th = th->next)
        {
            mobj_t* m;
            if (th->function.acp1 != (actionf_p1) P_MobjThinker)
                continue;
            m = (mobj_t*) th;
            nmobjs++;
            mh = fnv (mh, m->x);     mh = fnv (mh, m->y);    mh = fnv (mh, m->z);
            mh = fnv (mh, m->angle); mh = fnv (mh, m->momx); mh = fnv (mh, m->momy);
            mh = fnv (mh, m->momz);  mh = fnv (mh, m->type);
            mh = fnv (mh, m->state - states);
            mh = fnv (mh, m->tics);  mh = fnv (mh, m->health); mh = fnv (mh, m->flags);
        }
        for (i = 0; i < numsectors; i++)
        {
            sh = fnv (sh, sectors[i].floorheight);
            sh = fnv (sh, sectors[i].ceilingheight);
            sh = fnv (sh, sectors[i].lightlevel);
            sh = fnv (sh, sectors[i].special);
        }
    }

    fprintf (tracefile, "%d %d %d %d %d %d %d %d %u %d %d %d %d %d %u %u %u %u\n",
             gametic, gamestate, leveltime, prndindex, rndindex,
             mo ? mo->x : 0, mo ? mo->y : 0, mo ? mo->z : 0,
             mo ? (unsigned) mo->angle : 0u, mo ? mo->momx : 0, mo ? mo->momy : 0,
             p->health, p->armorpoints,
             nmobjs, mh, sh,
             words_hash (screens[0], SCREENWIDTH*SCREENHEIGHT),
             words_hash (oracle_palette, 768));
    fflush (tracefile);

    if (ppmdir && ppmevery > 0 && gametic % ppmevery == 0)
        dump_ppm ();
}
