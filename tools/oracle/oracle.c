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
//   ORACLE_MAX_FRAMES=n      exit(0) after n frames (e.g. to grab the title
//                            screen without -timedemo)
//   ORACLE_RAW_DIR=dir       dump screens[0] (raw palette indices, 64000
//                            bytes) after every frame as dir/fNNNN.raw,
//                            frames counted from 1
//   ORACLE_KEYS=script       after frame F, post a keydown+keyup for each key:
//                            "F:k,k,...;F:k,..." with decimal doom key codes
//   ORACLE_LEVELDUMP=path    P_SetupLevel writes the loaded level's structures
//                            here (see oracle_level_dump)
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
#include "d_main.h"
#include "w_wad.h"
#include "doomdata.h"
#include <string.h>

extern byte oracle_palette[768];
extern int prndindex, rndindex;

static FILE* tracefile;
static char* ppmdir;
static int   ppmevery = 35;
static int   maxframes = -1, frames;
static char* rawdir;
static char* keyscript;

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

static void dump_raw (void)
{
    char path[1024];
    FILE* f;
    snprintf (path, sizeof path, "%s/f%04d.raw", rawdir, frames);
    f = fopen (path, "wb");
    if (!f) return;
    fwrite (screens[0], 1, SCREENWIDTH*SCREENHEIGHT, f);
    fclose (f);
}

// Post the keys scripted for after this frame.
static void post_keys (void)
{
    char* s = keyscript;
    while (s && *s)
    {
        int f = strtol (s, &s, 10);
        if (*s++ != ':') { fprintf (stderr, "bad ORACLE_KEYS\n"); exit (2); }
        for (;;)
        {
            int k = strtol (s, &s, 10);
            if (f == frames)
            {
                event_t ev;
                ev.data2 = ev.data3 = 0;
                ev.data1 = k;
                ev.type = ev_keydown; D_PostEvent (&ev);
                ev.type = ev_keyup;   D_PostEvent (&ev);
            }
            if (*s != ',') break;
            s++;
        }
        if (*s == ';') s++;
    }
}

// The level as P_SetupLevel left it after P_GroupLines, one record per line,
// pointers written as array indices (-1 for NULL). tests/level.ss emits the
// same text from the Scheme port and diffs it.
void oracle_level_dump (int episode, int map, int lumpnum)
{
    char* path = getenv ("ORACLE_LEVELDUMP");
    FILE* f;
    int i, j, k;
    u32 h;
#define IDX(p, base) ((p) ? (int) ((p) - (base)) : -1)
    if (!path) return;
    f = fopen (path, "w");
    if (!f) return;
    fprintf (f, "level %d %d\n", episode, map);
    fprintf (f, "vertexes %d\n", numvertexes);
    for (i = 0; i < numvertexes; i++)
        fprintf (f, "v %d %d\n", vertexes[i].x, vertexes[i].y);
    fprintf (f, "sectors %d\n", numsectors);
    for (i = 0; i < numsectors; i++)
    {
        sector_t* s = &sectors[i];
        fprintf (f, "s %d %d %d %d %d %d %d bb %d %d %d %d org %d %d lines %d:",
                 s->floorheight, s->ceilingheight, s->floorpic, s->ceilingpic,
                 s->lightlevel, s->special, s->tag,
                 s->blockbox[0], s->blockbox[1], s->blockbox[2], s->blockbox[3],
                 s->soundorg.x, s->soundorg.y, s->linecount);
        for (j = 0; j < s->linecount; j++)
            fprintf (f, " %d", IDX (s->lines[j], lines));
        fprintf (f, "\n");
    }
    fprintf (f, "sides %d\n", numsides);
    for (i = 0; i < numsides; i++)
        fprintf (f, "d %d %d %d %d %d %d\n", sides[i].textureoffset, sides[i].rowoffset,
                 sides[i].toptexture, sides[i].bottomtexture, sides[i].midtexture,
                 IDX (sides[i].sector, sectors));
    fprintf (f, "lines %d\n", numlines);
    for (i = 0; i < numlines; i++)
    {
        line_t* l = &lines[i];
        fprintf (f, "l %d %d %d %d %d %d %d %d %d bb %d %d %d %d %d %d %d\n",
                 IDX (l->v1, vertexes), IDX (l->v2, vertexes), l->dx, l->dy,
                 l->flags, l->special, l->tag, l->sidenum[0], l->sidenum[1],
                 l->bbox[0], l->bbox[1], l->bbox[2], l->bbox[3], l->slopetype,
                 IDX (l->frontsector, sectors), IDX (l->backsector, sectors));
    }
    fprintf (f, "subsectors %d\n", numsubsectors);
    for (i = 0; i < numsubsectors; i++)
        fprintf (f, "ss %d %d %d\n", IDX (subsectors[i].sector, sectors),
                 subsectors[i].numlines, subsectors[i].firstline);
    fprintf (f, "nodes %d\n", numnodes);
    for (i = 0; i < numnodes; i++)
    {
        node_t* n = &nodes[i];
        fprintf (f, "n %d %d %d %d", n->x, n->y, n->dx, n->dy);
        for (j = 0; j < 2; j++)
            for (k = 0; k < 4; k++)
                fprintf (f, " %d", n->bbox[j][k]);
        fprintf (f, " %d %d\n", n->children[0], n->children[1]);
    }
    fprintf (f, "segs %d\n", numsegs);
    for (i = 0; i < numsegs; i++)
    {
        seg_t* g = &segs[i];
        fprintf (f, "g %d %d %d %u %d %d %d %d\n",
                 IDX (g->v1, vertexes), IDX (g->v2, vertexes), g->offset,
                 (unsigned) g->angle, IDX (g->sidedef, sides), IDX (g->linedef, lines),
                 IDX (g->frontsector, sectors), IDX (g->backsector, sectors));
    }
    // Whole lumps, one hash word per short / byte.
    h = FNV_INIT;
    for (i = 0; i < W_LumpLength (lumpnum + ML_BLOCKMAP) / 2; i++)
        h = fnv (h, blockmaplump[i]);
    fprintf (f, "blockmap %d %d %d %d %u\n", bmaporgx, bmaporgy, bmapwidth, bmapheight, h);
    h = FNV_INIT;
    for (i = 0; i < W_LumpLength (lumpnum + ML_REJECT); i++)
        h = fnv (h, rejectmatrix[i]);
    fprintf (f, "reject %d %u\n", W_LumpLength (lumpnum + ML_REJECT), h);
    fprintf (f, "end\n");
    fclose (f);
#undef IDX
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
        if (getenv ("ORACLE_MAX_FRAMES")) maxframes = atoi (getenv ("ORACLE_MAX_FRAMES"));
        rawdir = getenv ("ORACLE_RAW_DIR");
        keyscript = getenv ("ORACLE_KEYS");
    }
    frames++;

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

    if (rawdir)
        dump_raw ();
    post_keys ();

    if (maxframes >= 0 && frames >= maxframes)
        exit (0);
}
