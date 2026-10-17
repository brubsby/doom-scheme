// The rest of Chocolate Doom, as far as i_oplmusic.c, mus2mid.c, midifile.c,
// memio.c and opl/opl.c need it: SDL on one thread, w_wad over one WAD file,
// z_zone as malloc, and the m_misc file helpers.
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

#include "SDL.h"
#include "doomtype.h"
#include "i_system.h"
#include "m_misc.h"
#include "w_wad.h"
#include "z_zone.h"

void music_pump (void);

// SDL: mutexes are no-ops; a condition wait runs the mixer (see music_sdl.c).
static int dummy;
SDL_mutex *SDL_CreateMutex (void) { return (SDL_mutex *) &dummy; }
int SDL_LockMutex (SDL_mutex *m) { return 0; }
int SDL_UnlockMutex (SDL_mutex *m) { return 0; }
void SDL_DestroyMutex (SDL_mutex *m) { }
SDL_cond *SDL_CreateCond (void) { return (SDL_cond *) &dummy; }
int SDL_CondSignal (SDL_cond *c) { return 0; }
int SDL_CondWait (SDL_cond *c, SDL_mutex *m) { music_pump (); return 0; }
void SDL_DestroyCond (SDL_cond *c) { }
int SDL_Init (Uint32 flags) { return 0; }
void SDL_QuitSubSystem (Uint32 flags) { }
void SDL_PauseAudio (int pause_on) { }

// SDL_MixAudioFormat for AUDIO_S16SYS at full volume: saturating add.
void SDL_MixAudioFormat (Uint8 *dst, const Uint8 *src, Uint16 format,
                         Uint32 len, int volume)
{
    Uint32 i;
    for (i = 0; i + 1 < len; i += 2)
    {
        int a = (int16_t) (dst[i] | (dst[i + 1] << 8));
        int b = (int16_t) (src[i] | (src[i + 1] << 8));
        int s = a + b;
        if (s > 32767) s = 32767;
        if (s < -32768) s = -32768;
        dst[i] = s & 0xff;
        dst[i + 1] = (s >> 8) & 0xff;
    }
}

// i_sound.c
int snd_samplerate = 44100;

// i_system.c
void I_Error (const char *error, ...)
{
    va_list ap;
    va_start (ap, error);
    vfprintf (stderr, error, ap);
    va_end (ap);
    fputc ('\n', stderr);
    exit (1);
}

void *I_Realloc (void *ptr, size_t size)
{
    void *p = realloc (ptr, size);
    if (size != 0 && p == NULL)
        I_Error ("I_Realloc: failed on reallocation of %zu bytes", size);
    return p;
}

// z_zone.c
void *Z_Malloc (int size, int tag, void *ptr)
{
    void *p = malloc (size);
    if (p == NULL)
        I_Error ("Z_Malloc: failed on allocation of %i bytes", size);
    return p;
}

void Z_Free (void *ptr) { free (ptr); }

// w_wad.c, over one WAD file loaded whole.
static unsigned char *wad;
static int wad_numlumps;
static int wad_dirofs;

static int le32 (const unsigned char *p)
{
    return p[0] | (p[1] << 8) | (p[2] << 16) | ((unsigned) p[3] << 24);
}

void probe_wad_open (const char *path)
{
    FILE *f = fopen (path, "rb");
    long len;
    if (f == NULL)
        I_Error ("can't open %s", path);
    fseek (f, 0, SEEK_END);
    len = ftell (f);
    fseek (f, 0, SEEK_SET);
    wad = malloc (len);
    if (fread (wad, 1, len, f) != (size_t) len)
        I_Error ("can't read %s", path);
    fclose (f);
    wad_numlumps = le32 (wad + 4);
    wad_dirofs = le32 (wad + 8);
}

lumpindex_t W_CheckNumForName (const char *name)
{
    int i;
    for (i = wad_numlumps - 1; i >= 0; --i)
    {
        if (!strncasecmp ((char *) wad + wad_dirofs + 16 * i + 8, name, 8))
            return i;
    }
    return -1;
}

lumpindex_t W_GetNumForName (const char *name)
{
    lumpindex_t i = W_CheckNumForName (name);
    if (i < 0)
        I_Error ("W_GetNumForName: %s not found!", name);
    return i;
}

int W_LumpLength (lumpindex_t lump)
{
    return le32 (wad + wad_dirofs + 16 * lump + 4);
}

void *W_CacheLumpNum (lumpindex_t lump, int tag)
{
    return wad + le32 (wad + wad_dirofs + 16 * lump);
}

void *W_CacheLumpName (const char *name, int tag)
{
    return W_CacheLumpNum (W_GetNumForName (name), tag);
}

void W_ReleaseLumpNum (lumpindex_t lump) { }
void W_ReleaseLumpName (const char *name) { }

// m_misc.c
FILE *M_fopen (const char *filename, const char *mode)
{
    return fopen (filename, mode);
}

int M_remove (const char *path) { return remove (path); }

boolean M_WriteFile (const char *name, const void *source, int length)
{
    FILE *f = fopen (name, "wb");
    int n;
    if (f == NULL)
        return false;
    n = fwrite (source, 1, length, f);
    fclose (f);
    return n == length;
}

char *M_TempFile (const char *s)
{
    const char *dir = getenv ("TMPDIR");
    char *path;
    if (dir == NULL)
        dir = "/tmp";
    path = malloc (strlen (dir) + strlen (s) + 32);
    sprintf (path, "%s/music_probe.%ld.%s", dir, (long) getpid (), s);
    return path;
}

boolean M_StringConcat (char *dest, const char *src, size_t dest_size)
{
    size_t offset = strlen (dest);
    if (offset > dest_size)
        offset = dest_size;
    snprintf (dest + offset, dest_size - offset, "%s", src);
    return strlen (src) + offset < dest_size;
}

int M_snprintf (char *buf, size_t buf_len, const char *s, ...)
{
    va_list ap;
    int r;
    va_start (ap, s);
    r = vsnprintf (buf, buf_len, s, ap);
    va_end (ap);
    return r;
}

// deh_str.c, with no dehacked patches loaded.
const char *DEH_String (const char *s) { return s; }
