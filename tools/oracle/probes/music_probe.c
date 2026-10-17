// Runs Chocolate Doom's OPL music player over a song, for cross-checking the
// Scheme port (tests/music.ss). It links the real chocolate-doom sources:
// src/i_oplmusic.c, src/mus2mid.c, src/midifile.c, src/memio.c, opl/opl.c,
// opl/opl_queue.c, opl/opl3.c and opl/opl_sdl.c (through music/music_sdl.c),
// with the stubs in music/ standing in for SDL and the rest of the game.
// Build (tests/music.ss does this into build/music/):
//   cc -O2 -I tools/oracle/probes/music -I $C/src -I $C/opl
//      -o build/music/music_probe tools/oracle/probes/music_probe.c
//      tools/oracle/probes/music/music_sdl.c tools/oracle/probes/music/stubs.c
//      $C/src/i_oplmusic.c $C/src/mus2mid.c $C/src/midifile.c $C/src/memio.c
//      $C/opl/opl.c $C/opl/opl_queue.c $C/opl/opl3.c
//   with C=reference/chocolate-doom.
//
// Usage: music_probe WAD LUMP RATE FRAMES LOOP VOLUME OUT [SCRIPT]
//   Initializes the music module at RATE Hz (snd_samplerate), sets the music
//   volume, registers LUMP (or, if LUMP contains a '/', the file at that path)
//   and plays it with looping LOOP (0/1), then renders FRAMES stereo frames
//   to OUT as raw s16le. DMXOPTION in the environment is read as Chocolate
//   reads it (e.g. "-opl3", "-opl3 -reverse"). OPL_DRV_VER, if set, is
//   passed to I_SetOPLDriverVer: 0 Doom 1 1.666, 1 Doom 2 1.666, 2 Doom 1.9
//   (the default).
//   The mixing callback runs on buffers of 4096 frames (SDL's slice for
//   these rates), split at script events.
// Script: one command per line, '#' starts a comment line. FRAME is the
// output frame at which the command runs; FRAMEs must not decrease.
//   FRAME pause | resume | stop
//   FRAME volume V           I_OPL_SetMusicVolume(V)
//   FRAME play LUMP LOOP     stop, unregister, register LUMP, play
//   FRAME chunk N            mixing callback buffer size from here on
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "doomtype.h"
#include "i_sound.h"
#include "w_wad.h"

extern const music_module_t music_opl_module;
void probe_wad_open (const char *path);
void music_mix (uint8_t *buf, unsigned int frames);

static void *ReadSong (const char *name, int *len)
{
    if (strchr (name, '/') != NULL)
    {
        FILE *f = fopen (name, "rb");
        void *data;
        if (f == NULL)
        {
            perror (name);
            exit (2);
        }
        fseek (f, 0, SEEK_END);
        *len = ftell (f);
        fseek (f, 0, SEEK_SET);
        data = malloc (*len + 1);
        if (fread (data, 1, *len, f) != (size_t) *len)
        {
            perror (name);
            exit (2);
        }
        fclose (f);
        return data;
    }
    *len = W_LumpLength (W_GetNumForName (name));
    return W_CacheLumpName (name, 1);
}

static void *Start (const char *name, int looping)
{
    int len;
    void *data = ReadSong (name, &len);
    void *handle = music_opl_module.RegisterSong (data, len);
    if (handle == NULL)
        fprintf (stderr, "music_probe: %s did not register\n", name);
    music_opl_module.PlaySong (handle, looping);
    return handle;
}

int main (int argc, char **argv)
{
    FILE *out, *script = NULL;
    unsigned long frames, pos = 0, chunk = 4096;
    uint8_t *buf;
    void *handle;
    char line[256];

    if (argc != 8 && argc != 9)
    {
        fprintf (stderr, "usage: music_probe WAD LUMP RATE FRAMES LOOP VOLUME "
                         "OUT [SCRIPT]\n");
        return 2;
    }
    probe_wad_open (argv[1]);
    snd_samplerate = atoi (argv[3]);
    frames = strtoul (argv[4], NULL, 10);
    if (argc == 9 && (script = fopen (argv[8], "r")) == NULL)
    {
        perror (argv[8]);
        return 2;
    }
    if ((out = fopen (argv[7], "wb")) == NULL)
    {
        perror (argv[7]);
        return 2;
    }

    if (getenv ("OPL_DRV_VER") != NULL)
        I_SetOPLDriverVer ((opl_driver_ver_t) atoi (getenv ("OPL_DRV_VER")));
    if (!music_opl_module.Init ())
    {
        fprintf (stderr, "music_probe: init failed\n");
        return 1;
    }
    music_opl_module.SetMusicVolume (atoi (argv[6]));
    handle = Start (argv[2], atoi (argv[5]));

    buf = malloc (4 * frames + 4);
    for (;;)
    {
        unsigned long at = frames;
        char cmd[32] = "", arg[200] = "";
        int n = 0;

        // Next script command, if any.
        while (script != NULL && fgets (line, sizeof line, script))
        {
            if (line[0] == '#' || line[0] == '\n')
                continue;
            n = sscanf (line, "%lu %31s %199s", &at, cmd, arg);
            if (n < 2)
            {
                fprintf (stderr, "music_probe: bad script line: %s", line);
                return 2;
            }
            break;
        }
        if (at < pos || at > frames)
        {
            fprintf (stderr, "music_probe: script frame %lu out of order\n", at);
            return 2;
        }
        while (pos < at)
        {
            unsigned long k = at - pos < chunk ? at - pos : chunk;
            music_mix (buf + 4 * pos, k);
            pos += k;
        }
        if (n < 2)
            break;

        if (!strcmp (cmd, "pause"))
            music_opl_module.PauseMusic ();
        else if (!strcmp (cmd, "resume"))
            music_opl_module.ResumeMusic ();
        else if (!strcmp (cmd, "stop"))
            music_opl_module.StopSong ();
        else if (!strcmp (cmd, "volume"))
            music_opl_module.SetMusicVolume (atoi (arg));
        else if (!strcmp (cmd, "chunk"))
            chunk = strtoul (arg, NULL, 10);
        else if (!strcmp (cmd, "play"))
        {
            unsigned long skip;
            int looping = 0;
            sscanf (line, "%lu %*s %*s %d", &skip, &looping);
            music_opl_module.StopSong ();
            music_opl_module.UnRegisterSong (handle);
            handle = Start (arg, looping);
        }
        else
        {
            fprintf (stderr, "music_probe: unknown command %s\n", cmd);
            return 2;
        }
    }
    if (fwrite (buf, 4, frames, out) != frames)
    {
        perror (argv[7]);
        return 2;
    }
    fclose (out);
    return 0;
}
