// Headless replacement for linuxdoom's OSS i_sound.c.
// Keeps the parts s_sound.c depends on (lump lookup, handle allocation).

#include <stdio.h>
#include "doomdef.h"
#include "doomstat.h"
#include "sounds.h"
#include "w_wad.h"
#include "i_sound.h"

// m_misc.c lists this among its config defaults.
char* sndserver_filename = "./sndserver ";

static int next_handle;

void I_InitSound (void) {}
void I_UpdateSound (void) {}
void I_SubmitSound (void) {}
void I_ShutdownSound (void) {}
void I_SetChannels (void) {}

int I_GetSfxLumpNum (sfxinfo_t* sfx)
{
    char namebuf[9];
    sprintf (namebuf, "ds%s", sfx->name);
    return W_GetNumForName (namebuf);
}

int I_StartSound (int id, int vol, int sep, int pitch, int priority)
{
    return next_handle++;
}
void I_StopSound (int handle) {}
int  I_SoundIsPlaying (int handle) { return 0; }
void I_UpdateSoundParams (int handle, int vol, int sep, int pitch) {}

void I_InitMusic (void) {}
void I_ShutdownMusic (void) {}
void I_SetMusicVolume (int volume) {}
void I_PauseSong (int handle) {}
void I_ResumeSong (int handle) {}
int  I_RegisterSong (void* data) { return 1; }
void I_PlaySong (int handle, int looping) {}
void I_StopSong (int handle) {}
void I_UnRegisterSong (int handle) {}
