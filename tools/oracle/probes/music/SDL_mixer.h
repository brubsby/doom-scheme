/* Stand-in for SDL_mixer.h, enough for opl/opl_sdl.c. */
#ifndef PROBE_SDL_MIXER_H
#define PROBE_SDL_MIXER_H
#include "SDL.h"

#define MIX_CHANNEL_POST (-2)
#define SDL_AUDIO_ALLOW_FREQUENCY_CHANGE 0x01

typedef void (*Mix_EffectFunc_t) (int chan, void *stream, int len, void *udata);
typedef void (*Mix_EffectDone_t) (int chan, void *udata);

int Mix_QuerySpec (int *frequency, Uint16 *format, int *channels);
int Mix_OpenAudioDevice (int frequency, Uint16 format, int channels,
                         int chunksize, const char *device,
                         int allowed_changes);
void Mix_CloseAudio (void);
const char *Mix_GetError (void);
void Mix_HookMusic (void (*mix_func) (void *, Uint8 *, int), void *arg);
int Mix_RegisterEffect (int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d,
                        void *arg);
#endif
