/* Stand-in for SDL.h, enough for opl/opl.c and opl/opl_sdl.c. Everything
   runs on one thread, so mutexes do nothing. A condition wait is where the
   main thread would sleep while SDL's audio thread runs the mixer: here it
   runs the mixer itself (music_pump in music_sdl.c). */
#ifndef PROBE_SDL_H
#define PROBE_SDL_H
#include <stdint.h>
#include "SDL_endian.h"

typedef uint8_t Uint8;
typedef uint16_t Uint16;
typedef uint32_t Uint32;
typedef int16_t Sint16;
typedef struct SDL_mutex SDL_mutex;
typedef struct SDL_cond SDL_cond;

#define SDL_INIT_AUDIO 0x10
#define AUDIO_S16SYS 0x8010
#define SDL_MIX_MAXVOLUME 128

SDL_mutex *SDL_CreateMutex (void);
int SDL_LockMutex (SDL_mutex *m);
int SDL_UnlockMutex (SDL_mutex *m);
void SDL_DestroyMutex (SDL_mutex *m);
SDL_cond *SDL_CreateCond (void);
int SDL_CondSignal (SDL_cond *c);
int SDL_CondWait (SDL_cond *c, SDL_mutex *m);
void SDL_DestroyCond (SDL_cond *c);
int SDL_Init (Uint32 flags);
void SDL_QuitSubSystem (Uint32 flags);
void SDL_PauseAudio (int pause_on);
void SDL_MixAudioFormat (Uint8 *dst, const Uint8 *src, Uint16 format,
                         Uint32 len, int volume);
#endif
