// Chocolate Doom's SDL OPL driver (opl/opl_sdl.c), compiled unchanged against
// the stub SDL headers in this directory, plus two entry points for the
// probe. Including the .c gives them its statics (the queue and clock).
//
// music_mix runs the driver's mixing callback, OPL_Mix_Callback, on a zeroed
// buffer, as SDL_mixer's postmix would on a silent stream: the result is the
// OPL output itself.
//
// music_pump stands in for SDL's audio thread while the main thread waits in
// OPL_Delay (OPL_Detect's 1 ms wait at init): it runs the mixing callback
// over exactly the frames up to the next queued callback, then discards them.
#include "opl_sdl.c"

static void (*registered_effect) (int, void *, int, void *);
static int open_freq;
static int audio_open;

int Mix_RegisterEffect (int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d,
                        void *arg)
{
    registered_effect = f;
    return 1;
}

int Mix_QuerySpec (int *frequency, Uint16 *format, int *channels)
{
    if (!audio_open)
        return 0;
    *frequency = open_freq;
    *format = AUDIO_S16SYS;
    *channels = 2;
    return 1;
}

int Mix_OpenAudioDevice (int frequency, Uint16 format, int channels,
                         int chunksize, const char *device,
                         int allowed_changes)
{
    open_freq = frequency;
    audio_open = 1;
    return 0;
}

void Mix_CloseAudio (void) { audio_open = 0; }
const char *Mix_GetError (void) { return "no error"; }
void Mix_HookMusic (void (*mix_func) (void *, Uint8 *, int), void *arg) { }

void music_mix (uint8_t *buf, unsigned int frames)
{
    memset (buf, 0, frames * 4);
    registered_effect (MIX_CHANNEL_POST, buf, frames * 4, NULL);
}

void music_pump (void)
{
    uint64_t n;
    uint8_t *buf;

    assert (!opl_sdl_paused && !OPL_Queue_IsEmpty (callback_queue));
    n = (OPL_Queue_Peek (callback_queue) + pause_offset - current_time)
        * mixing_freq;
    n = (n + OPL_SECOND - 1) / OPL_SECOND;
    assert (n > 0 && n < (uint64_t) mixing_freq);
    buf = malloc (n * 4);
    music_mix (buf, (unsigned int) n);
    free (buf);
}
