/* Stand-in for SDL_endian.h: little-endian host only. */
#ifndef PROBE_SDL_ENDIAN_H
#define PROBE_SDL_ENDIAN_H
#include <stdint.h>
#define SDL_LIL_ENDIAN 1234
#define SDL_BIG_ENDIAN 4321
#define SDL_BYTEORDER SDL_LIL_ENDIAN
#define SDL_SwapLE16(x) ((uint16_t) (x))
#define SDL_SwapLE32(x) ((uint32_t) (x))
#define SDL_SwapBE16(x) ((uint16_t) __builtin_bswap16 ((uint16_t) (x)))
#define SDL_SwapBE32(x) ((uint32_t) __builtin_bswap32 ((uint32_t) (x)))
#endif
