// Headless replacement for linuxdoom's X11 i_video.c.
// Rendering still happens into screens[0]; nothing is displayed.

#include <string.h>
#include "doomdef.h"
#include "v_video.h"
#include "i_video.h"

byte oracle_palette[768];

void I_InitGraphics (void) {}
void I_ShutdownGraphics (void) {}
void I_SetPalette (byte* palette) { memcpy (oracle_palette, palette, 768); }
void I_UpdateNoBlit (void) {}
void I_FinishUpdate (void) {}
void I_ReadScreen (byte* scr) { memcpy (scr, screens[0], SCREENWIDTH*SCREENHEIGHT); }
void I_StartFrame (void) {}
void I_StartTic (void) {}
