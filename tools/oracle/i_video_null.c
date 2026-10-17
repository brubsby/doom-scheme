// Headless replacement for linuxdoom's X11 i_video.c.
// Rendering still happens into screens[0]; nothing is displayed.

#include <string.h>
#include "doomdef.h"
#include "v_video.h"
#include "i_video.h"
#include "i_system.h"
#include "doomstat.h"

byte oracle_palette[768];

void I_InitGraphics (void) {}
void I_ShutdownGraphics (void) {}
void I_SetPalette (byte* palette) { memcpy (oracle_palette, palette, 768); }
void I_UpdateNoBlit (void) {}
// I_FinishUpdate keeps i_video.c's -devparm tic-rate dots (they draw into
// screens[0] and call I_GetTime).
void I_FinishUpdate (void)
{
    static int	lasttic;
    int		tics;
    int		i;

    // draws little dots on the bottom of the screen
    if (devparm)
    {

	i = I_GetTime();
	tics = i - lasttic;
	lasttic = i;
	if (tics > 20) tics = 20;

	for (i=0 ; i<tics*2 ; i+=2)
	    screens[0][ (SCREENHEIGHT-1)*SCREENWIDTH + i] = 0xff;
	for ( ; i<20*2 ; i+=2)
	    screens[0][ (SCREENHEIGHT-1)*SCREENWIDTH + i] = 0x0;
    
    }
}
void I_ReadScreen (byte* scr) { memcpy (scr, screens[0], SCREENWIDTH*SCREENHEIGHT); }
void I_StartFrame (void) {}
void I_StartTic (void) {}
