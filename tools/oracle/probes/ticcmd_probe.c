// G_BuildTiccmd reference: runs the oracle's own G_BuildTiccmd on scripted
// input. Linked against all the oracle's objects except i_main.o.
//
// stdin, one line per tic:  keys mousex mousey mousebuttons joyx joyy joybuttons
//   keys: comma-separated decimal key codes held down this tic, or "-"
//   (mousex/mousey are consumed each tic, as in the game)
// stdout, one line per tic: forwardmove sidemove angleturn buttons chatchar
// Key bindings and mouse settings are the defaults (M_LoadDefaults with no
// config file).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doomdef.h"
#include "d_ticcmd.h"
#include "m_argv.h"
#include "m_misc.h"
#include "doomstat.h"

void G_BuildTiccmd (ticcmd_t* cmd);
extern boolean gamekeydown[256];
extern int mousex, mousey, joyxmove, joyymove;
extern boolean* mousebuttons;
extern boolean* joybuttons;
extern char basedefault[1024];
extern int ticdup;

int main (int argc, char** argv)
{
    char line[4096], keys[4000];
    int mx, my, mb, jx, jy, jb;
    myargc = argc;
    myargv = argv;
    strcpy (basedefault, "/nonexistent/.doomrc");
    ticdup = 1;                 // as D_CheckNetGame sets it for one player
    M_LoadDefaults ();
    while (fgets (line, sizeof line, stdin))
    {
        ticcmd_t cmd;
        char* k;
        if (sscanf (line, "%s %d %d %d %d %d %d", keys, &mx, &my, &mb, &jx, &jy, &jb) != 7)
            break;
        memset (gamekeydown, 0, 256 * sizeof (boolean));
        if (strcmp (keys, "-"))
            for (k = strtok (keys, ","); k; k = strtok (NULL, ","))
                gamekeydown[atoi (k)] = true;
        mousex = mx;
        mousey = my;
        mousebuttons[0] = mb & 1; mousebuttons[1] = mb & 2; mousebuttons[2] = mb & 4;
        joyxmove = jx;
        joyymove = jy;
        joybuttons[0] = jb & 1; joybuttons[1] = jb & 2; joybuttons[2] = jb & 4; joybuttons[3] = jb & 8;
        G_BuildTiccmd (&cmd);
        printf ("%d %d %d %d %d\n", cmd.forwardmove, cmd.sidemove, cmd.angleturn,
                cmd.buttons, cmd.chatchar);
    }
    return 0;
}
