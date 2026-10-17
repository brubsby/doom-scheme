// Prints the 32-bit layouts of the structs p_saveg.c copies into
// savegames byte for byte, as "struct field offset" lines and
// "struct sizeof n" (tests/saveg.ss checks p_saveg's tables against them).

#include <stdio.h>
#include <stddef.h>
#include "doomdef.h"
#include "d_player.h"
#include "p_local.h"
#include "p_spec.h"

#define F(s, f) printf (#s " " #f " %d\n", (int) offsetof (s, f))
#define S(s)    printf (#s " sizeof %d\n", (int) sizeof (s))

int main (void)
{
    S(thinker_t); F(thinker_t, prev); F(thinker_t, next); F(thinker_t, function);
    S(ticcmd_t); F(ticcmd_t, forwardmove); F(ticcmd_t, sidemove); F(ticcmd_t, angleturn);
    F(ticcmd_t, consistancy); F(ticcmd_t, chatchar); F(ticcmd_t, buttons);
    S(pspdef_t); F(pspdef_t, state); F(pspdef_t, tics); F(pspdef_t, sx); F(pspdef_t, sy);
    S(mapthing_t); F(mapthing_t, x); F(mapthing_t, y); F(mapthing_t, angle);
    F(mapthing_t, type); F(mapthing_t, options);

    S(player_t);
    F(player_t, mo); F(player_t, playerstate); F(player_t, cmd); F(player_t, viewz);
    F(player_t, viewheight); F(player_t, deltaviewheight); F(player_t, bob);
    F(player_t, health); F(player_t, armorpoints); F(player_t, armortype);
    F(player_t, powers); F(player_t, cards); F(player_t, backpack); F(player_t, frags);
    F(player_t, readyweapon); F(player_t, pendingweapon); F(player_t, weaponowned);
    F(player_t, ammo); F(player_t, maxammo); F(player_t, attackdown); F(player_t, usedown);
    F(player_t, cheats); F(player_t, refire); F(player_t, killcount); F(player_t, itemcount);
    F(player_t, secretcount); F(player_t, message); F(player_t, damagecount);
    F(player_t, bonuscount); F(player_t, attacker); F(player_t, extralight);
    F(player_t, fixedcolormap); F(player_t, colormap); F(player_t, psprites);
    F(player_t, didsecret);

    S(mobj_t);
    F(mobj_t, thinker); F(mobj_t, x); F(mobj_t, y); F(mobj_t, z); F(mobj_t, snext);
    F(mobj_t, sprev); F(mobj_t, angle); F(mobj_t, sprite); F(mobj_t, frame);
    F(mobj_t, bnext); F(mobj_t, bprev); F(mobj_t, subsector); F(mobj_t, floorz);
    F(mobj_t, ceilingz); F(mobj_t, radius); F(mobj_t, height); F(mobj_t, momx);
    F(mobj_t, momy); F(mobj_t, momz); F(mobj_t, validcount); F(mobj_t, type);
    F(mobj_t, info); F(mobj_t, tics); F(mobj_t, state); F(mobj_t, flags);
    F(mobj_t, health); F(mobj_t, movedir); F(mobj_t, movecount); F(mobj_t, target);
    F(mobj_t, reactiontime); F(mobj_t, threshold); F(mobj_t, player);
    F(mobj_t, lastlook); F(mobj_t, spawnpoint); F(mobj_t, tracer);

    S(ceiling_t);
    F(ceiling_t, thinker); F(ceiling_t, type); F(ceiling_t, sector);
    F(ceiling_t, bottomheight); F(ceiling_t, topheight); F(ceiling_t, speed);
    F(ceiling_t, crush); F(ceiling_t, direction); F(ceiling_t, tag);
    F(ceiling_t, olddirection);

    S(vldoor_t);
    F(vldoor_t, thinker); F(vldoor_t, type); F(vldoor_t, sector); F(vldoor_t, topheight);
    F(vldoor_t, speed); F(vldoor_t, direction); F(vldoor_t, topwait);
    F(vldoor_t, topcountdown);

    S(floormove_t);
    F(floormove_t, thinker); F(floormove_t, type); F(floormove_t, crush);
    F(floormove_t, sector); F(floormove_t, direction); F(floormove_t, newspecial);
    F(floormove_t, texture); F(floormove_t, floordestheight); F(floormove_t, speed);

    S(plat_t);
    F(plat_t, thinker); F(plat_t, sector); F(plat_t, speed); F(plat_t, low);
    F(plat_t, high); F(plat_t, wait); F(plat_t, count); F(plat_t, status);
    F(plat_t, oldstatus); F(plat_t, crush); F(plat_t, tag); F(plat_t, type);

    S(lightflash_t);
    F(lightflash_t, thinker); F(lightflash_t, sector); F(lightflash_t, count);
    F(lightflash_t, maxlight); F(lightflash_t, minlight); F(lightflash_t, maxtime);
    F(lightflash_t, mintime);

    S(strobe_t);
    F(strobe_t, thinker); F(strobe_t, sector); F(strobe_t, count);
    F(strobe_t, minlight); F(strobe_t, maxlight); F(strobe_t, darktime);
    F(strobe_t, brighttime);

    S(glow_t);
    F(glow_t, thinker); F(glow_t, sector); F(glow_t, minlight); F(glow_t, maxlight);
    F(glow_t, direction);
    return 0;
}
