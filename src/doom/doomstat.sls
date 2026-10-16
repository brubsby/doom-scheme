;;; doomstat: game-wide state shared between modules (doomstat.h). In C these
;;; are defined across d_main.c, g_game.c, p_setup.c and others; collecting
;;; them in the header's library keeps the import graph acyclic.
;;; Only what the ported modules use so far is here.
(library (doom doomstat)
  (export gamemode gamemission modifiedgame
          nomonsters respawnparm fastparm devparm
          startskill startepisode startmap autostart
          gameskill gameepisode gamemap respawnmonsters netgame deathmatch
          statusbaractive automapactive menuactive paused viewactive
          consoleplayer displayplayer
          totalkills totalitems totalsecret levelstarttic leveltime
          usergame demoplayback demorecording singledemo
          gamestate gametic
          players playeringame
          MAX_DM_STARTS deathmatchstarts deathmatch_p playerstarts
          precache bodyqueslot maxammo gameaction secretexit)
  (import (chezscheme) (doom global) (doom doomdef) (doom doomdata) (doom d_player)
          (doom d_event))

  (define-global gamemode indetermined)
  (define-global gamemission doom)
  ;; Set if a PWAD was added (sprite lumps are then looked up by name).
  (define-global modifiedgame #f)

  (define-global nomonsters #f)
  (define-global respawnparm #f)
  (define-global fastparm #f)
  (define-global devparm #f)

  (define-global startskill sk_medium)
  (define-global startepisode 1)
  (define-global startmap 1)
  (define-global autostart #f)

  (define-global gameskill sk_medium)
  (define-global gameepisode 1)
  (define-global gamemap 1)
  (define-global respawnmonsters #f)
  (define-global netgame #f)
  (define-global deathmatch 0)         ; 0, 1, or 2 (altdeath)

  (define-global statusbaractive #f)
  (define-global automapactive #f)
  (define-global menuactive #f)
  (define-global paused #f)
  (define-global viewactive #t)

  (define-global consoleplayer 0)
  (define-global displayplayer 0)

  (define-global totalkills 0)
  (define-global totalitems 0)
  (define-global totalsecret 0)
  (define-global levelstarttic 0)
  (define-global leveltime 0)

  (define-global usergame #f)
  (define-global demoplayback #f)
  (define-global demorecording #f)
  (define-global singledemo #f)

  (define-global gamestate GS_DEMOSCREEN)
  (define-global gametic 0)

  (define players (vector (make-player) (make-player) (make-player) (make-player)))
  (define playeringame (make-vector MAXPLAYERS #f))

  ;; deathmatch_p is an index into deathmatchstarts (C: a pointer into it).
  (define MAX_DM_STARTS 10)
  (define deathmatchstarts (make-vector MAX_DM_STARTS #f))
  (define-global deathmatch_p 0)
  (define playerstarts (make-vector MAXPLAYERS #f))

  (define-global precache #t)
  (define-global bodyqueslot 0)

  ;; Defined in g_game.c; here because the playsim's level exits set them.
  (define-global gameaction ga_nothing)
  (define-global secretexit #f)

  ;; Ammo limits without a backpack (defined in p_inter.c).
  (define maxammo (fxvector 200 50 300 50)))
