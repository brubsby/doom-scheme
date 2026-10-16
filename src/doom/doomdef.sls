;;; doomdef: global constants and enums from doomdef.h. C enums are plain
;;; integers here, since the C code compares and indexes with them (and the
;;; trace prints them).
(library (doom doomdef)
  (export SCREENWIDTH SCREENHEIGHT MAXPLAYERS TICRATE
          shareware registered commercial retail indetermined
          doom doom2 pack_tnt pack_plut none english french german unknown
          GS_LEVEL GS_INTERMISSION GS_FINALE GS_DEMOSCREEN
          MTF_EASY MTF_NORMAL MTF_HARD MTF_AMBUSH
          sk_baby sk_easy sk_medium sk_hard sk_nightmare
          it_bluecard it_yellowcard it_redcard it_blueskull it_yellowskull
          it_redskull NUMCARDS
          wp_fist wp_pistol wp_shotgun wp_chaingun wp_missile wp_plasma wp_bfg
          wp_chainsaw wp_supershotgun NUMWEAPONS wp_nochange
          am_clip am_shell am_cell am_misl NUMAMMO am_noammo
          pw_invulnerability pw_strength pw_invisibility pw_ironfeet pw_allmap
          pw_infrared NUMPOWERS
          INVULNTICS INVISTICS INFRATICS IRONTICS
          KEY_RIGHTARROW KEY_LEFTARROW KEY_UPARROW KEY_DOWNARROW KEY_ESCAPE
          KEY_ENTER KEY_TAB KEY_F1 KEY_F2 KEY_F3 KEY_F4 KEY_F5 KEY_F6 KEY_F7
          KEY_F8 KEY_F9 KEY_F10 KEY_F11 KEY_F12 KEY_BACKSPACE KEY_PAUSE
          KEY_EQUALS KEY_MINUS KEY_RSHIFT KEY_RCTRL KEY_RALT KEY_LALT)
  (import (chezscheme))

  (define-syntax define-enum
    (syntax-rules ()
      [(_ name ...) (define-enum-from 0 name ...)]))
  (define-syntax define-enum-from
    (syntax-rules ()
      [(_ n) (begin)]
      [(_ n name rest ...) (begin (define name n) (define-enum-from (+ n 1) rest ...))]))

  ;; Fixed at vanilla's 320x200 for now. Everything derives from these two;
  ;; see DESIGN.md "Future: hi-res and uncapped".
  (define SCREENWIDTH 320)
  (define SCREENHEIGHT 200)

  (define MAXPLAYERS 4)
  (define TICRATE 35)

  (define-enum shareware registered commercial retail indetermined)   ; GameMode_t
  (define-enum doom doom2 pack_tnt pack_plut none)                    ; GameMission_t
  (define-enum english french german unknown)                         ; Language_t
  (define-enum GS_LEVEL GS_INTERMISSION GS_FINALE GS_DEMOSCREEN)      ; gamestate_t

  ;; Mapthing option flags.
  (define MTF_EASY 1)
  (define MTF_NORMAL 2)
  (define MTF_HARD 4)
  (define MTF_AMBUSH 8)

  (define-enum sk_baby sk_easy sk_medium sk_hard sk_nightmare)        ; skill_t
  (define-enum it_bluecard it_yellowcard it_redcard                   ; card_t
               it_blueskull it_yellowskull it_redskull NUMCARDS)
  (define-enum wp_fist wp_pistol wp_shotgun wp_chaingun wp_missile    ; weapontype_t
               wp_plasma wp_bfg wp_chainsaw wp_supershotgun NUMWEAPONS wp_nochange)
  (define-enum am_clip am_shell am_cell am_misl NUMAMMO am_noammo)    ; ammotype_t
  (define-enum pw_invulnerability pw_strength pw_invisibility         ; powertype_t
               pw_ironfeet pw_allmap pw_infrared NUMPOWERS)

  (define INVULNTICS (* 30 TICRATE))
  (define INVISTICS (* 60 TICRATE))
  (define INFRATICS (* 120 TICRATE))
  (define IRONTICS (* 60 TICRATE))

  ;; Key codes: ASCII for most keys, these for the rest.
  (define KEY_RIGHTARROW #xae)
  (define KEY_LEFTARROW #xac)
  (define KEY_UPARROW #xad)
  (define KEY_DOWNARROW #xaf)
  (define KEY_ESCAPE 27)
  (define KEY_ENTER 13)
  (define KEY_TAB 9)
  (define KEY_F1 (+ #x80 #x3b))
  (define KEY_F2 (+ #x80 #x3c))
  (define KEY_F3 (+ #x80 #x3d))
  (define KEY_F4 (+ #x80 #x3e))
  (define KEY_F5 (+ #x80 #x3f))
  (define KEY_F6 (+ #x80 #x40))
  (define KEY_F7 (+ #x80 #x41))
  (define KEY_F8 (+ #x80 #x42))
  (define KEY_F9 (+ #x80 #x43))
  (define KEY_F10 (+ #x80 #x44))
  (define KEY_F11 (+ #x80 #x57))
  (define KEY_F12 (+ #x80 #x58))
  (define KEY_BACKSPACE 127)
  (define KEY_PAUSE #xff)
  (define KEY_EQUALS #x3d)
  (define KEY_MINUS #x2d)
  (define KEY_RSHIFT (+ #x80 #x36))
  (define KEY_RCTRL (+ #x80 #x1d))
  (define KEY_RALT (+ #x80 #x38))
  (define KEY_LALT KEY_RALT))
