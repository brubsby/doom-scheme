;;; d_main: the game: startup, the main loop, timing, input to ticcmds,
;;; game flow, and the front end (status bar, heads-up text, menus,
;;; intermission, finale).
;;;
;;; d_main.c, d_net.c and g_game.c call each other in a cycle, and the
;;; front end's files join it (the menu starts games, the intermission and
;;; finale end them, the status bar's cheats change level), so like the
;;; renderer and playsim this is one library with an included file per C
;;; file. The widget libraries (st_lib, hu_lib) and the wipe (f_wipe) are
;;; below it.
(library (doom d_main)
  (export
   ;; d_main
   d-run d-doom-main d-post-event d-process-events d-advance-demo d-start-title
   advancedemo wipegamestate
   ;; d_net
   netcmds maketic ticdup d-tic-hook d-frame-hook BACKUPTICS
   ;; g_game
   g-build-ticcmd g-responder g-ticker g-init-new g-defered-init-new g-do-load-level
   g-play-demo-bytes g-defered-play-demo g-time-demo g-record-demo g-begin-recording
   g-check-demo-status g-exit-level g-world-done demobuffer demo-p demorecording-name
   timingdemo gamekeydown mousex mousey joyxmove joyymove mousebuttons-set! joybuttons-set!
   ;; m_misc
   m-load-defaults screenblocks detailLevel mouseSensitivity showMessages
   ;; front end
   m-responder m-drawer menuactive-state st-responder hu-responder st-init hu-init)
  (import (chezscheme) (doom global) (doom struct) (doom doomdef) (doom doomdata)
          (doom doomstat) (doom d_event) (doom d_ticcmd) (doom d_player) (doom info)
          (doom sounds) (doom dstrings) (doom m_fixed) (doom m_random) (doom m_argv)
          (doom tables) (doom w_wad) (doom v_video) (doom i_system) (doom i_video)
          (doom r_defs) (doom r_data) (doom r_local) (doom p_mobj.h) (doom p_local)
          (doom am_map) (doom s_sound) (doom m_cheat) (doom d_items) (doom st_lib)
          (doom hu_lib) (doom f_wipe))

  ;; Globals shared between the included files (define-global makes a
  ;; macro, which must be defined before the files that use it).
  ;; d_net.c
  (define-global maketic 0)
  (define-global ticdup 1)
  (define-global d-tic-hook #f)
  ;; Test hook: if set, called with no arguments after every frame (after
  ;; D_Display, where the oracle traces).
  (define-global d-frame-hook #f)
  ;; g_game.c
  (define-global sendpause #f)            ; send a pause event next tic
  (define-global sendsave #f)             ; send a save event next tic
  (define-global timingdemo #f)           ; if true, exit with report on completion
  (define-global demoname "")
  (define-global netdemo #f)
  (define-global demobuffer (bytevector))
  (define-global demo-p 0)
  (define-global key_right KEY_RIGHTARROW)
  (define-global key_left KEY_LEFTARROW)
  (define-global key_up KEY_UPARROW)
  (define-global key_down KEY_DOWNARROW)
  (define-global key_strafeleft (char->integer #\,))
  (define-global key_straferight (char->integer #\.))
  (define-global key_fire KEY_RCTRL)
  (define-global key_use (char->integer #\space))
  (define-global key_strafe KEY_RALT)
  (define-global key_speed KEY_RSHIFT)
  (define-global mousebfire 0)
  (define-global mousebstrafe 1)
  (define-global mousebforward 2)
  (define-global joybfire 0)
  (define-global joybstrafe 1)
  (define-global joybuse 3)
  (define-global joybspeed 2)
  (define-global mousex 0)
  (define-global mousey 0)
  (define-global joyxmove 0)
  (define-global joyymove 0)
  (define-global savegameslot 0)
  (define-global savedescription "")
  (define-global defdemoname "")
  ;; m_misc.c (m_menu.c's in C)
  (define-global mouseSensitivity 5)
  (define-global showMessages 1)
  (define-global usemouse 1)
  (define-global usejoystick 0)
  (define-global screenblocks 9)
  (define-global detailLevel 0)
  ;; d_main.c
  (define-global advancedemo #f)
  (define-global wipegamestate GS_DEMOSCREEN)

  (define (demorecording-name) demoname)

  (define (menuactive-state) (list menuactive messageToPrint))

  (include "d_net.c.ss")
  (include "m_misc.c.ss")
  (include "g_game.c.ss")
  (include "hu_stuff.c.ss")
  (include "st_stuff.c.ss")
  (include "m_menu.c.ss")
  (include "wi_stuff.c.ss")
  (include "f_finale.c.ss")
  (include "d_main.c.ss")

  ;; The playsim and automap call up into the front end through these.
  (define hooks-installed
    (begin
      (set! st-start-hook st-start)
      (set! hu-start-hook hu-start)
      (set! st-responder-hook st-responder)
      #t)))
