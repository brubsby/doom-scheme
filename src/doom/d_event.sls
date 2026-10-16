;;; d_event: input events and game actions (d_event.h).
(library (doom d_event)
  (export ev_keydown ev_keyup ev_mouse ev_joystick
          event make-event event? event-type event-type-set! event-data1
          event-data1-set! event-data2 event-data2-set! event-data3 event-data3-set!
          ga_nothing ga_loadlevel ga_newgame ga_loadgame ga_savegame ga_playdemo
          ga_completed ga_victory ga_worlddone ga_screenshot
          BT_ATTACK BT_USE BT_SPECIAL BT_SPECIALMASK BT_CHANGE BT_WEAPONMASK
          BT_WEAPONSHIFT BTS_PAUSE BTS_SAVEGAME BTS_SAVEMASK BTS_SAVESHIFT
          MAXEVENTS)
  (import (chezscheme) (doom struct))

  (define ev_keydown 0)
  (define ev_keyup 1)
  (define ev_mouse 2)
  (define ev_joystick 3)

  (define-struct event () (type 0) (data1 0) (data2 0) (data3 0))

  (define ga_nothing 0)
  (define ga_loadlevel 1)
  (define ga_newgame 2)
  (define ga_loadgame 3)
  (define ga_savegame 4)
  (define ga_playdemo 5)
  (define ga_completed 6)
  (define ga_victory 7)
  (define ga_worlddone 8)
  (define ga_screenshot 9)

  (define BT_ATTACK 1)
  (define BT_USE 2)
  (define BT_SPECIAL 128)
  (define BT_SPECIALMASK 3)
  (define BT_CHANGE 4)
  (define BT_WEAPONMASK (+ 8 16 32))
  (define BT_WEAPONSHIFT 3)
  (define BTS_PAUSE 1)
  (define BTS_SAVEGAME 2)
  (define BTS_SAVEMASK (+ 4 8 16))
  (define BTS_SAVESHIFT 2)

  (define MAXEVENTS 64))
