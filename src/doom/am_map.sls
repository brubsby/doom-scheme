;;; am_map: the automap.
(library (doom am_map)
  (export am-responder am-ticker am-drawer am-stop st-responder-hook
          AM_MSGHEADER AM_MSGENTERED AM_MSGEXITED)
  (import (chezscheme) (doom global) (doom doomdef) (doom doomdata) (doom doomstat) (doom d_event)
          (doom d_player) (doom p_mobj.h) (doom p_local) (doom r_defs) (doom r_state)
          (doom m_fixed) (doom tables) (doom m_cheat) (doom v_video) (doom w_wad)
          (doom dstrings))

  ;; ST_Responder, which is above the automap; d_main sets this when it
  ;; loads.
  (define-global st-responder-hook (lambda (ev) #f))

  ;; For use if I do walls with outsides/insides
  (define REDS (- 256 (* 5 16)))
  (define REDRANGE 16)
  (define BLUES (+ (- 256 (* 4 16)) 8))
  (define BLUERANGE 8)
  (define GREENS (* 7 16))
  (define GREENRANGE 16)
  (define GRAYS (* 6 16))
  (define GRAYSRANGE 16)
  (define BROWNS (* 4 16))
  (define BROWNRANGE 16)
  (define YELLOWS (+ (- 256 32) 7))
  (define YELLOWRANGE 1)
  (define BLACK 0)
  (define WHITE (- 256 47))

  ;; Automap colors
  (define BACKGROUND BLACK)
  (define YOURCOLORS WHITE)
  (define WALLCOLORS REDS)
  (define WALLRANGE REDRANGE)
  (define TSWALLCOLORS GRAYS)
  (define FDWALLCOLORS BROWNS)
  (define CDWALLCOLORS YELLOWS)
  (define THINGCOLORS GREENS)
  (define THINGRANGE GREENRANGE)
  (define SECRETWALLCOLORS WALLCOLORS)
  (define GRIDCOLORS (+ GRAYS (fxquotient GRAYSRANGE 2)))
  (define XHAIRCOLORS GRAYS)

  (define AM_PANDOWNKEY KEY_DOWNARROW)
  (define AM_PANUPKEY KEY_UPARROW)
  (define AM_PANRIGHTKEY KEY_RIGHTARROW)
  (define AM_PANLEFTKEY KEY_LEFTARROW)
  (define AM_ZOOMINKEY (char->integer #\=))
  (define AM_ZOOMOUTKEY (char->integer #\-))
  (define AM_STARTKEY KEY_TAB)
  (define AM_ENDKEY KEY_TAB)
  (define AM_GOBIGKEY (char->integer #\0))
  (define AM_FOLLOWKEY (char->integer #\f))
  (define AM_GRIDKEY (char->integer #\g))
  (define AM_MARKKEY (char->integer #\m))
  (define AM_CLEARMARKKEY (char->integer #\c))

  (define AM_NUMMARKPOINTS 10)

  ;; Status bar notifications (am_map.h).
  (define AM_MSGHEADER (+ (fxsll (char->integer #\a) 24) (fxsll (char->integer #\m) 16)))
  (define AM_MSGENTERED (fxior AM_MSGHEADER (fxsll (char->integer #\e) 8)))
  (define AM_MSGEXITED (fxior AM_MSGHEADER (fxsll (char->integer #\x) 8)))

  ;; C float constants, truncated to int as the C casts do.
  (define (ftrunc x) (exact (truncate x)))
  ;; scale on entry: (.2*FRACUNIT), truncated when stored in a fixed_t.
  (define INITSCALEMTOF (ftrunc (* .2 FRACUNIT)))
  ;; how much the automap moves window per tic in frame-buffer coordinates
  ;; moves 140 pixels in 1 second
  (define F_PANINC 4)
  ;; how much zoom-in per tic: goes to 2x in 1 second
  (define M_ZOOMIN (ftrunc (* 1.02 FRACUNIT)))
  ;; how much zoom-out per tic: pulls out to 0.5x in 1 second
  (define M_ZOOMOUT (ftrunc (/ (inexact FRACUNIT) 1.02)))

  (define LINE_NEVERSEE ML_DONTDRAW)

  ;; int arithmetic on fixed_t, wrapping like 32-bit C.
  (define-syntax i+ (syntax-rules () [(_ a b) (int32 (fx+ a b))]))
  (define-syntax i- (syntax-rules () [(_ a b) (int32 (fx- a b))]))
  (define-syntax half (syntax-rules () [(_ a) (fxquotient a 2)]))

  ;; Vector graphics: lists of lines ((ax . ay) . (bx . by)) in map units.
  (define (mline ax ay bx by) (vector ax ay bx by))

  ;; A line drawing of the player pointing right, starting from the middle.
  ;; R = (8*PLAYERRADIUS)/7; R/k with C truncation; -R/k = -(R/k).
  (define R (fxquotient (fx* 8 PLAYERRADIUS) 7))
  (define (R/ k) (fxquotient R k))
  (define player-arrow
    (vector
     (mline (+ (- R) (R/ 8)) 0 R 0)                                ; -----
     (mline R 0 (- R (R/ 2)) (R/ 4))                               ; ----->
     (mline R 0 (- R (R/ 2)) (- (R/ 4)))
     (mline (+ (- R) (R/ 8)) 0 (- (- R) (R/ 8)) (R/ 4))            ; >---->
     (mline (+ (- R) (R/ 8)) 0 (- (- R) (R/ 8)) (- (R/ 4)))
     (mline (+ (- R) (fxquotient (* 3 R) 8)) 0 (+ (- R) (R/ 8)) (R/ 4))  ; >>--->
     (mline (+ (- R) (fxquotient (* 3 R) 8)) 0 (+ (- R) (R/ 8)) (- (R/ 4)))))

  (define cheat-player-arrow
    (vector
     (mline (+ (- R) (R/ 8)) 0 R 0)                                ; -----
     (mline R 0 (- R (R/ 2)) (R/ 6))                               ; ----->
     (mline R 0 (- R (R/ 2)) (- (R/ 6)))
     (mline (+ (- R) (R/ 8)) 0 (- (- R) (R/ 8)) (R/ 6))            ; >----->
     (mline (+ (- R) (R/ 8)) 0 (- (- R) (R/ 8)) (- (R/ 6)))
     (mline (+ (- R) (fxquotient (* 3 R) 8)) 0 (+ (- R) (R/ 8)) (R/ 6))  ; >>----->
     (mline (+ (- R) (fxquotient (* 3 R) 8)) 0 (+ (- R) (R/ 8)) (- (R/ 6)))
     (mline (- (R/ 2)) 0 (- (R/ 2)) (- (R/ 6)))                    ; >>-d--->
     (mline (- (R/ 2)) (- (R/ 6)) (+ (- (R/ 2)) (R/ 6)) (- (R/ 6)))
     (mline (+ (- (R/ 2)) (R/ 6)) (- (R/ 6)) (+ (- (R/ 2)) (R/ 6)) (R/ 4))
     (mline (- (R/ 6)) 0 (- (R/ 6)) (- (R/ 6)))                    ; >>-dd-->
     (mline (- (R/ 6)) (- (R/ 6)) 0 (- (R/ 6)))
     (mline 0 (- (R/ 6)) 0 (R/ 4))
     (mline (R/ 6) (R/ 4) (R/ 6) (- (R/ 7)))                       ; >>-ddt->
     (mline (R/ 6) (- (R/ 7)) (+ (R/ 6) (R/ 32)) (- (- (R/ 7)) (R/ 32)))
     (mline (+ (R/ 6) (R/ 32)) (- (- (R/ 7)) (R/ 32)) (+ (R/ 6) (R/ 10)) (- (R/ 7)))))

  ;; These use R = FRACUNIT and double constants.
  (define (u x) (ftrunc (* x FRACUNIT)))
  (define triangle-guy
    (vector
     (mline (u -.867) (u -.5) (u .867) (u -.5))
     (mline (u .867) (u -.5) 0 FRACUNIT)
     (mline 0 FRACUNIT (u -.867) (u -.5))))
  (define thintriangle-guy
    (vector
     (mline (u -.5) (u -.7) FRACUNIT 0)
     (mline FRACUNIT 0 (u -.5) (u .7))
     (mline (u -.5) (u .7) (u -.5) (u -.7))))

  (define cheating 0)
  (define grid #f)

  (define finit-width SCREENWIDTH)
  (define finit-height (- SCREENHEIGHT 32))

  ;; location and size of window on screen
  (define f-x 0)
  (define f-y 0)
  (define f-w 0)
  (define f-h 0)

  (define lightlev 0)           ; used for funky strobing effect
  (define fb 0)                 ; pseudo-frame buffer: offset into `screens`
  (define amclock 0)

  (define m-paninc-x 0)         ; how far the window pans each tic (map coords)
  (define m-paninc-y 0)
  (define mtof-zoommul 0)       ; how far the window zooms in each tic (map coords)
  (define ftom-zoommul 0)       ; how far the window zooms in each tic (fb coords)

  (define m-x 0) (define m-y 0)   ; LL x,y where the window is on the map
  (define m-x2 0) (define m-y2 0) ; UR x,y where the window is on the map

  ;; width/height of window on map (map coords)
  (define m-w 0)
  (define m-h 0)

  ;; based on level size
  (define min-x 0) (define min-y 0)
  (define max-x 0) (define max-y 0)
  (define max-w 0) (define max-h 0)

  ;; based on player size
  (define min-w 0) (define min-h 0)

  (define min-scale-mtof 0)     ; used to tell when to stop zooming out
  (define max-scale-mtof 0)     ; used to tell when to stop zooming in

  ;; old stuff for recovery later
  (define old-m-w 0) (define old-m-h 0)
  (define old-m-x 0) (define old-m-y 0)

  ;; old location used by the Follower routine
  (define f-oldloc-x 0)
  (define f-oldloc-y 0)

  ;; used by MTOF to scale from map-to-frame-buffer coords
  (define scale-mtof INITSCALEMTOF)
  ;; used by FTOM to scale from frame-buffer-to-map coords (=1/scale_mtof)
  (define scale-ftom 0)

  (define plr #f)               ; the player represented by an arrow

  (define marknums (make-vector 10 #f))   ; numbers used for marking
  (define markpoints-x (make-fxvector AM_NUMMARKPOINTS -1))  ; -1 means empty
  (define markpoints-y (make-fxvector AM_NUMMARKPOINTS 0))
  (define markpointnum 0)       ; next point to be assigned

  (define followplayer #t)      ; whether to follow the player around

  (define cheat-amap (make-cheatseq '(#xb2 #x26 #x26 #x2e #xff)))   ; "iddt"

  (define stopped #t)

  ;; Translates between frame-buffer and map distances / coordinates.
  (define (ftom x) (fixed-mul (int32 (fxsll x 16)) scale-ftom))
  (define (mtof x) (fxsra (fixed-mul x scale-mtof) 16))
  (define (cxmtof x) (fx+ f-x (mtof (i- x m-x))))
  (define (cymtof y) (fx+ f-y (fx- f-h (mtof (i- y m-y)))))

  (define (am-activate-new-scale)
    (set! m-x (i+ m-x (half m-w)))
    (set! m-y (i+ m-y (half m-h)))
    (set! m-w (ftom f-w))
    (set! m-h (ftom f-h))
    (set! m-x (i- m-x (half m-w)))
    (set! m-y (i- m-y (half m-h)))
    (set! m-x2 (i+ m-x m-w))
    (set! m-y2 (i+ m-y m-h)))

  (define (am-save-scale-and-loc)
    (set! old-m-x m-x)
    (set! old-m-y m-y)
    (set! old-m-w m-w)
    (set! old-m-h m-h))

  (define (am-restore-scale-and-loc)
    (set! m-w old-m-w)
    (set! m-h old-m-h)
    (if (not followplayer)
        (begin (set! m-x old-m-x)
               (set! m-y old-m-y))
        (begin (set! m-x (i- (mobj-x (player-mo plr)) (half m-w)))
               (set! m-y (i- (mobj-y (player-mo plr)) (half m-h)))))
    (set! m-x2 (i+ m-x m-w))
    (set! m-y2 (i+ m-y m-h))
    ;; Change the scaling multipliers
    (set! scale-mtof (fixed-div (fxsll f-w FRACBITS) m-w))
    (set! scale-ftom (fixed-div FRACUNIT scale-mtof)))

  ;; adds a marker at the current location
  (define (am-add-mark)
    (fxvector-set! markpoints-x markpointnum (i+ m-x (half m-w)))
    (fxvector-set! markpoints-y markpointnum (i+ m-y (half m-h)))
    (set! markpointnum (fxremainder (fx+ markpointnum 1) AM_NUMMARKPOINTS)))

  ;; Determines bounding box of all vertices, sets global variables
  ;; controlling zoom range. (Note the else-ifs: the first vertex only sets
  ;; the minimums.)
  (define (am-find-min-max-boundaries)
    (set! min-x MAXINT) (set! min-y MAXINT)
    (set! max-x (- MAXINT)) (set! max-y (- MAXINT))
    (vector-for-each
     (lambda (v)
       (let ([x (vertex-x v)] [y (vertex-y v)])
         (cond [(fx< x min-x) (set! min-x x)]
               [(fx> x max-x) (set! max-x x)])
         (cond [(fx< y min-y) (set! min-y y)]
               [(fx> y max-y) (set! max-y y)])))
     vertexes)
    (set! max-w (i- max-x min-x))
    (set! max-h (i- max-y min-y))
    (set! min-w (* 2 PLAYERRADIUS))
    (set! min-h (* 2 PLAYERRADIUS))
    (let ([a (fixed-div (fxsll f-w FRACBITS) max-w)]
          [b (fixed-div (fxsll f-h FRACBITS) max-h)])
      (set! min-scale-mtof (if (fx< a b) a b))
      (set! max-scale-mtof (fixed-div (fxsll f-h FRACBITS) (* 2 PLAYERRADIUS)))))

  (define (am-change-window-loc)
    (when (or (not (fx= m-paninc-x 0)) (not (fx= m-paninc-y 0)))
      (set! followplayer #f)
      (set! f-oldloc-x MAXINT))
    (set! m-x (i+ m-x m-paninc-x))
    (set! m-y (i+ m-y m-paninc-y))
    (cond [(fx> (i+ m-x (half m-w)) max-x) (set! m-x (i- max-x (half m-w)))]
          [(fx< (i+ m-x (half m-w)) min-x) (set! m-x (i- min-x (half m-w)))])
    (cond [(fx> (i+ m-y (half m-h)) max-y) (set! m-y (i- max-y (half m-h)))]
          [(fx< (i+ m-y (half m-h)) min-y) (set! m-y (i- min-y (half m-h)))])
    (set! m-x2 (i+ m-x m-w))
    (set! m-y2 (i+ m-y m-h)))

  (define (am-init-variables)
    (set! automapactive #t)
    (set! fb (screen-base 0))
    (set! f-oldloc-x MAXINT)
    (set! amclock 0)
    (set! lightlev 0)
    (set! m-paninc-x 0)
    (set! m-paninc-y 0)
    (set! ftom-zoommul FRACUNIT)
    (set! mtof-zoommul FRACUNIT)
    (set! m-w (ftom f-w))
    (set! m-h (ftom f-h))
    ;; find player to center on initially
    (let ([pnum (if (vector-ref playeringame consoleplayer)
                    consoleplayer
                    (let loop ([i 0])
                      (if (or (fx= i MAXPLAYERS) (vector-ref playeringame i)) i (loop (fx+ i 1)))))])
      (set! plr (vector-ref players pnum)))
    (set! m-x (i- (mobj-x (player-mo plr)) (half m-w)))
    (set! m-y (i- (mobj-y (player-mo plr)) (half m-h)))
    (am-change-window-loc)
    ;; for saving & restoring
    (set! old-m-x m-x)
    (set! old-m-y m-y)
    (set! old-m-w m-w)
    (set! old-m-h m-h)
    ;; inform the status bar of the change
    (st-responder-hook (make-event ev_keyup AM_MSGENTERED 0 0)))

  (define (am-load-pics)
    (do ([i 0 (fx+ i 1)]) ((fx= i 10))
      (vector-set! marknums i (w-cache-lump-name (format "AMMNUM~a" i)))))

  (define (am-clear-marks)
    (fxvector-fill! markpoints-x -1)
    (set! markpointnum 0))

  ;; should be called at the start of every level
  (define (am-level-init)
    (set! f-x 0)
    (set! f-y 0)
    (set! f-w finit-width)
    (set! f-h finit-height)
    (am-clear-marks)
    (am-find-min-max-boundaries)
    (set! scale-mtof (fixed-div min-scale-mtof (ftrunc (* 0.7 FRACUNIT))))
    (when (fx> scale-mtof max-scale-mtof)
      (set! scale-mtof min-scale-mtof))
    (set! scale-ftom (fixed-div FRACUNIT scale-mtof)))

  (define (am-stop)
    (set! automapactive #f)
    ;; C's initializer here is { 0, ev_keyup, AM_MSGEXITED }, so the event
    ;; actually sent is type ev_keydown, data1 = ev_keyup (1), data2 =
    ;; AM_MSGEXITED: the status bar sees a keypress of 1 (which resets the
    ;; cheat matchers) and never learns the automap closed.
    (st-responder-hook (make-event ev_keydown ev_keyup AM_MSGEXITED 0))
    (set! stopped #t))

  (define lastlevel -1)
  (define lastepisode -1)
  (define (am-start)
    (unless stopped (am-stop))
    (set! stopped #f)
    (when (or (not (fx= lastlevel gamemap)) (not (fx= lastepisode gameepisode)))
      (am-level-init)
      (set! lastlevel gamemap)
      (set! lastepisode gameepisode))
    (am-init-variables)
    (am-load-pics))

  ;; set the window scale to the maximum size
  (define (am-min-out-window-scale)
    (set! scale-mtof min-scale-mtof)
    (set! scale-ftom (fixed-div FRACUNIT scale-mtof))
    (am-activate-new-scale))

  ;; set the window scale to the minimum size
  (define (am-max-out-window-scale)
    (set! scale-mtof max-scale-mtof)
    (set! scale-ftom (fixed-div FRACUNIT scale-mtof))
    (am-activate-new-scale))

  ;; Handle events (user inputs) in automap mode. Returns #t if eaten.
  (define bigstate #f)
  (define (am-responder ev)
    (let ([type (event-type ev)] [key (event-data1 ev)])
      (cond
        [(not automapactive)
         (if (and (fx= type ev_keydown) (fx= key AM_STARTKEY))
             (begin (am-start)
                    (set! viewactive #f)
                    #t)
             #f)]
        [(fx= type ev_keydown)
         (let ([rc
                (cond
                  [(fx= key AM_PANRIGHTKEY)
                   (and (not followplayer) (begin (set! m-paninc-x (ftom F_PANINC)) #t))]
                  [(fx= key AM_PANLEFTKEY)
                   (and (not followplayer) (begin (set! m-paninc-x (- (ftom F_PANINC))) #t))]
                  [(fx= key AM_PANUPKEY)
                   (and (not followplayer) (begin (set! m-paninc-y (ftom F_PANINC)) #t))]
                  [(fx= key AM_PANDOWNKEY)
                   (and (not followplayer) (begin (set! m-paninc-y (- (ftom F_PANINC))) #t))]
                  [(fx= key AM_ZOOMOUTKEY)
                   (set! mtof-zoommul M_ZOOMOUT)
                   (set! ftom-zoommul M_ZOOMIN)
                   #t]
                  [(fx= key AM_ZOOMINKEY)
                   (set! mtof-zoommul M_ZOOMIN)
                   (set! ftom-zoommul M_ZOOMOUT)
                   #t]
                  [(fx= key AM_ENDKEY)
                   (set! bigstate #f)
                   (set! viewactive #t)
                   (am-stop)
                   #t]
                  [(fx= key AM_GOBIGKEY)
                   (set! bigstate (not bigstate))
                   (if bigstate
                       (begin (am-save-scale-and-loc)
                              (am-min-out-window-scale))
                       (am-restore-scale-and-loc))
                   #t]
                  [(fx= key AM_FOLLOWKEY)
                   (set! followplayer (not followplayer))
                   (set! f-oldloc-x MAXINT)
                   (player-message-set! plr (if followplayer AMSTR_FOLLOWON AMSTR_FOLLOWOFF))
                   #t]
                  [(fx= key AM_GRIDKEY)
                   (set! grid (not grid))
                   (player-message-set! plr (if grid AMSTR_GRIDON AMSTR_GRIDOFF))
                   #t]
                  [(fx= key AM_MARKKEY)
                   (player-message-set! plr (format "~a ~a" AMSTR_MARKEDSPOT markpointnum))
                   (am-add-mark)
                   #t]
                  [(fx= key AM_CLEARMARKKEY)
                   (am-clear-marks)
                   (player-message-set! plr AMSTR_MARKSCLEARED)
                   #t]
                  [else #f])])
           (if (and (fx= deathmatch 0) (cht-check-cheat cheat-amap key))
               (begin (set! cheating (fxremainder (fx+ cheating 1) 3))
                      #f)
               rc))]
        [(fx= type ev_keyup)
         (cond
           [(or (fx= key AM_PANRIGHTKEY) (fx= key AM_PANLEFTKEY))
            (unless followplayer (set! m-paninc-x 0))]
           [(or (fx= key AM_PANUPKEY) (fx= key AM_PANDOWNKEY))
            (unless followplayer (set! m-paninc-y 0))]
           [(or (fx= key AM_ZOOMOUTKEY) (fx= key AM_ZOOMINKEY))
            (set! mtof-zoommul FRACUNIT)
            (set! ftom-zoommul FRACUNIT)])
         #f]
        [else #f])))

  ;; Zooming
  (define (am-change-window-scale)
    ;; Change the scaling multipliers
    (set! scale-mtof (fixed-mul scale-mtof mtof-zoommul))
    (set! scale-ftom (fixed-div FRACUNIT scale-mtof))
    (cond [(fx< scale-mtof min-scale-mtof) (am-min-out-window-scale)]
          [(fx> scale-mtof max-scale-mtof) (am-max-out-window-scale)]
          [else (am-activate-new-scale)]))

  (define (am-do-follow-player)
    (let ([mo (player-mo plr)])
      (when (or (not (fx= f-oldloc-x (mobj-x mo))) (not (fx= f-oldloc-y (mobj-y mo))))
        (set! m-x (i- (ftom (mtof (mobj-x mo))) (half m-w)))
        (set! m-y (i- (ftom (mtof (mobj-y mo))) (half m-h)))
        (set! m-x2 (i+ m-x m-w))
        (set! m-y2 (i+ m-y m-h))
        (set! f-oldloc-x (mobj-x mo))
        (set! f-oldloc-y (mobj-y mo)))))

  ;; Updates on Game Tick
  (define (am-ticker)
    (when automapactive
      (set! amclock (fx+ amclock 1))
      (when followplayer (am-do-follow-player))
      ;; Change the zoom if necessary
      (unless (fx= ftom-zoommul FRACUNIT) (am-change-window-scale))
      ;; Change x,y location
      (when (or (not (fx= m-paninc-x 0)) (not (fx= m-paninc-y 0)))
        (am-change-window-loc))
      ;; Update light level: AM_updateLightLev is commented out in C.
      ))

  ;; Clear automap frame buffer.
  (define (am-clear-fb color)
    (let ([end (fx+ fb (fx* f-w f-h))])
      (do ([i fb (fx+ i 1)]) ((fx= i end))
        (bytevector-u8-set! screens i color))))

  ;; Automap clipping of lines: Cohen-Sutherland with a faster trivial
  ;; reject in map coordinates. Takes a map line, returns the frame-buffer
  ;; line as (values #t ax ay bx by), or #f if nothing is visible.
  (define LEFT 1)
  (define RIGHT 2)
  (define BOTTOM 4)
  (define TOP 8)
  (define (outcode x y)
    (fxior (cond [(fx< y 0) TOP] [(fx>= y f-h) BOTTOM] [else 0])
           (cond [(fx< x 0) LEFT] [(fx>= x f-w) RIGHT] [else 0])))
  (define (am-clip-mline ml)
    (let ([max* (vector-ref ml 0)] [may (vector-ref ml 1)]
          [mbx (vector-ref ml 2)] [mby (vector-ref ml 3)])
      ;; do trivial rejects and outcodes
      (let ([oc1 (cond [(fx> may m-y2) TOP] [(fx< may m-y) BOTTOM] [else 0])]
            [oc2 (cond [(fx> mby m-y2) TOP] [(fx< mby m-y) BOTTOM] [else 0])])
        (if (fxlogtest oc1 oc2)
            (values #f 0 0 0 0)         ; trivially outside
            (let ([oc1 (fxior oc1 (cond [(fx< max* m-x) LEFT] [(fx> max* m-x2) RIGHT] [else 0]))]
                  [oc2 (fxior oc2 (cond [(fx< mbx m-x) LEFT] [(fx> mbx m-x2) RIGHT] [else 0]))])
              (if (fxlogtest oc1 oc2)
                  (values #f 0 0 0 0)
                  ;; transform to frame-buffer coordinates.
                  (let loop ([ax (cxmtof max*)] [ay (cymtof may)]
                             [bx (cxmtof mbx)] [by (cymtof mby)])
                    (let ([oc1 (outcode ax ay)] [oc2 (outcode bx by)])
                      (cond
                        [(fxlogtest oc1 oc2) (values #f 0 0 0 0)]
                        [(fx= (fxior oc1 oc2) 0) (values #t ax ay bx by)]
                        [else
                         ;; may be partially inside box: find an outside
                         ;; point and clip it to each side
                         (let ([outside (if (fx= oc1 0) oc2 oc1)])
                           (let-values
                               ([(tx ty)
                                 (cond
                                   [(fxlogtest outside TOP)
                                    (let ([dy (fx- ay by)] [dx (fx- bx ax)])
                                      (values (fx+ ax (fxquotient (int32 (* dx ay)) dy)) 0))]
                                   [(fxlogtest outside BOTTOM)
                                    (let ([dy (fx- ay by)] [dx (fx- bx ax)])
                                      (values (fx+ ax (fxquotient (int32 (* dx (fx- ay f-h))) dy))
                                              (fx- f-h 1)))]
                                   [(fxlogtest outside RIGHT)
                                    (let ([dy (fx- by ay)] [dx (fx- bx ax)])
                                      (values (fx- f-w 1)
                                              (fx+ ay (fxquotient (int32 (* dy (fx- (fx- f-w 1) ax))) dx))))]
                                   [else ; LEFT
                                    (let ([dy (fx- by ay)] [dx (fx- bx ax)])
                                      (values 0 (fx+ ay (fxquotient (int32 (* dy (fx- ax))) dx))))])])
                             ;; C recomputes only the moved point's outcode and
                             ;; rejects on a shared bit; the loop head does both.
                             (if (fx= outside oc1)
                                 (loop tx ty bx by)
                                 (loop ax ay tx ty))))])))))))))

  ;; Classic Bresenham w/ whatever optimizations needed for speed
  (define (am-draw-fline ax ay bx by color)
    ;; For debugging only: C prints "fuck %d" and skips the line.
    (unless (or (fx< ax 0) (fx>= ax f-w) (fx< ay 0) (fx>= ay f-h)
                (fx< bx 0) (fx>= bx f-w) (fx< by 0) (fx>= by f-h))
      (let* ([dx (fx- bx ax)] [ax2 (fx* 2 (fxabs dx))] [sx (if (fx< dx 0) -1 1)]
             [dy (fx- by ay)] [ay2 (fx* 2 (fxabs dy))] [sy (if (fx< dy 0) -1 1)])
        (define (putdot x y) (bytevector-u8-set! screens (fx+ fb (fx* y f-w) x) color))
        (if (fx> ax2 ay2)
            (let loop ([x ax] [y ay] [d (fx- ay2 (fxquotient ax2 2))])
              (putdot x y)
              (unless (fx= x bx)
                (if (fx>= d 0)
                    (loop (fx+ x sx) (fx+ y sy) (fx+ (fx- d ax2) ay2))
                    (loop (fx+ x sx) y (fx+ d ay2)))))
            (let loop ([x ax] [y ay] [d (fx- ax2 (fxquotient ay2 2))])
              (putdot x y)
              (unless (fx= y by)
                (if (fx>= d 0)
                    (loop (fx+ x sx) (fx+ y sy) (fx+ (fx- d ay2) ax2))
                    (loop x (fx+ y sy) (fx+ d ax2)))))))))

  ;; Clip lines, draw visible parts of lines.
  (define (am-draw-mline ml color)
    (let-values ([(visible ax ay bx by) (am-clip-mline ml)])
      (when visible (am-draw-fline ax ay bx by color))))

  ;; Draws flat (floor/ceiling tile) aligned grid lines.
  (define (am-draw-grid color)
    (let ([unit (fxsll MAPBLOCKUNITS FRACBITS)])
      ;; Figure out start of vertical gridlines
      (let* ([start m-x]
             [r (remainder (i- start bmaporgx) unit)]
             [start (if (fx= r 0) start (i+ start (fx- unit r)))]
             [end (i+ m-x m-w)])
        ;; draw vertical gridlines
        (do ([x start (i+ x unit)]) ((not (fx< x end)))
          (am-draw-mline (mline x m-y x (i+ m-y m-h)) color)))
      ;; Figure out start of horizontal gridlines
      (let* ([start m-y]
             [r (remainder (i- start bmaporgy) unit)]
             [start (if (fx= r 0) start (i+ start (fx- unit r)))]
             [end (i+ m-y m-h)])
        ;; draw horizontal gridlines
        (do ([y start (i+ y unit)]) ((not (fx< y end)))
          (am-draw-mline (mline m-x y (i+ m-x m-w) y) color)))))

  ;; Determines visible lines, draws them. This is LineDef based, not
  ;; LineSeg based.
  (define (am-draw-walls)
    (vector-for-each
     (lambda (li)
       (let ([l (mline (vertex-x (line-v1 li)) (vertex-y (line-v1 li))
                       (vertex-x (line-v2 li)) (vertex-y (line-v2 li)))]
             [flags (line-flags li)]
             [back (line-backsector li)])
         (cond
           [(or (not (fx= cheating 0)) (fxlogtest flags ML_MAPPED))
            (cond
              [(and (fxlogtest flags LINE_NEVERSEE) (fx= cheating 0)) (void)]
              [(not back) (am-draw-mline l (fx+ WALLCOLORS lightlev))]
              [(fx= (line-special li) 39)      ; teleporters
               (am-draw-mline l (fx+ WALLCOLORS (fxquotient WALLRANGE 2)))]
              [(fxlogtest flags ML_SECRET)     ; secret door
               (if (not (fx= cheating 0))
                   (am-draw-mline l (fx+ SECRETWALLCOLORS lightlev))
                   (am-draw-mline l (fx+ WALLCOLORS lightlev)))]
              [(not (fx= (sector-floorheight back) (sector-floorheight (line-frontsector li))))
               (am-draw-mline l (fx+ FDWALLCOLORS lightlev))]   ; floor level change
              [(not (fx= (sector-ceilingheight back) (sector-ceilingheight (line-frontsector li))))
               (am-draw-mline l (fx+ CDWALLCOLORS lightlev))]   ; ceiling level change
              [(not (fx= cheating 0))
               (am-draw-mline l (fx+ TSWALLCOLORS lightlev))])]
           [(not (fx= (fxvector-ref (player-powers plr) pw_allmap) 0))
            (unless (fxlogtest flags LINE_NEVERSEE)
              (am-draw-mline l (fx+ GRAYS 3)))])))
     lines))

  ;; Rotation in 2D. Used to rotate player arrow line character.
  (define (am-rotate x y a)
    (let* ([i (fxsrl a ANGLETOFINESHIFT)]
           [cos (finecosine i)] [sin (fxvector-ref finesine i)])
      (values (i- (fixed-mul x cos) (fixed-mul y sin))
              (i+ (fixed-mul x sin) (fixed-mul y cos)))))

  (define (am-draw-line-character lineguy scale angle color x y)
    (define (transform px py)
      (let-values ([(px py) (if (fx= scale 0)
                                (values px py)
                                (values (fixed-mul scale px) (fixed-mul scale py)))])
        (let-values ([(px py) (if (fx= angle 0) (values px py) (am-rotate px py angle))])
          (values (i+ px x) (i+ py y)))))
    (vector-for-each
     (lambda (ml)
       (let-values ([(ax ay) (transform (vector-ref ml 0) (vector-ref ml 1))]
                    [(bx by) (transform (vector-ref ml 2) (vector-ref ml 3))])
         (am-draw-mline (mline ax ay bx by) color)))
     lineguy))

  (define their-colors (vector GREENS GRAYS BROWNS REDS))
  (define (am-draw-players)
    (if (not netgame)
        (let ([mo (player-mo plr)])
          (am-draw-line-character (if (fx= cheating 0) player-arrow cheat-player-arrow)
                                  0 (mobj-angle mo) WHITE (mobj-x mo) (mobj-y mo)))
        (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
          (let ([p (vector-ref players i)])
            (unless (or (and (not (fx= deathmatch 0)) (not singledemo) (not (eq? p plr)))
                        (not (vector-ref playeringame i)))
              (let ([color (if (fx= (fxvector-ref (player-powers p) pw_invisibility) 0)
                               (vector-ref their-colors i)
                               246)]  ; *close* to black
                    [mo (player-mo p)])
                (am-draw-line-character player-arrow 0 (mobj-angle mo) color
                                        (mobj-x mo) (mobj-y mo))))))))

  (define (am-draw-things colors colorrange)
    (vector-for-each
     (lambda (sec)
       (let loop ([t (sector-thinglist sec)])
         (when t
           (am-draw-line-character thintriangle-guy (fxsll 16 FRACBITS) (mobj-angle t)
                                   (fx+ colors lightlev) (mobj-x t) (mobj-y t))
           (loop (mobj-snext t)))))
     sectors))

  (define (am-draw-marks)
    (do ([i 0 (fx+ i 1)]) ((fx= i AM_NUMMARKPOINTS))
      (unless (fx= (fxvector-ref markpoints-x i) -1)
        ;; w = 5, h = 6: "because something's wrong with the wad, i guess"
        (let ([w 5] [h 6]
              [fx (cxmtof (fxvector-ref markpoints-x i))]
              [fy (cymtof (fxvector-ref markpoints-y i))])
          (when (and (fx>= fx f-x) (fx<= fx (fx- f-w w)) (fx>= fy f-y) (fx<= fy (fx- f-h h)))
            (v-draw-patch fx fy 0 (vector-ref marknums i)))))))

  (define (am-draw-crosshair color)
    ;; single point for now
    (bytevector-u8-set! screens (fx+ fb (fxquotient (fx* f-w (fx+ f-h 1)) 2)) color))

  (define (am-drawer)
    (when automapactive
      (am-clear-fb BACKGROUND)
      (when grid (am-draw-grid GRIDCOLORS))
      (am-draw-walls)
      (am-draw-players)
      (when (fx= cheating 2) (am-draw-things THINGCOLORS THINGRANGE))
      (am-draw-crosshair XHAIRCOLORS)
      (am-draw-marks)
      (v-mark-rect f-x f-y f-w f-h)))

  ;; P_KillMobj calls AM_Stop through this hook (the playsim is below the
  ;; automap).
  (define hook-installed (begin (set! am-stop-hook am-stop) #t)))
