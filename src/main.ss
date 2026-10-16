;;; Milestone 3 demo: the software renderer with a free camera.
;;; Loads E1M1 with its things in their spawn poses (nothing moves yet) and
;;; puts you at player 1's start, walking through walls at eye height.
;;;   arrows        move / turn          , .   strafe
;;;   - =           shrink / grow the view (as in Doom)
;;;   F5            toggle low detail
;;;   TAB           automap (shows the walls you have seen; iddt works)
;;;   [ ]           previous / next map  (demo only)
;;;   ESC           quit
;;; Environment:
;;;   DOOMWAD=path   (default wads/doom1.wad)
;;;   MAP=n          start on E1Mn (default 1)
;;;   SHOT=out.ppm   save one view of every map and exit
(import (chezscheme) (doom i_sdl) (doom i_video) (doom i_system) (doom w_wad)
        (doom v_video) (doom doomdef) (doom doomstat) (doom d_event) (doom d_player)
        (doom p_mobj.h) (doom r_defs) (doom r_data) (doom r_local) (doom p_local)
        (doom am_map) (doom m_fixed) (doom tables))

(define (save-shot path)
  (let-values ([(w h bv) (i-read-pixels)])
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (p)
        (put-bytevector p (string->utf8 (format "P6\n~a ~a\n255\n" w h)))
        (do ([i 0 (fx+ i 1)]) ((fx= i (fx* w h)))
          (let ([px (bytevector-u32-native-ref bv (fx* i 4))])
            (put-u8 p (fxand (fxsrl px 16) 255))
            (put-u8 p (fxand (fxsrl px 8) 255))
            (put-u8 p (fxand px 255))))))))

(define nummaps 9)
(define screenblocks 10)
(define detail 0)
(define player (vector-ref players 0))
(define (mo) (player-mo player))

;; G_InitNew / G_DoLoadLevel for a single player game of E1Mn, skill 3.
(define (load-map! n)
  (when automapactive (am-responder (make-event ev_keydown KEY_TAB 0 0)))
  (player-playerstate-set! player PST_REBORN)
  (set! gameskill sk_medium)
  (set! gameepisode 1)
  (set! gamemap n)
  (set! skyflatnum (r-flat-num-for-name SKYFLATNAME))
  (set! skytexture (r-texture-num-for-name "SKY1"))
  (p-setup-level 1 n 0 sk_medium)
  (place-camera!))

;; Stand on the floor at eye height (C: P_CalcHeight, without the bob).
(define (place-camera!)
  (let ([sec (subsector-sector (mobj-subsector (mo)))])
    (mobj-z-set! (mo) (sector-floorheight sec))
    (player-viewz-set! player (+ (sector-floorheight sec) VIEWHEIGHT))))

;; Noclip movement, relinking the player's mobj like P_TryMove does.
(define (move! forward side)
  (let* ([a (fxsrl (mobj-angle (mo)) ANGLETOFINESHIFT)]
         [ra (fxsrl (angle- (mobj-angle (mo)) ANG90) ANGLETOFINESHIFT)]
         [dx (+ (fixed-mul forward (finecosine a)) (fixed-mul side (finecosine ra)))]
         [dy (+ (fixed-mul forward (fxvector-ref finesine a)) (fixed-mul side (fxvector-ref finesine ra)))])
    (p-unset-thing-position (mo))
    (mobj-x-set! (mo) (int32 (+ (mobj-x (mo)) dx)))
    (mobj-y-set! (mo) (int32 (+ (mobj-y (mo)) dy)))
    (p-set-thing-position (mo))
    (place-camera!)))

;; D_Display, roughly: the view (or automap), its border, the status bar.
(define (display!)
  (when setsizeneeded
    (r-execute-set-view-size)
    (r-fill-back-screen))
  (if automapactive
      (am-drawer)
      (begin
        (r-draw-view-border)
        (r-render-player-view player)))
  (when (or automapactive (not (= viewheight SCREENHEIGHT)))
    (v-draw-patch 0 (- SCREENHEIGHT 32) 0 (w-cache-lump-name "STBAR")))
  (i-finish-update))

(w-init-multiple-files (list (or (getenv "DOOMWAD") "wads/doom1.wad")))
(set! gamemode shareware)
(r-init screenblocks detail)
(p-init)
(v-init)
(i-init-graphics)
(i-set-palette (w-cache-lump-name "PLAYPAL"))
(vector-set! playeringame 0 #t)
(load-map! (let ([m (getenv "MAP")]) (if m (string->number m) 1)))

;; Held keys, by doom key code.
(define down (make-vector 256 #f))
(define (held? k) (vector-ref down k))

(define (camera-ticker)
  (let ([speed (* 8 FRACUNIT)] [turn (* 640 65536)])
    (when (held? KEY_UPARROW) (move! speed 0))
    (when (held? KEY_DOWNARROW) (move! (- speed) 0))
    (when (held? (char->integer #\,)) (move! 0 (- speed)))
    (when (held? (char->integer #\.)) (move! 0 speed))
    (when (held? KEY_LEFTARROW) (mobj-angle-set! (mo) (angle+ (mobj-angle (mo)) turn)))
    (when (held? KEY_RIGHTARROW) (mobj-angle-set! (mo) (angle- (mobj-angle (mo)) turn)))))

(define (handle-key ev)
  (let ([key (event-data1 ev)] [pressed (fx= (event-type ev) ev_keydown)])
    (cond
      [(and (fx= key KEY_TAB) pressed) (am-responder ev)]
      [automapactive (am-responder ev)]
      [(not pressed) (void)]
      [(fx= key KEY_MINUS)
       (when (> screenblocks 3) (set! screenblocks (- screenblocks 1)))
       (r-set-view-size screenblocks detail)]
      [(fx= key KEY_EQUALS)
       (when (< screenblocks 11) (set! screenblocks (+ screenblocks 1)))
       (r-set-view-size screenblocks detail)]
      [(fx= key KEY_F5)
       (set! detail (- 1 detail))
       (r-set-view-size screenblocks detail)])))

(let ([shot (getenv "SHOT")])
  (if shot
      (do ([m 1 (+ m 1)]) ((> m nummaps))
        (load-map! m)
        (display!)
        (save-shot (format "~a-E1M~a.ppm" (path-root shot) m)))
      (let loop ([lasttic (i-get-time)])
        (let ([quit #f])
          (unless (i-get-events
                   (lambda (ev)
                     (let ([key (event-data1 ev)] [pressed (fx= (event-type ev) ev_keydown)])
                       (when (fx< key 256) (vector-set! down key pressed))
                       (cond
                         [(fx= key KEY_ESCAPE) (set! quit #t)]
                         [(and pressed (memv key '(91 93)))
                          (load-map! (+ 1 (mod (+ (- gamemap 1) (if (fx= key 93) 1 -1)) nummaps)))]
                         [else (handle-key ev)]))))
            (set! quit #t))
          (unless quit
            ;; Run the 35 Hz tickers for elapsed tics, then draw.
            (let ([now (i-get-time)])
              (do ([t lasttic (+ t 1)]) ((>= t now))
                (if automapactive (am-ticker) (camera-ticker)))
              (display!)
              (sdl-delay 1)
              (loop now)))))))
(i-shutdown-graphics)
