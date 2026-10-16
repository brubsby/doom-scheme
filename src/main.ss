;;; Milestone 5 demo: the game is playable (the front end is not).
;;; A single player game on skill 3: monsters, weapons, doors, lifts,
;;; switches, pickups and damage all work. There is no status bar, menu or
;;; intermission yet (milestone 7): pickup messages print to the console,
;;; and exiting a level starts the next one.
;;;   arrows        move / turn          , .   strafe     shift  run
;;;   alt + arrows  strafe               ctrl  fire       space  use
;;;   1-7           weapons
;;;   - =           shrink / grow the view (as in Doom)
;;;   F5            toggle low detail
;;;   TAB           automap (iddt works)
;;;   [ ]           previous / next map  (demo only)
;;;   ESC           quit
;;; Environment:
;;;   DOOMWAD=path   (default wads/doom1.wad)
;;;   MAP=n          start on E1Mn (default 1)
;;;   SHOT=out.ppm   walk forward for a second on every map, save the view, exit
(import (chezscheme) (doom i_sdl) (doom i_video) (doom i_system) (doom w_wad)
        (doom v_video) (doom doomdef) (doom doomstat) (doom d_event) (doom d_ticcmd)
        (doom d_player) (doom r_local) (doom p_local) (doom g_game) (doom am_map))

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

(define (new-game! map)
  (when automapactive (am-responder (make-event ev_keydown KEY_TAB 0 0)))
  (g-init-new sk_medium 1 map))

;; Held keys, by doom key code.
(define down (make-vector 256 #f))
(define (held? k) (vector-ref down k))
(define turnheld 0)

;; A simplified G_BuildTiccmd (the real one is milestone 6): run with
;; shift, strafe with alt or , . and the slow turn for the first tics; fire,
;; use, and weapon changes.
(define (build-ticcmd! cmd)
  (let* ([speed (if (held? KEY_RSHIFT) 1 0)]
         [strafe (held? KEY_RALT)]
         [left (held? KEY_LEFTARROW)] [right (held? KEY_RIGHTARROW)])
    (set! turnheld (if (or left right) (+ turnheld 1) 0))
    (let ([turn (if (< turnheld 6) 320 (if (= speed 1) 1280 640))]
          [fwd (vector-ref '#(25 50) speed)]
          [side (vector-ref '#(24 40) speed)])
      (ticcmd-forwardmove-set! cmd (+ (if (held? KEY_UPARROW) fwd 0) (if (held? KEY_DOWNARROW) (- fwd) 0)))
      (ticcmd-sidemove-set! cmd (+ (if (held? (char->integer #\.)) side 0)
                                   (if (held? (char->integer #\,)) (- side) 0)
                                   (if (and strafe right) side 0)
                                   (if (and strafe left) (- side) 0)))
      (ticcmd-angleturn-set! cmd (if strafe
                                     0
                                     (+ (if left turn 0) (if right (- turn) 0))))
      (ticcmd-buttons-set!
       cmd
       (let ([weapon (let loop ([k 1])
                       (cond [(> k 7) #f]
                             [(held? (+ 48 k)) (- k 1)]
                             [else (loop (+ k 1))]))])
         (fxior (if (held? KEY_RCTRL) BT_ATTACK 0)
                (if (held? 32) BT_USE 0)
                (if weapon (fxior BT_CHANGE (fxsll weapon BT_WEAPONSHIFT)) 0)))))))

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

;; Until the HUD exists, print the player's messages.
(define last-message #f)
(define (show-message!)
  (let ([m (player-message player)])
    (when (and m (not (eq? m last-message)))
      (printf "~a\n" m))
    (set! last-message m)))

(define (tic!)
  (build-ticcmd! (vector-ref netcmds 0))
  (g-ticker)
  (set! gametic (+ gametic 1))
  (show-message!)
  ;; No intermission yet: on to the next map.
  (when (memq 'ga_completed unported-calls)
    (set! unported-calls '())
    (set! unported-first-tic #f)
    (new-game! (+ 1 (mod gamemap nummaps)))))

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

(w-init-multiple-files (list (or (getenv "DOOMWAD") "wads/doom1.wad")))
(set! gamemode shareware)
(r-init screenblocks detail)
(p-init)
(v-init)
(i-init-graphics)
(i-set-palette (w-cache-lump-name "PLAYPAL"))
(vector-set! playeringame 0 #t)
(new-game! (let ([m (getenv "MAP")]) (if m (string->number m) 1)))

(let ([shot (getenv "SHOT")])
  (if shot
      (do ([m 1 (+ m 1)]) ((> m nummaps))
        (new-game! m)
        (vector-set! down KEY_UPARROW #t)
        (do ([i 0 (+ i 1)]) ((= i 35)) (tic!))
        (vector-set! down KEY_UPARROW #f)
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
                          (new-game! (+ 1 (mod (+ (- gamemap 1) (if (fx= key 93) 1 -1)) nummaps)))]
                         [else (handle-key ev)]))))
            (set! quit #t))
          (unless quit
            ;; Run a game tic for each elapsed 1/35 s, then draw.
            (let ([now (i-get-time)])
              (do ([t lasttic (+ t 1)]) ((>= t now)) (tic!))
              (display!)
              (sdl-delay 1)
              (loop now)))))))
(i-shutdown-graphics)
