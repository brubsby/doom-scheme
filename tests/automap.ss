;;; am_map against the oracle. For each shareware map, the oracle
;;; (tools/oracle/levels.sh) opens the automap with the iddt cheat, zooms out
;;; to the whole map ('0') and turns on the grid ('g'), dumping screens[0]
;;; after each step. We replay the same keys and compare the automap area.
(import (chezscheme) (doom w_wad) (doom r_data) (doom p_local) (doom doomdef)
        (doom doomstat) (doom d_event) (doom am_map) (doom v_video))
(load "tests/lib.ss")

;; The oracle frame also has HUD text on top of the map: the "Grid ON"
;; message in the top rows and the level title above the status bar. Those
;; come from hu_stuff (milestone 7), so compare only rows [hud-top, hud-bottom).
(define hud-top 8)
(define hud-bottom 160)

(define (press key)
  (am-responder (make-event ev_keydown key 0 0))
  (am-responder (make-event ev_keyup key 0 0)))

(define (mkdir-p dir)
  (unless (file-exists? dir) (mkdir dir)))

(define (read-raw path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

;; Differing pixels in rows [y0, y1): count and bounding box.
(define (diff-region a b y0 y1)
  (let loop ([y y0] [x 0] [n 0] [box #f])
    (cond
      [(= y y1) (if (= n 0) 0 (list n 'box box))]
      [(= x SCREENWIDTH) (loop (+ y 1) 0 n box)]
      [else
       (let ([i (+ (* y SCREENWIDTH) x)])
         (if (= (bytevector-u8-ref a i) (bytevector-u8-ref b i))
             (loop y (+ x 1) n box)
             (loop y (+ x 1) (+ n 1)
                   (if box
                       (list (min (car box) x) (min (cadr box) y)
                             (max (caddr box) x) (max (cadddr box) y))
                       (list x y x y)))))])))

(w-init-multiple-files (list wad-path))
(set! gamemode shareware)
(r-init-textures)
(r-init-flats)
(vector-set! playeringame 0 #t)
(v-init)

(define steps
  ;; frame, keys, description
  `((2 (,KEY_TAB ,(char->integer #\i) ,(char->integer #\d) ,(char->integer #\d)
                 ,(char->integer #\t)) "follow, iddt")
    (3 (,(char->integer #\0)) "whole map")
    (4 (,(char->integer #\g)) "whole map + grid")))

(do ([m 1 (+ m 1)]) ((> m 9))
  (set! gameepisode 1)
  (set! gamemap m)
  (p-setup-level 1 m 0 sk_medium)
  (for-each
   (lambda (step)
     (let ([frame (car step)] [keys (cadr step)] [what (caddr step)])
       (for-each press keys)
       (am-ticker)
       (am-drawer)
       (let ([theirs (read-raw (format "build/oracle/levels/E1M~a/f~4,'0d.raw" m frame))])
         (let ([d (diff-region theirs screens hud-top hud-bottom)])
           ;; On a mismatch, keep our frame for inspection.
           (unless (eqv? d 0)
             (mkdir-p "build/test")
             (call-with-port (open-file-output-port
                              (format "build/test/automap-E1M~a-f~4,'0d.raw" m frame)
                              (file-options no-fail))
               (lambda (p) (put-bytevector p screens 0 (* SCREENWIDTH SCREENHEIGHT)))))
           (check (format "E1M~a automap: ~a" m what) 0 d)))))
   steps)
  ;; Undo the toggles so the next map starts like a fresh oracle process:
  ;; grid off, iddt twice more (cheating 1 -> 2 -> 0; only seen while the
  ;; map is open), then close the map (which also resets the '0' state).
  (for-each press (map char->integer (string->list "giddtiddt")))
  (press KEY_TAB))
(finish)
