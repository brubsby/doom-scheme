;;; The renderer against the oracle. For each shareware map, the oracle
;;; (tools/oracle/render.sh) loads the level and renders it from every camera
;;; in build/oracle/cameras/E1Mn.txt (tools/oracle/cameras.py): things at
;;; their spawn states, a range of view sizes, low detail, extralight and
;;; the fixed colormaps. We do the same and compare whole frames.
;;;
;;; Each map runs in its own process (MAP=n), as the oracle does, because
;;; the renderer keeps state between frames (the spectre fuzz position).
;;; Without MAP, this runs all nine. Mismatching frames are saved to
;;; build/test/render-E1Mn-cNNNN.raw.
(import (chezscheme) (doom w_wad) (doom r_data) (doom r_local) (doom p_local)
        (doom doomdef) (doom doomstat) (doom d_player) (doom p_mobj.h) (doom r_defs)
        (doom v_video) (doom m_fixed))
(load "tests/lib.ss")

(define (read-raw path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define (read-cameras path)
  (call-with-input-file path
    (lambda (p)
      (let loop ([acc '()])
        (let ([line (get-line p)])
          (if (eof-object? line)
              (reverse acc)
              (loop (cons (with-input-from-string (string-append "(" line ")") read) acc))))))))

;; Differing pixels outside the garbage mask: count and bounding box.
(define (diff-frame a b mask)
  (let loop ([i 0] [n 0] [box #f])
    (if (= i (* SCREENWIDTH SCREENHEIGHT))
        (if (= n 0) 0 (list n 'box box))
        (if (or (= (bytevector-u8-ref a i) (bytevector-u8-ref b i))
                (= (bytevector-u8-ref mask i) 1))
            (loop (+ i 1) n box)
            (let ([x (mod i SCREENWIDTH)] [y (div i SCREENWIDTH)])
              (loop (+ i 1) (+ n 1)
                    (if box
                        (list (min (car box) x) (min (cadr box) y)
                              (max (caddr box) x) (max (cadddr box) y))
                        (list x y x y))))))))

(define (run-map m)
  (w-init-multiple-files (list wad-path))
  (set! gamemode shareware)
  (r-init 11 0)
  (p-init)
  ;; G_InitNew / G_DoLoadLevel, for a single player game of E1Mm on skill 3.
  (vector-set! playeringame 0 #t)
  (player-playerstate-set! (vector-ref players 0) PST_REBORN)
  (set! gameskill sk_medium)
  (set! gameepisode 1)
  (set! gamemap m)
  (set! skyflatnum (r-flat-num-for-name SKYFLATNAME))
  (set! skytexture (r-texture-num-for-name "SKY1"))
  (p-setup-level 1 m 0 sk_medium)
  ;; The probe's camera: a bare mobj and player (oracle.c, oracle_camera_probe).
  (let ([mo (make-mobj)] [pl (make-player)] [frame (make-bytevector (* SCREENWIDTH SCREENHEIGHT))]
        [mask (make-bytevector (* SCREENWIDTH SCREENHEIGHT))])
    (set! garbage-mask mask)
    (player-mo-set! pl mo)
    (let loop ([cams (read-cameras (format "build/oracle/cameras/E1M~a.txt" m))] [n 0] [start (real-time)])
      (if (null? cams)
          (printf "     (~a frames, ~a ms/frame)\n" n (if (= n 0) 0 (quotient (- (real-time) start) n)))
          (apply
           (lambda (x y angle blocks detail extralight fixedcolormap)
             (mobj-x-set! mo (* x FRACUNIT))
             (mobj-y-set! mo (* y FRACUNIT))
             (mobj-angle-set! mo angle)
             (mobj-subsector-set! mo (r-point-in-subsector (mobj-x mo) (mobj-y mo)))
             (player-viewz-set! pl (+ (sector-floorheight (subsector-sector (mobj-subsector mo)))
                                      (* 41 FRACUNIT)))
             (player-extralight-set! pl extralight)
             (player-fixedcolormap-set! pl fixedcolormap)
             (r-set-view-size blocks detail)
             (r-execute-set-view-size)
             (bytevector-fill! screens 0)
             (bytevector-fill! mask 0)
             (r-render-player-view pl)
             (bytevector-copy! screens 0 frame 0 (bytevector-length frame))
             (let* ([theirs (read-raw (format "build/oracle/render/E1M~a/c~4,'0d.raw" m n))]
                    [d (diff-frame theirs frame mask)]
                    [garbage (let count ([i 0] [k 0])
                               (if (= i (bytevector-length mask))
                                   k
                                   (count (+ i 1) (+ k (bytevector-u8-ref mask i)))))])
               (unless (= garbage 0)
                 (printf "     (c~4,'0d: ~a pixels depend on garbage reads, not compared)\n" n garbage))
               (unless (eqv? d 0)
                 (unless (file-exists? "build/test") (mkdir "build/test"))
                 (call-with-port (open-file-output-port
                                  (format "build/test/render-E1M~a-c~4,'0d.raw" m n)
                                  (file-options no-fail))
                   (lambda (p) (put-bytevector p frame))))
               (check (format "E1M~a c~4,'0d ~a" m n (car cams)) 0 d))
             (loop (cdr cams) (+ n 1) start))
           (car cams))))))

(let ([m (getenv "MAP")])
  (if m
      (run-map (string->number m))
      (do ([m 1 (+ m 1)]) ((> m 9))
        (unless (= 0 (system (format "MAP=~a scheme --libdirs src --script tests/render.ss" m)))
          (set! failures (+ failures 1))))))
(finish)
