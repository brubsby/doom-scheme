;;; Milestone 2 demo: a map loaded by p_setup, shown on the automap.
;;; Starts on E1M1 with the automap open and the iddt cheat on (nothing has
;;; been seen yet, since there is no renderer, so without it no lines would
;;; show). The automap keys work as in Doom: arrows pan once follow mode is
;;; off ('f'), '-' / '=' zoom, '0' whole map, 'g' grid, 'm' mark, 'c' clear
;;; marks, iddt cycles the cheat. Demo-only keys: '[' / ']' previous / next
;;; map, ESC quits.
;;;   DOOMWAD=path   (default wads/doom1.wad)
;;;   MAP=n          start on E1Mn (default 1)
;;;   SHOT=out.ppm   save every map's automap (whole-map view) and exit
(import (chezscheme) (doom i_sdl) (doom i_video) (doom i_system) (doom w_wad)
        (doom v_video) (doom doomdef) (doom doomstat) (doom d_event) (doom r_data)
        (doom p_setup) (doom am_map))

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

(define (press key)
  (am-responder (make-event ev_keydown key 0 0))
  (am-responder (make-event ev_keyup key 0 0)))

(define nummaps 9)

;; Load E1Mn and (re)open the automap on it.
(define (load-map! n)
  (when automapactive (press KEY_TAB))
  (set! gameepisode 1)
  (set! gamemap n)
  (p-setup-level 1 n 0 sk_medium)
  (press KEY_TAB))

(define (draw!)
  (am-drawer)
  (v-draw-patch 0 (- SCREENHEIGHT 32) 0 (w-cache-lump-name "STBAR"))
  (i-finish-update))

(w-init-multiple-files (list (or (getenv "DOOMWAD") "wads/doom1.wad")))
(set! gamemode shareware)
(r-init-textures)
(r-init-flats)
(v-init)
(i-init-graphics)
(i-set-palette (w-cache-lump-name "PLAYPAL"))
(vector-set! playeringame 0 #t)

(load-map! (let ([m (getenv "MAP")]) (if m (string->number m) 1)))
(for-each press (map char->integer (string->list "iddt")))

(let ([shot (getenv "SHOT")])
  (if shot
      (do ([m 1 (+ m 1)]) ((> m nummaps))
        (load-map! m)
        (press (char->integer #\0))
        (am-ticker)
        (draw!)
        (save-shot (format "~a-E1M~a.ppm" (path-root shot) m)))
      (let loop ([lasttic (i-get-time)])
        (let ([quit #f])
          (unless (i-get-events
                   (lambda (ev)
                     (let ([key (event-data1 ev)])
                       (cond
                         [(fx= key KEY_ESCAPE) (set! quit #t)]
                         ;; The map stays open in this demo.
                         [(fx= key KEY_TAB) (void)]
                         [(and (fx= (event-type ev) ev_keydown) (memv key '(91 93)))
                          (load-map! (+ 1 (mod (+ (- gamemap 1) (if (fx= key 93) 1 -1)) nummaps)))]
                         [else (am-responder ev)]))))
            (set! quit #t))
          (unless quit
            ;; Run the 35 Hz tickers for elapsed tics, then draw.
            (let ([now (i-get-time)])
              (do ([t lasttic (+ t 1)]) ((>= t now)) (am-ticker))
              (draw!)
              (sdl-delay 5)
              (loop now)))))))
(i-shutdown-graphics)
