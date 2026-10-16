;;; Milestone 1 demo: the title pages through w_wad + v_video + i_video.
;;; Any key advances TITLEPIC -> CREDIT -> HELP1 -> HELP2 (with a status bar
;;; and flipped face drawn over HELP2 to exercise the patch drawers).
;;; ESC or closing the window quits.
;;;   DOOMWAD=path   (default wads/doom1.wad)
;;;   SHOT=out.ppm   draw each page once, save the presented frames, and exit
(import (chezscheme) (doom i_sdl) (doom i_video) (doom w_wad) (doom v_video)
        (doom doomdef))

(define pages '("TITLEPIC" "CREDIT" "HELP1" "HELP2"))

(define (draw-page! i)
  (let ([name (list-ref pages i)])
    (v-draw-patch 0 0 0 (w-cache-lump-name name))
    (when (string=? name "HELP2")
      (v-draw-patch 0 (- SCREENHEIGHT 32) 0 (w-cache-lump-name "STBAR"))
      (v-draw-patch-flipped 143 (- SCREENHEIGHT 32) 0 (w-cache-lump-name "STFST01")))))

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

(w-init-multiple-files (list (or (getenv "DOOMWAD") "wads/doom1.wad")))
(v-init)
(i-init-graphics)
(i-set-palette (w-cache-lump-name "PLAYPAL"))

(let ([shot (getenv "SHOT")])
  (if shot
      (do ([i 0 (+ i 1)]) ((= i (length pages)))
        (draw-page! i)
        (i-finish-update)
        (save-shot (format "~a-~a.ppm" (path-root shot) (list-ref pages i))))
      (let loop ([page 0])
        (draw-page! page)
        (i-finish-update)
        (let ([next page] [quit #f])
          (unless (i-poll-events
                   (lambda (ev)
                     (when (fx= (sdl-event-type ev) SDL_KEYDOWN)
                       (if (fx= (sdl-event-key-sym ev) 27)
                           (set! quit #t)
                           (set! next (mod (+ page 1) (length pages)))))))
            (set! quit #t))
          (unless quit
            (sdl-delay 10)
            (loop next))))))
(i-shutdown-graphics)
