;;; Milestone 0 smoke test: open the window and show a palette test pattern
;;; until the window is closed or ESC is pressed. FRAMES=n exits after n frames.
(import (chezscheme) (i sdl) (i video))

(define screen (make-bytevector (* SCREENWIDTH SCREENHEIGHT) 0))
(define pal (make-bytevector 768 0))

;; A placeholder palette (a hue sweep) until w_wad can load PLAYPAL.
(do ([i 0 (fx+ i 1)]) ((fx= i 256))
  (bytevector-u8-set! pal (* i 3) i)
  (bytevector-u8-set! pal (+ (* i 3) 1) (fxand (fx* i 4) 255))
  (bytevector-u8-set! pal (+ (* i 3) 2) (fx- 255 i)))

(define (draw-pattern! t)
  (do ([y 0 (fx+ y 1)]) ((fx= y SCREENHEIGHT))
    (do ([x 0 (fx+ x 1)]) ((fx= x SCREENWIDTH))
      (bytevector-u8-set! screen (fx+ (fx* y SCREENWIDTH) x)
                          (fxand (fx+ (fx+ x y) t) 255)))))

(define max-frames
  (let ([s (getenv "FRAMES")]) (and s (string->number s))))

(i-init-graphics)
(i-set-palette pal)
(let loop ([t 0])
  (draw-pattern! t)
  (i-finish-update screen)
  (when (and (i-poll-events
              (lambda (ev)
                (when (and (fx= (sdl-event-type ev) SDL_KEYDOWN)
                           (fx= (sdl-event-key-sym ev) 27))
                  (exit 0))))
             (not (and max-frames (fx>= t max-frames))))
    (loop (fx+ t 1))))
(i-shutdown-graphics)
