;;; I_GetEvent's translation of SDL input into doom events, through SDL's
;;; own event queue (SDL_PushEvent, dummy video driver): keys by
;;; linuxdoom's xlatekey table, and the mouse as linuxdoom's X11 code posts
;;; it (button bits 1/2/4, a press or'ed into the buttons held and a
;;; release xor'ed, motion as dx<<2 and -dy<<2).
(import (chezscheme) (doom i_sdl) (doom i_video) (doom d_event) (doom doomdef))
(load "tests/lib.ss")

(putenv "SDL_VIDEODRIVER" "dummy")
(sdl-init (logor SDL_INIT_VIDEO SDL_INIT_EVENTS))

(define (sdl-event type . fields)
  (let ([ev (make-bytevector 56 0)])
    (bytevector-u32-native-set! ev 0 type)
    (let loop ([f fields])
      (unless (null? f)
        (let ([off (car f)] [kind (cadr f)] [v (caddr f)])
          (case kind
            [(u8) (bytevector-u8-set! ev off v)]
            [(s32) (bytevector-s32-native-set! ev off v)])
          (loop (cdddr f)))))
    (sdl-push-event ev)))
(define (key type sym) (sdl-event type 20 's32 sym))
(define (button type b) (sdl-event type 16 'u8 b 17 'u8 (if (= type SDL_MOUSEBUTTONDOWN) 1 0)))
(define (motion dx dy) (sdl-event SDL_MOUSEMOTION 28 's32 dx 32 's32 dy))

(define (events)
  (let ([acc '()])
    (i-get-events (lambda (e) (set! acc (cons (list (event-type e) (event-data1 e) (event-data2 e)
                                                    (event-data3 e))
                                              acc))))
    (reverse acc)))
(events)                                ; (whatever SDL queued at startup)

(key SDL_KEYDOWN #x40000052) (key SDL_KEYUP #x40000052) (key SDL_KEYDOWN 65)
(key SDL_KEYDOWN #x400000E0) (key SDL_KEYDOWN #x40000044) (key SDL_KEYDOWN #x400000FF)
(check "keys: arrows, letters (lower case), ctrl, F11; unknown keys dropped"
       `((,ev_keydown ,KEY_UPARROW 0 0) (,ev_keyup ,KEY_UPARROW 0 0) (,ev_keydown 97 0 0)
         (,ev_keydown ,KEY_RCTRL 0 0) (,ev_keydown ,KEY_F11 0 0))
       (events))

(button SDL_MOUSEBUTTONDOWN 1) (motion 3 -2) (button SDL_MOUSEBUTTONDOWN 3)
(button SDL_MOUSEBUTTONUP 1) (motion 0 0) (motion -1 0) (button SDL_MOUSEBUTTONUP 3)
(button SDL_MOUSEBUTTONDOWN 2) (button SDL_MOUSEBUTTONUP 2)
(check "mouse: buttons held and pressed, motion scaled by 4 with y up"
       `((,ev_mouse 1 0 0) (,ev_mouse 1 12 8) (,ev_mouse 5 0 0) (,ev_mouse 4 0 0)
         (,ev_mouse 4 -4 0) (,ev_mouse 0 0 0) (,ev_mouse 2 0 0) (,ev_mouse 0 0 0))
       (events))

(sdl-quit)
(finish)
