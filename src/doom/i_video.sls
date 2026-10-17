;;; i_video: present screens[0] (8-bit palette indices) through SDL2.
;;; i-finish-update expands it to ARGB via the current palette and shows it
;;; 4:3 aspect-corrected.
(library (doom i_video)
  (export i-init-graphics i-shutdown-graphics i-set-palette i-finish-update
          i-read-pixels i-poll-events i-xlate-key i-get-events i-start-tic i-start-frame
          i-update-no-blit last-palette headless)
  (import (chezscheme) (doom global) (doom i_sdl) (doom doomdef) (doom v_video) (doom d_event) (doom i_system) (doom doomstat))

  (define window 0)
  (define renderer 0)
  (define texture 0)
  (define argb (make-bytevector (* SCREENWIDTH SCREENHEIGHT 4) 0))
  ;; 256 palette entries pre-packed as native-endian ARGB8888 words.
  (define palette32 (make-fxvector 256 0))

  (define (check who x)
    (when (eqv? x 0) (errorf who "~a" (sdl-get-error)))
    x)

  ;; When true, I_InitGraphics opens no window (and so the frame is only
  ;; drawn into screens, as in the oracle): tests and timing runs.
  (define-global headless #f)

  (define (i-init-graphics)
    (unless headless (init-graphics)))

  (define (init-graphics)
    (when (< (sdl-init (logor SDL_INIT_VIDEO SDL_INIT_EVENTS)) 0)
      (errorf 'i-init-graphics "~a" (sdl-get-error)))
    (set! window (check 'sdl-create-window
                        (sdl-create-window "DOOM" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED
                                           (* SCREENWIDTH 3) (* SCREENHEIGHT 3 6/5)
                                           SDL_WINDOW_RESIZABLE)))
    (set! renderer (check 'sdl-create-renderer
                          (sdl-create-renderer window -1 SDL_RENDERER_PRESENTVSYNC)))
    ;; 320x240 logical size stretches 200 lines to 4:3, like a VGA monitor.
    (sdl-render-set-logical-size renderer SCREENWIDTH (* SCREENHEIGHT 6/5))
    (set! texture (check 'sdl-create-texture
                         (sdl-create-texture renderer SDL_PIXELFORMAT_ARGB8888
                                             SDL_TEXTUREACCESS_STREAMING
                                             SCREENWIDTH SCREENHEIGHT))))

  (define (i-shutdown-graphics)
    (unless (eqv? window 0) (sdl-destroy-window window) (set! window 0))
    (sdl-quit))

  ;; I_SetPalette: 768 bytes of RGB triples (one PLAYPAL palette, starting at
  ;; `offset`), each component passed through gammatable[usegamma] as in
  ;; linuxdoom.
  ;; The 768 bytes last passed to I_SetPalette (the trace hashes these, as
  ;; the oracle's null video layer keeps them).
  (define last-palette (make-bytevector 768 0))

  (define i-set-palette
    (case-lambda
      [(pal) (i-set-palette pal 0)]
      [(pal offset)
       (bytevector-copy! pal offset last-palette 0 768)
       (let ([gamma (fx* usegamma 256)])
         (define (c j)
           (bytevector-u8-ref gammatable
                              (fx+ gamma (bytevector-u8-ref pal (fx+ offset j)))))
         (do ([i 0 (fx+ i 1)]) ((fx= i 256))
           (let ([j (fx* i 3)])
             (fxvector-set! palette32 i
                            (fxior #xFF000000
                                   (fxsll (c j) 16)
                                   (fxsll (c (fx+ j 1)) 8)
                                   (c (fx+ j 2)))))))]))

  ;; I_FinishUpdate: show screens[0].
  (define lasttic 0)
  (define (i-finish-update)
   ;; draws little dots on the bottom of the screen
   (when devparm
     (let* ([i (i-get-time)]
            [tics (fxmin 20 (fx- i lasttic))]
            [row (fx* (fx- SCREENHEIGHT 1) SCREENWIDTH)])
       (set! lasttic i)
       (do ([i 0 (fx+ i 2)]) ((fx>= i 40))
         (bytevector-u8-set! screens (fx+ row i) (if (fx< i (fx* tics 2)) #xff 0)))))
   (unless (eqv? window 0)
    (let ([n (fx* SCREENWIDTH SCREENHEIGHT)])
      (do ([i 0 (fx+ i 1)]) ((fx= i n))
        (bytevector-u32-native-set! argb (fxsll i 2)
                                    (fxvector-ref palette32 (bytevector-u8-ref screens i)))))
    (sdl-update-texture texture 0 argb (fx* SCREENWIDTH 4))
    (sdl-render-clear renderer)
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer)))

  ;; For tests/screenshots: the whole presented window (physical pixels, so
  ;; including scaling and letterboxing) as ARGB8888 bytes.
  (define (i-read-pixels)
    (let-values ([(w h) (sdl-get-renderer-output-size renderer)])
      (let ([bv (make-bytevector (* w h 4))])
        (sdl-render-read-pixels renderer 0 SDL_PIXELFORMAT_ARGB8888 bv (* w 4))
        (values w h bv))))

  ;; Drain SDL's queue, calling (handle ev) for each. Returns #f on SDL_QUIT.
  (define ev (make-sdl-event))
  (define (i-poll-events handle)
    (let loop ()
      (cond
        [(fx= (sdl-poll-event ev) 0) #t]
        [(fx= (sdl-event-type ev) SDL_QUIT) #f]
        [else (handle ev) (loop)])))

  ;; xlatekey: SDL keycode -> doom key code, or #f for keys doom ignores.
  ;; Follows linuxdoom's X11 table; SDL keycodes for printable keys are
  ;; already lowercase ASCII.
  (define (i-xlate-key sym)
    (case sym
      [(#x40000050) KEY_LEFTARROW]
      [(#x4000004F) KEY_RIGHTARROW]
      [(#x40000051) KEY_DOWNARROW]
      [(#x40000052) KEY_UPARROW]
      [(27) KEY_ESCAPE]
      [(13 #x40000058) KEY_ENTER]               ; Return, keypad Enter
      [(9) KEY_TAB]
      [(#x4000003A) KEY_F1] [(#x4000003B) KEY_F2] [(#x4000003C) KEY_F3]
      [(#x4000003D) KEY_F4] [(#x4000003E) KEY_F5] [(#x4000003F) KEY_F6]
      [(#x40000040) KEY_F7] [(#x40000041) KEY_F8] [(#x40000042) KEY_F9]
      [(#x40000043) KEY_F10] [(#x40000044) KEY_F11] [(#x40000045) KEY_F12]
      [(8 127) KEY_BACKSPACE]                   ; Backspace, Delete
      [(#x40000048) KEY_PAUSE]
      [(#x40000067 61) KEY_EQUALS]              ; keypad =, =
      [(#x40000056 45) KEY_MINUS]               ; keypad -, -
      [(#x400000E1 #x400000E5) KEY_RSHIFT]
      [(#x400000E0 #x400000E4) KEY_RCTRL]
      [(#x400000E2 #x400000E6 #x400000E3 #x400000E7) KEY_RALT]  ; Alt, Meta/GUI
      [else (and (fx<= 32 sym 126)
                 (if (fx<= 65 sym 90) (fx+ sym 32) sym))]))

  ;; I_GetEvent: translate pending SDL input into doom events, calling
  ;; (post event) for each (C: D_PostEvent). Returns #f on SDL_QUIT.
  ;; TODO(milestone 4): mouse.
  (define (i-get-events post)
    (i-poll-events
     (lambda (ev)
       (let ([type (sdl-event-type ev)])
         (when (or (fx= type SDL_KEYDOWN) (fx= type SDL_KEYUP))
           (let ([key (i-xlate-key (sdl-event-key-sym ev))])
             (when key
               (post (make-event (if (fx= type SDL_KEYDOWN) ev_keydown ev_keyup)
                                 key 0 0)))))))))

  ;; I_StartTic: process pending input, posting events with `post` (C calls
  ;; D_PostEvent, which is above this library). Closing the window quits.
  ;; Does nothing before I_InitGraphics.
  (define (i-start-tic post)
    (unless (eqv? window 0)
      (unless (i-get-events post)
        (i-quit))))

  ;; I_StartFrame, I_UpdateNoBlit: nothing to do here.
  (define (i-start-frame) (void))
  (define (i-update-no-blit) (void)))
