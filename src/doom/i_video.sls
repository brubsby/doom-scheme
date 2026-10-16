;;; i_video: present screens[0] (8-bit palette indices) through SDL2.
;;; i-finish-update expands it to ARGB via the current palette and shows it
;;; 4:3 aspect-corrected.
(library (doom i_video)
  (export i-init-graphics i-shutdown-graphics i-set-palette i-finish-update
          i-read-pixels i-poll-events)
  (import (chezscheme) (doom i_sdl) (doom doomdef) (doom v_video))

  (define window 0)
  (define renderer 0)
  (define texture 0)
  (define argb (make-bytevector (* SCREENWIDTH SCREENHEIGHT 4) 0))
  ;; 256 palette entries pre-packed as native-endian ARGB8888 words.
  (define palette32 (make-fxvector 256 0))

  (define (check who x)
    (when (eqv? x 0) (errorf who "~a" (sdl-get-error)))
    x)

  (define (i-init-graphics)
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
  (define i-set-palette
    (case-lambda
      [(pal) (i-set-palette pal 0)]
      [(pal offset)
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
  (define (i-finish-update)
    (let ([n (fx* SCREENWIDTH SCREENHEIGHT)])
      (do ([i 0 (fx+ i 1)]) ((fx= i n))
        (bytevector-u32-native-set! argb (fxsll i 2)
                                    (fxvector-ref palette32 (bytevector-u8-ref screens i)))))
    (sdl-update-texture texture 0 argb (fx* SCREENWIDTH 4))
    (sdl-render-clear renderer)
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer))

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
        [else (handle ev) (loop)]))))
