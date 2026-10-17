;;; i_video: present screens[0] (8-bit palette indices) through SDL2.
;;; i-finish-update expands it to ARGB via the current palette and shows it
;;; 4:3 aspect-corrected.
(library (doom i_video)
  (export i-init-graphics i-shutdown-graphics i-set-palette i-finish-update
          i-read-pixels i-poll-events i-xlate-key i-get-events i-start-tic i-start-frame
          i-update-no-blit last-palette headless grabmouse joystick-enabled)
  (import (chezscheme) (doom global) (doom i_sdl) (doom doomdef) (doom v_video) (doom d_event) (doom i_system) (doom doomstat) (doom m_argv))

  (define window 0)
  (define renderer 0)
  (define texture 0)
  ;; The texture's pixels: 320x200, or k times that with -hires.
  (define argb (make-bytevector (* SCREENWIDTH SCREENHEIGHT 4) 0))
  ;; 256 palette entries pre-packed as native-endian ARGB8888 words.
  (define palette32 (make-fxvector 256 0))

  (define (check who x)
    (when (eqv? x 0) (errorf who "~a" (sdl-get-error)))
    x)

  ;; When true, I_InitGraphics opens no window (and so the frame is only
  ;; drawn into screens, as in the oracle): tests and timing runs.
  (define-global headless #f)
  ;; -grabmouse (see I_GetEvent)
  (define-global grabmouse #f)
  ;; use_joystick (d_main sets this from the config before I_InitGraphics)
  (define-global joystick-enabled #f)

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
    (sdl-render-set-logical-size renderer (* SCREENWIDTH hires-k) (* SCREENHEIGHT 6/5 hires-k))
    (set! argb (make-bytevector (* SCREENWIDTH SCREENHEIGHT 4 hires-k hires-k) 0))
    ;; -grabmouse: keep the pointer (SDL's relative mode, which also hides
    ;; it), as linuxdoom grabs and recenters it
    (set! grabmouse (fx> (m-check-parm "-grabmouse") 0))
    (when grabmouse (sdl-set-relative-mouse-mode 1))
    ;; use_joystick: the first game controller SDL knows
    (when (and joystick-enabled (>= (sdl-init-sub-system SDL_INIT_GAMECONTROLLER) 0))
      (let loop ([i 0])
        (when (fx< i (sdl-num-joysticks))
          (if (sdl-is-game-controller i)
              (sdl-game-controller-open i)
              (loop (fx+ i 1))))))
    (set! texture (check 'sdl-create-texture
                         (sdl-create-texture renderer SDL_PIXELFORMAT_ARGB8888
                                             SDL_TEXTUREACCESS_STREAMING
                                             (* SCREENWIDTH hires-k) (* SCREENHEIGHT hires-k)))))

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
    (if (fx= hires-k 1)
        (let ([n (fx* SCREENWIDTH SCREENHEIGHT)])
          (do ([i 0 (fx+ i 1)]) ((fx= i n))
            (bytevector-u32-native-set! argb (fxsll i 2)
                                        (fxvector-ref palette32 (bytevector-u8-ref screens i)))))
        (compose-hires))
    (sdl-update-texture texture 0 argb (fx* SCREENWIDTH hires-k 4))
    (sdl-render-clear renderer)
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer)))

;; -hires: screen 0 scaled up k times, except that inside the view window
  ;; (when the renderer drew one this frame) a pixel whose 320x200 pixel no
  ;; 2D drawing covered comes from the hi-res view.
  (define (compose-hires)
    (let* ([k hires-k] [w (fx* SCREENWIDTH k)] [h (fx* SCREENHEIGHT k)]
           [view hires-view] [buf hires-buf] [mask overlay-mask]
           [vx (if view (car view) 0)] [vy (if view (cadr view) 0)]
           [vx2 (if view (fx+ vx (caddr view)) 0)] [vy2 (if view (fx+ vy (cadddr view)) 0)])
      (do ([y 0 (fx+ y 1)]) ((fx= y h))
        (let ([lrow (fx* (fxquotient y k) SCREENWIDTH)] [out (fx* y w)]
              [inview (and view (fx<= vy y) (fx< y vy2))])
          (do ([x 0 (fx+ x 1)]) ((fx= x w))
            (let* ([l (fx+ lrow (fxquotient x k))]
                   [c (if (and inview (fx<= vx x) (fx< x vx2) (fx= 0 (bytevector-u8-ref mask l)))
                          (bytevector-u8-ref buf (fx+ out x))
                          (bytevector-u8-ref screens l))])
              (bytevector-u32-native-set! argb (fxsll (fx+ out x) 2) (fxvector-ref palette32 c))))))))

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
  ;;
  ;; The mouse as linuxdoom's X11 code: buttons 1-3 (left, middle, right)
  ;; are bits 1, 2 and 4 of data1. A press posts the buttons held before it
  ;; or'ed with the one pressed, a release those held before xor'ed with the
  ;; one released, and motion the buttons held, with data2 = dx<<2 and
  ;; data3 = -dy<<2 (unless both are 0).
  (define mousebuttons 0)                ; held, as X's state mask

  ;; The joystick, which linuxdoom's X11 code never read but the game does
  ;; (G_Responder): an ev_joystick with data1 the buttons (bits 1, 2, 4, 8:
  ;; the config's joyb_* numbers 0-3) and data2/data3 the x and y axes,
  ;; -1, 0 or 1 (DOOM's joystick is digital), posted when any of them
  ;; changes. From a game controller: the left stick (past a dead zone) or
  ;; the d-pad are the axes; A, B, X, Y are buttons 0-3, and the right and
  ;; left triggers are buttons 0 and 2 too (fire and run, by default).
  ;; Start and Back are the Escape and Tab keys (the menu, the automap).
  (define JOYDEADZONE 8000)
  (define joybuttons 0)
  (define stick (make-fxvector 2 0))    ; left stick x, y
  (define dpad (make-fxvector 4 0))     ; up, down, left, right
  (define triggers (make-fxvector 2 0)) ; left, right
  (define lastjoy (list 0 0 0))
  (define (joy-axis neg pos stickv)
    (cond [(or (fx= neg 1) (fx< stickv (fx- 0 JOYDEADZONE))) -1]
          [(or (fx= pos 1) (fx> stickv JOYDEADZONE)) 1]
          [else 0]))
  (define (post-joystick post)
    (let* ([buttons (fxior joybuttons
                           (if (fx= (fxvector-ref triggers 1) 1) 1 0)
                           (if (fx= (fxvector-ref triggers 0) 1) 4 0))]
           [x (joy-axis (fxvector-ref dpad 2) (fxvector-ref dpad 3) (fxvector-ref stick 0))]
           [y (joy-axis (fxvector-ref dpad 0) (fxvector-ref dpad 1) (fxvector-ref stick 1))]
           [now (list buttons x y)])
      (unless (equal? now lastjoy)
        (set! lastjoy now)
        (post (make-event ev_joystick buttons x y)))))
  (define (controller-button ev post down?)
    (let ([b (sdl-event-cbutton-button ev)])
      (case b
        [(0 1 2 3)                      ; A B X Y
         (set! joybuttons (if down?
                              (fxior joybuttons (fxsll 1 b))
                              (fxand joybuttons (fxnot (fxsll 1 b)))))
         (post-joystick post)]
        [(11 12 13 14)                  ; d-pad up, down, left, right
         (fxvector-set! dpad (fx- b 11) (if down? 1 0))
         (post-joystick post)]
        [(6) (post (make-event (if down? ev_keydown ev_keyup) KEY_ESCAPE 0 0))] ; start
        [(4) (post (make-event (if down? ev_keydown ev_keyup) KEY_TAB 0 0))]    ; back
        [else (void)])))
  (define (controller-axis ev post)
    (let ([a (sdl-event-caxis-axis ev)] [v (sdl-event-caxis-value ev)])
      (case a
        [(0 1) (fxvector-set! stick a v)]                         ; left stick
        [(4 5) (fxvector-set! triggers (fx- a 4) (if (fx> v 16000) 1 0))] ; triggers
        [else (void)])
      (post-joystick post)))
  (define (button-bit b) (case b [(1) 1] [(2) 2] [(3) 4] [else 0]))
  (define (i-get-events post)
    (i-poll-events
     (lambda (ev)
       (let ([type (sdl-event-type ev)])
         (cond
           [(or (fx= type SDL_KEYDOWN) (fx= type SDL_KEYUP))
            (let ([key (i-xlate-key (sdl-event-key-sym ev))])
              (when key
                (post (make-event (if (fx= type SDL_KEYDOWN) ev_keydown ev_keyup)
                                  key 0 0))))]
           [(fx= type SDL_MOUSEBUTTONDOWN)
            (let ([b (button-bit (sdl-event-mouse-button ev))])
              (post (make-event ev_mouse (fxior mousebuttons b) 0 0))
              (set! mousebuttons (fxior mousebuttons b)))]
           [(fx= type SDL_MOUSEBUTTONUP)
            (let ([b (button-bit (sdl-event-mouse-button ev))])
              (post (make-event ev_mouse (fxxor mousebuttons b) 0 0))
              (set! mousebuttons (fxand mousebuttons (fxnot b))))]
           [(fx= type SDL_CONTROLLERBUTTONDOWN) (controller-button ev post #t)]
           [(fx= type SDL_CONTROLLERBUTTONUP) (controller-button ev post #f)]
           [(fx= type SDL_CONTROLLERAXISMOTION) (controller-axis ev post)]
           [(fx= type SDL_MOUSEMOTION)
            (let ([dx (fxsll (sdl-event-mouse-xrel ev) 2)]
                  [dy (fxsll (fx- 0 (sdl-event-mouse-yrel ev)) 2)])
              (unless (and (fx= dx 0) (fx= dy 0))
                (post (make-event ev_mouse mousebuttons dx dy))))])))))

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
