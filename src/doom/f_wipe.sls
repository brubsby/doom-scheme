;;; f_wipe: the screen wipe package (the melt, and the unused colour
;;; crossfade).
;;;
;;; Screens are offsets into v_video's one `screens` bytevector:
;;; wipe_scr_start is screen 2, wipe_scr_end screen 3, and wipe_scr screen
;;; 0. The melt moves pixels in pairs (C's shorts).
(library (doom f_wipe)
  (export wipe-start-screen wipe-end-screen wipe-screen-wipe wipe_ColorXForm wipe_Melt)
  (import (chezscheme) (doom doomdef) (doom v_video) (doom m_random))

  (define wipe_ColorXForm 0)
  (define wipe_Melt 1)

  (define go #f)                        ; when false, stop the wipe
  (define wipe-scr-start (screen-base 2))
  (define wipe-scr-end (screen-base 3))
  (define wipe-scr (screen-base 0))

  ;; C's shorts, at short index i from byte offset base.
  (define-syntax s16-ref
    (syntax-rules () [(_ base i) (bytevector-u16-native-ref screens (fx+ base (fx* 2 i)))]))
  (define-syntax s16-set!
    (syntax-rules () [(_ base i v) (bytevector-u16-native-set! screens (fx+ base (fx* 2 i)) v)]))

  ;; wipe_shittyColMajorXform: transpose width*height shorts in place.
  (define (wipe-shitty-col-major-xform base width height)
    (let ([dest (make-bytevector (fx* width height 2))])
      (do ([y 0 (fx+ y 1)]) ((fx= y height))
        (do ([x 0 (fx+ x 1)]) ((fx= x width))
          (bytevector-u16-native-set! dest (fx* 2 (fx+ (fx* x height) y))
                                      (s16-ref base (fx+ (fx* y width) x)))))
      (bytevector-copy! dest 0 screens base (fx* width height 2))))

  (define (wipe-init-color-xform width height ticks)
    (bytevector-copy! screens wipe-scr-start screens wipe-scr (fx* width height))
    #f)

  (define (wipe-do-color-xform width height ticks)
    (let loop ([i 0] [changed #f])
      (if (fx= i (fx* width height))
          (not changed)
          (let ([w (bytevector-u8-ref screens (fx+ wipe-scr i))]
                [e (bytevector-u8-ref screens (fx+ wipe-scr-end i))])
            (cond
              [(fx> w e)
               (bytevector-u8-set! screens (fx+ wipe-scr i) (fxmax e (fx- w ticks)))
               (loop (fx+ i 1) #t)]
              [(fx< w e)
               (bytevector-u8-set! screens (fx+ wipe-scr i) (fxmin e (fx+ w ticks)))
               (loop (fx+ i 1) #t)]
              [else (loop (fx+ i 1) changed)])))))

  (define (wipe-exit-color-xform width height ticks) #f)

  (define y (make-fxvector 0))

  (define (wipe-init-melt width height ticks)
    ;; copy start screen to main screen
    (bytevector-copy! screens wipe-scr-start screens wipe-scr (fx* width height))
    ;; makes this wipe faster (in theory) to have stuff in column-major
    ;; format
    (wipe-shitty-col-major-xform wipe-scr-start (fxquotient width 2) height)
    (wipe-shitty-col-major-xform wipe-scr-end (fxquotient width 2) height)
    ;; setup initial column positions (y<0 => not ready to scroll yet)
    (set! y (make-fxvector width 0))
    (fxvector-set! y 0 (fx- 0 (fxremainder (m-random) 16)))
    (do ([i 1 (fx+ i 1)]) ((fx= i width))
      (let* ([r (fx- (fxremainder (m-random) 3) 1)]
             [v (fx+ (fxvector-ref y (fx- i 1)) r)])
        (fxvector-set! y i (cond [(fx> v 0) 0] [(fx= v -16) -15] [else v]))))
    #f)

  (define (wipe-do-melt width height ticks)
    (let ([width (fxquotient width 2)])
      (let tick ([ticks ticks] [done #t])
        (if (fx= ticks 0)
            done
            (let col ([i 0] [done done])
              (if (fx= i width)
                  (tick (fx- ticks 1) done)
                  (let ([yi (fxvector-ref y i)])
                    (cond
                      [(fx< yi 0)
                       (fxvector-set! y i (fx+ yi 1))
                       (col (fx+ i 1) #f)]
                      [(fx< yi height)
                       (let* ([dy (if (fx< yi 16) (fx+ yi 1) 8)]
                              [dy (if (fx>= (fx+ yi dy) height) (fx- height yi) dy)])
                         (do ([j 0 (fx+ j 1)]) ((fx= j dy))
                           (s16-set! wipe-scr (fx+ (fx* (fx+ yi j) width) i)
                                     (s16-ref wipe-scr-end (fx+ (fx* i height) yi j))))
                         (let ([yi (fx+ yi dy)])
                           (fxvector-set! y i yi)
                           (do ([j 0 (fx+ j 1)]) ((fx= j (fx- height yi)))
                             (s16-set! wipe-scr (fx+ (fx* (fx+ yi j) width) i)
                                       (s16-ref wipe-scr-start (fx+ (fx* i height) j))))))
                       (col (fx+ i 1) #f)]
                      [else (col (fx+ i 1) done)]))))))))

  (define (wipe-exit-melt width height ticks) #f)

  ;; wipe_StartScreen: I_ReadScreen into screen 2.
  (define (wipe-start-screen x y width height)
    (bytevector-copy! screens (screen-base 0) screens wipe-scr-start (fx* SCREENWIDTH SCREENHEIGHT)))

  ;; wipe_EndScreen: I_ReadScreen into screen 3, then restore the start
  ;; screen.
  (define (wipe-end-screen x y width height)
    (bytevector-copy! screens (screen-base 0) screens wipe-scr-end (fx* SCREENWIDTH SCREENHEIGHT))
    (v-draw-block x y 0 width height screens wipe-scr-start))

  (define wipes
    (vector wipe-init-color-xform wipe-do-color-xform wipe-exit-color-xform
            wipe-init-melt wipe-do-melt wipe-exit-melt))

  ;; wipe_ScreenWipe: returns true when the wipe is done.
  (define (wipe-screen-wipe wipeno x y width height ticks)
    ;; initial stuff
    (unless go
      (set! go #t)
      ((vector-ref wipes (fx* wipeno 3)) width height ticks))
    ;; do a piece of wipe-in
    (v-mark-rect 0 0 width height)
    (when ((vector-ref wipes (fx+ (fx* wipeno 3) 1)) width height ticks)
      ;; final stuff
      (set! go #f)
      ((vector-ref wipes (fx+ (fx* wipeno 3) 2)) width height ticks))
    (not go)))
