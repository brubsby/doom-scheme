;;; r_interp: frame interpolation for -uncapped (no C counterpart).
;;;
;;; The playsim runs at 35 tics a second; with -uncapped the main loop draws
;;; as often as the display allows, and each frame shows the world between
;;; the last two tics: things' positions and angles, sector floor and
;;; ceiling heights, the player's view height and the weapon sprites' offsets
;;; are moved to prev + frac*(cur - prev) for the render, then put back. The
;;; tic logic never sees an interpolated value.
;;;
;;; interp-save-tic! runs at the start of every tic and records the "prev"
;;; values. Something with no record from the last tic (spawned during it,
;;; or the level just loaded) is drawn where it is, and so is anything that
;;; moved more than SNAPDIST in one tic (a teleport).
(library (doom r_interp)
  (export interp-save-tic! interp-begin! interp-end! interp-enabled interp-frames)
  (import (chezscheme) (doom global) (doom doomdef) (doom doomstat) (doom d_think)
          (doom d_player) (doom r_defs) (doom r_state) (doom p_mobj.h) (doom p_local))

  (define-global interp-enabled #f)
  ;; Frames drawn interpolated (for tests).
  (define-global interp-frames 0)

  (define SNAPDIST (* 64 #x10000))

  ;; mobj or pspdef or player -> fxvector: the tic it was saved at, the prev
  ;; values, and (during a render) the current values.
  (define saved (make-weak-eq-hashtable))
  (define sector-prev (make-fxvector 0))   ; floor, ceiling per sector
  (define sector-cur (make-fxvector 0))
  (define sector-tic (make-fxvector 0))
  (define sector-vec #f)                   ; the sectors these are for

  (define (record obj n)
    (or (hashtable-ref saved obj #f)
        (let ([v (make-fxvector (fx+ 1 (fx* 2 n)) -1)])
          (hashtable-set! saved obj v)
          v)))

  (define (each-mobj f)
    ;; (no list before the first level)
    (let loop ([th (or (thinker-next thinkercap) thinkercap)])
      (unless (eq? th thinkercap)
        (let ([next (thinker-next th)])
          (when (eq? (thinker-func th) p-mobj-thinker) (f th))
          (loop next)))))

  (define (each-player f)
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i) (f (vector-ref players i)))))

  ;; At the start of tic gametic: remember where everything is.
  (define (interp-save-tic!)
    (when interp-enabled
      (let ([tic gametic])
        (each-mobj
         (lambda (mo)
           (let ([v (record mo 4)])
             (fxvector-set! v 0 tic)
             (fxvector-set! v 1 (mobj-x mo))
             (fxvector-set! v 2 (mobj-y mo))
             (fxvector-set! v 3 (mobj-z mo))
             (fxvector-set! v 4 (mobj-angle mo)))))
        (each-player
         (lambda (p)
           (let ([v (record p 1)])
             (fxvector-set! v 0 tic)
             (fxvector-set! v 1 (player-viewz p)))
           (vector-for-each
            (lambda (psp)
              (let ([v (record psp 2)])
                (fxvector-set! v 0 tic)
                (fxvector-set! v 1 (pspdef-sx psp))
                (fxvector-set! v 2 (pspdef-sy psp))))
            (player-psprites p))))
        (unless (eq? sector-vec sectors)
          (set! sector-vec sectors)
          (set! sector-prev (make-fxvector (fx* 2 numsectors) 0))
          (set! sector-cur (make-fxvector (fx* 2 numsectors) 0))
          (set! sector-tic (make-fxvector numsectors -1)))
        (do ([i 0 (fx+ i 1)]) ((fx= i numsectors))
          (let ([s (vector-ref sectors i)])
            (fxvector-set! sector-tic i tic)
            (fxvector-set! sector-prev (fx* 2 i) (sector-floorheight s))
            (fxvector-set! sector-prev (fx+ 1 (fx* 2 i)) (sector-ceilingheight s)))))))

  ;; frac: 0 to FRACUNIT, the time since the last tic in tics.
  (define (lerp a b frac) (fx+ a (fxsra (fx* (fx- b a) frac) 16)))
  (define (lerp-angle a b frac)
    (let* ([d (fx- b a)]
           [d (cond [(fx> d #x7fffffff) (fx- d #x100000000)]
                    [(fx< d (fx- #x80000000)) (fx+ d #x100000000)]
                    [else d])])
      (fxand (fx+ a (fxsra (fx* d frac) 16)) #xffffffff)))

  (define active #f)

  ;; Move everything to its interpolated place (and keep the real values).
  (define (interp-begin! frac)
    (when (and interp-enabled (fx< frac #x10000))
      (set! active #t)
      (set! interp-frames (fx+ interp-frames 1))
      (let ([last (fx- gametic 1)])
        (define (fresh v) (and v (fx= (fxvector-ref v 0) last)))
        (each-mobj
         (lambda (mo)
           (let ([v (hashtable-ref saved mo #f)])
             (when (fresh v)
               (let ([x (mobj-x mo)] [y (mobj-y mo)] [z (mobj-z mo)] [a (mobj-angle mo)])
                 (fxvector-set! v 5 x) (fxvector-set! v 6 y) (fxvector-set! v 7 z) (fxvector-set! v 8 a)
                 (if (fx> (fx+ (fxabs (fx- x (fxvector-ref v 1))) (fxabs (fx- y (fxvector-ref v 2))))
                          SNAPDIST)
                     (fxvector-set! v 0 -1)     ; teleported: drawn where it is
                     (begin
                       (mobj-x-set! mo (lerp (fxvector-ref v 1) x frac))
                       (mobj-y-set! mo (lerp (fxvector-ref v 2) y frac))
                       (mobj-z-set! mo (lerp (fxvector-ref v 3) z frac))
                       (mobj-angle-set! mo (lerp-angle (fxvector-ref v 4) a frac)))))))))
        (each-player
         (lambda (p)
           (let ([v (hashtable-ref saved p #f)])
             (when (fresh v)
               (fxvector-set! v 2 (player-viewz p))
               (player-viewz-set! p (lerp (fxvector-ref v 1) (player-viewz p) frac))))
           (vector-for-each
            (lambda (psp)
              (let ([v (hashtable-ref saved psp #f)])
                (when (fresh v)
                  (fxvector-set! v 3 (pspdef-sx psp))
                  (fxvector-set! v 4 (pspdef-sy psp))
                  (pspdef-sx-set! psp (lerp (fxvector-ref v 1) (pspdef-sx psp) frac))
                  (pspdef-sy-set! psp (lerp (fxvector-ref v 2) (pspdef-sy psp) frac)))))
            (player-psprites p))))
        (when (eq? sector-vec sectors)
          (do ([i 0 (fx+ i 1)]) ((fx= i numsectors))
            (when (fx= (fxvector-ref sector-tic i) last)
              (let ([s (vector-ref sectors i)] [f (fx* 2 i)])
                (fxvector-set! sector-cur f (sector-floorheight s))
                (fxvector-set! sector-cur (fx+ f 1) (sector-ceilingheight s))
                (sector-floorheight-set! s (lerp (fxvector-ref sector-prev f) (sector-floorheight s) frac))
                (sector-ceilingheight-set! s (lerp (fxvector-ref sector-prev (fx+ f 1))
                                                   (sector-ceilingheight s) frac)))))))))

  ;; Put everything back.
  (define (interp-end!)
    (when active
      (set! active #f)
      (let ([last (fx- gametic 1)])
        (define (fresh v) (and v (fx= (fxvector-ref v 0) last)))
        (each-mobj
         (lambda (mo)
           (let ([v (hashtable-ref saved mo #f)])
             (when (fresh v)
               (mobj-x-set! mo (fxvector-ref v 5))
               (mobj-y-set! mo (fxvector-ref v 6))
               (mobj-z-set! mo (fxvector-ref v 7))
               (mobj-angle-set! mo (fxvector-ref v 8))))))
        (each-player
         (lambda (p)
           (let ([v (hashtable-ref saved p #f)])
             (when (fresh v) (player-viewz-set! p (fxvector-ref v 2))))
           (vector-for-each
            (lambda (psp)
              (let ([v (hashtable-ref saved psp #f)])
                (when (fresh v)
                  (pspdef-sx-set! psp (fxvector-ref v 3))
                  (pspdef-sy-set! psp (fxvector-ref v 4)))))
            (player-psprites p))))
        (when (eq? sector-vec sectors)
          (do ([i 0 (fx+ i 1)]) ((fx= i numsectors))
            (when (fx= (fxvector-ref sector-tic i) last)
              (let ([s (vector-ref sectors i)] [f (fx* 2 i)])
                (sector-floorheight-set! s (fxvector-ref sector-cur f))
                (sector-ceilingheight-set! s (fxvector-ref sector-cur (fx+ f 1))))))))))
)
