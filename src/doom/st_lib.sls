;;; st_lib: the status bar widget library: numbers, percents, multi-state
;;; and binary icons, each redrawn from the background screen (BG) onto
;;; the foreground (FG).
;;;
;;; C's widget fields point at the values they show (int* num, boolean* on);
;;; here they are thunks returning the current value.
(library (doom st_lib)
  (export BG FG ST_HEIGHT ST_WIDTH ST_Y stlib-init
          st-number make-st-number st-number-x st-number-y st-number-width st-number-oldnum
          st-number-num st-number-num-set! st-number-on st-number-data st-number-data-set!
          stlib-init-num stlib-update-num
          st-percent make-st-percent st-percent-n stlib-init-percent stlib-update-percent
          st-multicon make-st-multicon st-multicon-oldinum st-multicon-oldinum-set! stlib-init-mult-icon
          stlib-update-mult-icon
          st-binicon make-st-binicon stlib-init-bin-icon stlib-update-bin-icon)
  (import (chezscheme) (doom struct) (doom doomdef) (doom v_video) (doom w_wad) (doom i_system))

  (define BG 4)
  (define FG 0)
  ;; (st_stuff.h)
  (define ST_HEIGHT 32)
  (define ST_WIDTH SCREENWIDTH)
  (define ST_Y (fx- SCREENHEIGHT ST_HEIGHT))

  ;; Hack display negative frags. Loads and store the stminus lump.
  (define sttminus #f)
  (define (stlib-init)
    (set! sttminus (w-cache-lump-name "STTMINUS")))

  ;; st_number_t: upper right-hand corner of the number (right-justified),
  ;; max # of digits, last value, current value, whether to update, and
  ;; patches for 0-9.
  (define-struct st-number () (x 0) (y 0) (width 0) (oldnum 0) (num #f) (on #f) (p #f) (data 0))
  (define-struct st-percent () (n (make-st-number)) (p #f))
  ;; center-justified location of icons, last icon number, current icon,
  ;; whether to update, list of icons
  (define-struct st-multicon () (x 0) (y 0) (oldinum 0) (inum #f) (on #f) (p #f) (data 0))
  (define-struct st-binicon () (x 0) (y 0) (oldval #f) (val #f) (on #f) (p #f) (data 0))

  (define (stlib-init-num n x y pl num on width)
    (st-number-x-set! n x)
    (st-number-y-set! n y)
    (st-number-oldnum-set! n 0)
    (st-number-width-set! n width)
    (st-number-num-set! n num)
    (st-number-on-set! n on)
    (st-number-p-set! n pl))

  ;; A fairly efficient way to draw a number based on differences from the
  ;; old number. Note: worth the trouble?
  (define (stlib-draw-num n refresh)
    (let* ([numdigits (st-number-width n)]
           [num ((st-number-num n))]
           [p0 (vector-ref (st-number-p n) 0)]
           [w (patch-width p0)]
           [h (patch-height p0)]
           [neg (fx< num 0)])
      (st-number-oldnum-set! n num)
      (let ([num (if neg
                     (fx- 0 (cond [(and (fx= numdigits 2) (fx< num -9)) -9]
                                  [(and (fx= numdigits 3) (fx< num -99)) -99]
                                  [else num]))
                     num)])
        ;; clear the area
        (let ([x (fx- (st-number-x n) (fx* numdigits w))])
          (when (fx< (fx- (st-number-y n) ST_Y) 0)
            (i-error "drawNum: n->y - ST_Y < 0"))
          (v-copy-rect x (fx- (st-number-y n) ST_Y) BG (fx* w numdigits) h x (st-number-y n) FG))
        ;; if non-number, do not draw it
        (unless (fx= num 1994)
          (let ([x (st-number-x n)])
            ;; in the special case of 0, you draw 0
            (when (fx= num 0)
              (v-draw-patch (fx- x w) (st-number-y n) FG p0))
            ;; draw the new number
            (let loop ([num num] [numdigits numdigits] [x x])
              (if (and (not (fx= num 0)) (fx> numdigits 0))
                  (let ([x (fx- x w)])
                    (v-draw-patch x (st-number-y n) FG
                                  (vector-ref (st-number-p n) (fxremainder num 10)))
                    (loop (fxquotient num 10) (fx- numdigits 1) x))
                  ;; draw a minus sign if necessary
                  (when neg
                    (v-draw-patch (fx- x 8) (st-number-y n) FG sttminus)))))))))

  (define (stlib-update-num n refresh)
    (when ((st-number-on n))
      (stlib-draw-num n refresh)))

  (define (stlib-init-percent p x y pl num on percent)
    (stlib-init-num (st-percent-n p) x y pl num on 3)
    (st-percent-p-set! p percent))

  (define (stlib-update-percent per refresh)
    (let ([n (st-percent-n per)])
      (when (and refresh ((st-number-on n)))
        (v-draw-patch (st-number-x n) (st-number-y n) FG (st-percent-p per)))
      (stlib-update-num n refresh)))

  (define (stlib-init-mult-icon i x y il inum on)
    (st-multicon-x-set! i x)
    (st-multicon-y-set! i y)
    (st-multicon-oldinum-set! i -1)
    (st-multicon-inum-set! i inum)
    (st-multicon-on-set! i on)
    (st-multicon-p-set! i il))

  (define (stlib-update-mult-icon mi refresh)
    (let ([inum ((st-multicon-inum mi))]
          [oldinum (st-multicon-oldinum mi)])
      (when (and ((st-multicon-on mi))
                 (or (not (fx= oldinum inum)) refresh)
                 (not (fx= inum -1)))
        (unless (fx= oldinum -1)
          (let* ([op (vector-ref (st-multicon-p mi) oldinum)]
                 [x (fx- (st-multicon-x mi) (patch-leftoffset op))]
                 [y (fx- (st-multicon-y mi) (patch-topoffset op))])
            (when (fx< (fx- y ST_Y) 0)
              (i-error "updateMultIcon: y - ST_Y < 0"))
            (v-copy-rect x (fx- y ST_Y) BG (patch-width op) (patch-height op) x y FG)))
        (v-draw-patch (st-multicon-x mi) (st-multicon-y mi) FG (vector-ref (st-multicon-p mi) inum))
        (st-multicon-oldinum-set! mi inum))))

  (define (stlib-init-bin-icon b x y i val on)
    (st-binicon-x-set! b x)
    (st-binicon-y-set! b y)
    (st-binicon-oldval-set! b #f)
    (st-binicon-val-set! b val)
    (st-binicon-on-set! b on)
    (st-binicon-p-set! b i))

  (define (stlib-update-bin-icon bi refresh)
    (let ([val ((st-binicon-val bi))])
      (when (and ((st-binicon-on bi))
                 (or (not (eq? (st-binicon-oldval bi) val)) refresh))
        (let* ([p (st-binicon-p bi)]
               [x (fx- (st-binicon-x bi) (patch-leftoffset p))]
               [y (fx- (st-binicon-y bi) (patch-topoffset p))])
          (when (fx< (fx- y ST_Y) 0)
            (i-error "updateBinIcon: y - ST_Y < 0"))
          (if val
              (v-draw-patch (st-binicon-x bi) (st-binicon-y bi) FG p)
              (v-copy-rect x (fx- y ST_Y) BG (patch-width p) (patch-height p) x y FG))
          (st-binicon-oldval-set! bi val))))))
