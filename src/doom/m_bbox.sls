;;; m_bbox: bounding boxes as 4-element fxvectors indexed by BOXTOP etc.
(library (doom m_bbox)
  (export BOXTOP BOXBOTTOM BOXLEFT BOXRIGHT make-box m-clear-box m-add-to-box)
  (import (chezscheme) (doom m_fixed))

  (define BOXTOP 0)
  (define BOXBOTTOM 1)
  (define BOXLEFT 2)
  (define BOXRIGHT 3)

  (define (make-box) (make-fxvector 4 0))

  (define (m-clear-box box)
    (fxvector-set! box BOXTOP MININT)
    (fxvector-set! box BOXRIGHT MININT)
    (fxvector-set! box BOXBOTTOM MAXINT)
    (fxvector-set! box BOXLEFT MAXINT))

  ;; Note the else-ifs (as in C): on a freshly cleared box the first point
  ;; only sets LEFT and BOTTOM.
  (define (m-add-to-box box x y)
    (cond
      [(fx< x (fxvector-ref box BOXLEFT)) (fxvector-set! box BOXLEFT x)]
      [(fx> x (fxvector-ref box BOXRIGHT)) (fxvector-set! box BOXRIGHT x)])
    (cond
      [(fx< y (fxvector-ref box BOXBOTTOM)) (fxvector-set! box BOXBOTTOM y)]
      [(fx> y (fxvector-ref box BOXTOP)) (fxvector-set! box BOXTOP y)])))
