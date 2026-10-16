;;; tables: trig lookup tables and angle_t (unsigned 32-bit binary angles).
(library (doom tables)
  (export FINEANGLES FINEMASK ANGLETOFINESHIFT
          ANG45 ANG90 ANG180 ANG270
          SLOPERANGE SLOPEBITS DBITS
          finesine finetangent tantoangle finecosine
          angle+ angle- uint32 slope-div)
  (import (chezscheme) (doom m_fixed) (doom tables_data))

  (define FINEANGLES 8192)
  (define FINEMASK (- FINEANGLES 1))
  (define ANGLETOFINESHIFT 19)

  (define ANG45  #x20000000)
  (define ANG90  #x40000000)
  (define ANG180 #x80000000)
  (define ANG270 #xc0000000)

  (define SLOPERANGE 2048)
  (define SLOPEBITS 11)
  (define DBITS (- FRACBITS SLOPEBITS))

  ;; C: fixed_t* finecosine = &finesine[FINEANGLES/4];
  (define-syntax finecosine
    (syntax-rules ()
      [(_ i) (fxvector-ref finesine (fx+ i 2048))]))

  ;; angle_t arithmetic wraps mod 2^32.
  (define (uint32 x) (logand x #xFFFFFFFF))
  (define-syntax angle+
    (syntax-rules () [(_ a b) (fxand (fx+ a b) #xFFFFFFFF)]))
  (define-syntax angle-
    (syntax-rules () [(_ a b) (fxand (fx- a b) #xFFFFFFFF)]))

  ;; SlopeDiv: num and den are unsigned; num<<3 wraps mod 2^32.
  (define (slope-div num den)
    (let ([num (uint32 num)] [den (uint32 den)])
      (if (fx< den 512)
          SLOPERANGE
          (let ([ans (fxquotient (fxand (fxsll num 3) #xFFFFFFFF) (fxsrl den 8))])
            (if (fx<= ans SLOPERANGE) ans SLOPERANGE))))))
