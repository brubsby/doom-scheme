;;; m_fixed: 16.16 fixed-point arithmetic on int32 values.
;;; Fixnums are 61-bit, so every operation that can overflow int32 in C
;;; is explicitly wrapped with int32.
(library (doom m_fixed)
  (export FRACBITS FRACUNIT MAXINT MININT int32 c-abs fixed-mul fixed-div fixed-div2)
  (import (chezscheme) (doom i_system))

  (define FRACBITS 16)
  (define FRACUNIT 65536)
  (define MAXINT #x7fffffff)
  (define MININT (- #x80000000))

  ;; Reduce any exact integer to int32 two's complement, as a C cast does.
  (define (int32 x)
    (let ([u (logand x #xFFFFFFFF)])
      (if (fx>= u #x80000000) (fx- u #x100000000) u)))

  ;; C's abs() on int32: abs(MININT) overflows and stays MININT.
  (define (c-abs x) (if (fx= x MININT) MININT (fxabs x)))

  ;; ((long long)a * b) >> 16, truncated to int32. The 64-bit product can
  ;; exceed fixnum range, so use generic * here.
  (define (fixed-mul a b)
    (int32 (ash (* a b) -16)))

  (define (fixed-div a b)
    (if (fx>= (fxsra (c-abs a) 14) (c-abs b))
        (if (fx< (fxxor a b) 0) MININT MAXINT)
        (fixed-div2 a b)))

  ;; linuxdoom computes (fixed_t)((double)a / (double)b * FRACUNIT). With int32
  ;; inputs that equals exact truncating division: a*2^16 < 2^47 in magnitude,
  ;; so the true quotient is never within 2^-47 (relative) of an integer it
  ;; isn't equal to, and a correctly rounded double (53 bits) cannot cross one.
  ;; tests/fixed.ss checks this against the oracle's own m_fixed.o.
  (define (fixed-div2 a b)
    (when (fx= b 0) (i-error "FixedDiv: divide by zero"))
    (let ([c (quotient (ash a FRACBITS) b)])
      (when (or (>= c #x80000000) (< c (- #x80000000)))
        (i-error "FixedDiv: divide by zero"))
      c)))
