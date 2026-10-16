;;; m_fixed against the oracle's m_fixed.o (build/oracle/fixed-cases.txt,
;;; lines "a b FixedMul FixedDiv [E]"; E = FixedDiv hit I_Error).
(import (chezscheme) (doom m_fixed) (doom i_system))
(load "tests/lib.ss")

(define (div-or-error a b)
  (guard (e [(doom-error? e) 'error]) (fixed-div a b)))

(let ([p (open-input-file "build/oracle/fixed-cases.txt")])
  (let loop ([n 0] [mul-bad 0] [div-bad 0] [first-bad #f])
    (let ([line (get-line p)])
      (if (eof-object? line)
          (begin
            (check (format "FixedMul matches C on ~a cases" n) 0 mul-bad)
            (check (format "FixedDiv matches C on ~a cases" n) 0 div-bad)
            (when first-bad (printf "     first mismatch: ~a\n" first-bad)))
          (let* ([f (with-input-from-string (string-append "(" line ")") read)]
                 [a (car f)] [b (cadr f)] [mul (caddr f)] [div (cadddr f)]
                 [err? (= (length f) 5)]
                 [mul-ok (= (fixed-mul a b) mul)]
                 [div-ok (or (= b 0)
                             (if err? (eq? (div-or-error a b) 'error)
                                 (eqv? (div-or-error a b) div)))])
            (loop (+ n 1)
                  (if mul-ok mul-bad (+ mul-bad 1))
                  (if div-ok div-bad (+ div-bad 1))
                  (or first-bad (and (not (and mul-ok div-ok)) line))))))))

(check "int32 wraps" MININT (int32 #x80000000))
(check "int32 of a 64-bit value" -1 (int32 #xFFFFFFFFFFFF))
(finish)
