;;; Generated tables and SlopeDiv against the oracle's compiled ones
;;; (build/oracle/tables.txt, lines "name index value").
(import (chezscheme) (doom tables) (doom m_random) (doom m_random_data)
        (doom v_video))
(load "tests/lib.ss")

(define bad (make-eq-hashtable))
(define count (make-eq-hashtable))
(define (tally name ok)
  (hashtable-update! count name (lambda (n) (+ n 1)) 0)
  (unless ok (hashtable-update! bad name (lambda (n) (+ n 1)) 0)))

(let ([p (open-input-file "build/oracle/tables.txt")])
  (let loop ()
    (let ([line (get-line p)])
      (unless (eof-object? line)
        (let* ([f (with-input-from-string (string-append "(" line ")") read)]
               [name (car f)] [i (cadr f)] [v (caddr f)])
          (tally name
                 (case name
                   [(finesine) (= (fxvector-ref finesine i) v)]
                   [(finetangent) (= (fxvector-ref finetangent i) v)]
                   [(tantoangle) (= (fxvector-ref tantoangle i) v)]
                   [(rndtable) (= (bytevector-u8-ref rndtable i) v)]
                   [(gammatable) (= (bytevector-u8-ref gammatable i) v)]
                   ;; "slopediv num den result"
                   [(slopediv) (= (slope-div i v) (cadddr f))]
                   [else #f])))
        (loop)))))

(vector-for-each
 (lambda (name)
   (check (format "~a (~a values)" name (hashtable-ref count name 0))
          0 (hashtable-ref bad name 0)))
 (vector-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
              (hashtable-keys count)))

(check "finecosine aliases finesine+2048" (fxvector-ref finesine 2048) (finecosine 0))
(check "angle+ wraps" 0 (angle+ ANG270 ANG90))
(check "angle- wraps" ANG270 (angle- 0 ANG90))

(m-clear-random)
(check "P_Random sequence" '(8 109 220) (list (p-random) (p-random) (p-random)))
(check "M_Random is independent" 8 (m-random))
(check "prndindex after 3 calls" 3 prndindex)
(finish)
