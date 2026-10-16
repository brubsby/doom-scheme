;;; m_cheat: scrambled cheat sequence matching.
(library (doom m_cheat)
  (export make-cheatseq cheatseq? cht-check-cheat cht-get-param)
  (import (chezscheme))

  (define (scramble a)
    (fx+ (fxsll (fxand a 1) 7) (fxsll (fxand a 2) 5) (fxand a 4) (fxsll (fxand a 8) 1)
         (fxsrl (fxand a 16) 1) (fxand a 32) (fxsrl (fxand a 64) 5) (fxsrl (fxand a 128) 7)))

  (define cheat-xlate-table
    (let ([t (make-bytevector 256)])
      (do ([i 0 (fx+ i 1)]) ((fx= i 256) t)
        (bytevector-u8-set! t i (scramble i)))))

  ;; cheatseq_t: `sequence` is a bytevector of scrambled bytes, where 1 marks
  ;; the start of a parameter, 0 a parameter slot (overwritten with the typed
  ;; key), and #xff the end. `p` is the match position (C: a pointer, NULL
  ;; before first use).
  (define-record-type (cheatseq make-cheatseq* cheatseq?)
    (fields (immutable sequence) (mutable p)))
  (define (make-cheatseq bytes) (make-cheatseq* (u8-list->bytevector bytes) #f))

  ;; Returns #t when the sequence completes. key is a C char, so only its
  ;; low byte matters.
  (define (cht-check-cheat cht key)
    (let ([seq (cheatseq-sequence cht)]
          [key (fxand key 255)])
      (unless (cheatseq-p cht) (cheatseq-p-set! cht 0))
      (let ([p (cheatseq-p cht)])
        (cond
          [(fx= (bytevector-u8-ref seq p) 0)
           (bytevector-u8-set! seq p key)
           (cheatseq-p-set! cht (fx+ p 1))]
          [(fx= (bytevector-u8-ref cheat-xlate-table key) (bytevector-u8-ref seq p))
           (cheatseq-p-set! cht (fx+ p 1))]
          [else (cheatseq-p-set! cht 0)]))
      (let ([c (bytevector-u8-ref seq (cheatseq-p cht))])
        (cond
          [(fx= c 1) (cheatseq-p-set! cht (fx+ (cheatseq-p cht) 1)) #f]
          [(fx= c #xff) (cheatseq-p-set! cht 0) #t]
          [else #f]))))

  ;; cht_GetParam: the parameter bytes typed into the sequence (clearing the
  ;; slots again), as a list of byte values.
  (define (cht-get-param cht)
    (let ([seq (cheatseq-sequence cht)])
      (let skip ([p 0])
        (if (fx= (bytevector-u8-ref seq p) 1)
            (let loop ([p (fx+ p 1)] [acc '()])
              (let ([c (bytevector-u8-ref seq p)])
                (bytevector-u8-set! seq p 0)
                (if (and (not (fx= c 0)) (not (fx= (bytevector-u8-ref seq (fx+ p 1)) #xff)))
                    (loop (fx+ p 1) (cons c acc))
                    (reverse (cons c acc)))))
            (skip (fx+ p 1)))))))
