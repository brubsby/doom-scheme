;;; m_argv: the command line.
(library (doom m_argv)
  (export myargc myargv m-check-parm m-set-args! m-atoi)
  (import (chezscheme) (doom global))

  ;; myargv is a vector of strings; [0] is the program name.
  (define-global myargv (vector "doom"))
  (define-global myargc 1)

  (define (m-set-args! args)
    (set! myargv (list->vector args))
    (set! myargc (vector-length myargv)))

;; C's atoi: optional white space and sign, then the digits there are
  ;; (0 if none).
  (define (m-atoi s)
    (let* ([n (string-length s)]
           [i (let skip ([i 0])
                (if (and (fx< i n) (memv (string-ref s i) '(#\space #\tab #\newline #\return #\vtab #\page)))
                    (skip (fx+ i 1))
                    i))]
           [neg (and (fx< i n) (char=? (string-ref s i) #\-))]
           [i (if (and (fx< i n) (memv (string-ref s i) '(#\- #\+))) (fx+ i 1) i)])
      (let loop ([i i] [v 0])
        (if (and (fx< i n) (char<=? #\0 (string-ref s i) #\9))
            (loop (fx+ i 1) (+ (* v 10) (fx- (char->integer (string-ref s i)) 48)))
            (if neg (- v) v)))))

  ;; M_CheckParm: checks for the given parameter in the program's command
  ;; line arguments. Returns the argument number (1 to argc-1) or 0 if not
  ;; present.
  (define (m-check-parm check)
    (let loop ([i 1])
      (cond [(fx>= i myargc) 0]
            [(string-ci=? check (vector-ref myargv i)) i]
            [else (loop (fx+ i 1))]))))
