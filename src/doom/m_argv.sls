;;; m_argv: the command line.
(library (doom m_argv)
  (export myargc myargv m-check-parm m-set-args!)
  (import (chezscheme) (doom global))

  ;; myargv is a vector of strings; [0] is the program name.
  (define-global myargv (vector "doom"))
  (define-global myargc 1)

  (define (m-set-args! args)
    (set! myargv (list->vector args))
    (set! myargc (vector-length myargv)))

  ;; M_CheckParm: checks for the given parameter in the program's command
  ;; line arguments. Returns the argument number (1 to argc-1) or 0 if not
  ;; present.
  (define (m-check-parm check)
    (let loop ([i 1])
      (cond [(fx>= i myargc) 0]
            [(string-ci=? check (vector-ref myargv i)) i]
            [else (loop (fx+ i 1))]))))
