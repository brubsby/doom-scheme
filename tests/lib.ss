;;; Minimal test harness shared by tests/*.ss.

;; The port reads and writes $HOME/.doomrc: tests get their own HOME.
(unless (file-exists? "build/test/home") (system "mkdir -p build/test/home"))
(putenv "HOME" (string-append (current-directory) "/build/test/home"))
(define failures 0)
(define-syntax check
  (syntax-rules ()
    [(_ name expected actual)
     (let ([e expected] [a actual])
       (if (equal? e a)
           (printf "ok   ~a\n" name)
           (begin (set! failures (+ failures 1))
                  (printf "FAIL ~a\n     expected ~s\n     actual   ~s\n" name e a))))]))
(define (finish)
  (if (= failures 0)
      (printf "all passed\n")
      (begin (printf "~a failed\n" failures) (exit 1))))
(define wad-path
  (or (getenv "DOOMWAD") "wads/doom1.wad"))
