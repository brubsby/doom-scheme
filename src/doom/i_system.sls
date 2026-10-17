;;; i_system: fatal errors, quitting, and timing.
(library (doom i_system)
  (export i-error &doom-error doom-error? i-quit &doom-quit doom-quit?
          i-get-time i-fake-time i-wait-vbl i-wait-hook)
  (import (chezscheme) (doom global))

  ;; I_Error: vanilla prints the message and exits. We raise a condition so
  ;; tests can catch it; the top level reports it as "Error: ..." and exits 255.
  (define-condition-type &doom-error &error make-doom-error doom-error?)

  (define (i-error fmt . args)
    (raise (condition (make-doom-error)
                      (make-message-condition (apply format fmt args)))))

  ;; I_Quit: vanilla shuts down and exits 0 (after saving the config). We
  ;; raise &doom-quit; the top level does the shutdown.
  (define-condition-type &doom-quit &condition make-doom-quit doom-quit?)
  (define (i-quit)
    (raise (make-doom-quit)))

  ;; When true, I_GetTime advances one tic per call instead of following the
  ;; clock: runs are repeatable and as fast as the machine allows. (The
  ;; oracle has the same switch, ORACLE_FAKETIME.)
  (define-global i-fake-time #f)
  (define faketime 0)

  ;; I_GetTime: tics (1/35 s) since first call.
  (define basetime #f)
  (define (i-get-time)
    (if i-fake-time
        (begin (set! faketime (fx+ faketime 1))
               (fx- faketime 1))
        (let* ([t (current-time 'time-monotonic)]
               [ms (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))])
          (unless basetime (set! basetime ms))
          (quotient (* (- ms basetime) 35) 1000))))

  ;; I_WaitVBL: sleep count 70ths of a second (not under fake time),
  ;; calling i-wait-hook (if set) every 70th meanwhile: linuxdoom's sound
  ;; server, a separate process, kept playing while the game slept (the
  ;; quit sound), and here i_sound keeps the device fed.
  (define-global i-wait-hook #f)
  (define (i-wait-vbl count)
    (unless i-fake-time
      (do ([i 0 (fx+ i 1)]) ((fx= i count))
        (when i-wait-hook (i-wait-hook))
        (sleep (make-time 'time-duration (quotient 1000000000 70) 0))))))
