;;; i_system: fatal errors and timing.
(library (doom i_system)
  (export i-error &doom-error doom-error? i-get-time)
  (import (chezscheme))

  ;; I_Error: vanilla prints the message and exits. We raise a condition so
  ;; tests can catch it; the top level reports it as "Error: ..." and exits 255.
  (define-condition-type &doom-error &error make-doom-error doom-error?)

  (define (i-error fmt . args)
    (raise (condition (make-doom-error)
                      (make-message-condition (apply format fmt args)))))

  ;; I_GetTime: tics (1/35 s) since first call.
  (define basetime #f)
  (define (i-get-time)
    (let* ([t (current-time 'time-monotonic)]
           [ms (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))])
      (unless basetime (set! basetime ms))
      (quotient (* (- ms basetime) 35) 1000))))
