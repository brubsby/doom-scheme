;;; Stand-ins for functions the playsim reaches that aren't ported yet.
;;; Included by p_local.sls.
;;;
;;; A stand-in that would change the simulation logs the call: tests
;;; compare against the oracle only up to the first such call (see
;;; tests/sim.ss). g_game logs its unported game actions here too.

;; Names of logged functions, in first-call order, and the gametic of the
;; first call (#f if none yet).
(define-global unported-calls '())
(define-global unported-first-tic #f)

(define (unported name)
  (unless unported-first-tic
    (set! unported-first-tic gametic))
  (unless (memq name unported-calls)
    (set! unported-calls (append unported-calls (list name)))))

;; Action functions without a port (none in DOOM's info.c, but a state
;; table from elsewhere could name one).
(define (unported-action name)
  (lambda args (unported name)))

;; AM_Stop, for P_KillMobj (the automap is above the playsim; am_map sets
;; this when it loads).
(define-global am-stop-hook (lambda () (void)))

;; ST_Start and HU_Start, for P_SpawnPlayer (the front end is above the
;; playsim; d_main sets these when it loads).
(define-global st-start-hook (lambda () (void)))
(define-global hu-start-hook (lambda () (void)))
