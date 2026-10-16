;;; p_tick.c: the thinker list. Included by p_local.sls.
;;; TODO(milestone 4): P_RunThinkers, P_Ticker.

;; Both the head and tail of the thinker list.
(define thinkercap (make-thinker))

;; P_InitThinkers
(define (p-init-thinkers)
  (thinker-prev-set! thinkercap thinkercap)
  (thinker-next-set! thinkercap thinkercap))

;; P_AddThinker: adds a new thinker at the end of the list.
(define (p-add-thinker thinker)
  (thinker-next-set! (thinker-prev thinkercap) thinker)
  (thinker-next-set! thinker thinkercap)
  (thinker-prev-set! thinker (thinker-prev thinkercap))
  (thinker-prev-set! thinkercap thinker))

;; P_RemoveThinker: deallocation is lazy -- it will not actually be freed
;; until its thinking turn comes up.
(define (p-remove-thinker thinker)
  (thinker-func-set! thinker 'removed))
