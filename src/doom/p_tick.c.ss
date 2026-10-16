;;; p_tick.c: the thinker list and the per-tic playsim driver. Included by
;;; p_local.sls.

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

;; P_RunThinkers
(define (p-run-thinkers)
  (let loop ([current (thinker-next thinkercap)])
    (unless (eq? current thinkercap)
      (let ([func (thinker-func current)])
        (cond
          [(eq? func 'removed)
           ;; time to remove it
           (thinker-prev-set! (thinker-next current) (thinker-prev current))
           (thinker-next-set! (thinker-prev current) (thinker-next current))]
          [func (func current)])
        ;; (C reads current->next after the thinker ran, or after unlinking
        ;; it; the unlinked thinker's own next pointer is still valid)
        (loop (thinker-next current))))))

;; P_Ticker
(define (p-ticker)
  ;; run the tic
  (unless (or paused
              ;; pause if in menu and at least one tic has been run
              (and (not netgame) menuactive (not demoplayback)
                   (not (fx= (player-viewz (vector-ref players consoleplayer)) 1))))
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (p-player-think (vector-ref players i))))
    (p-run-thinkers)
    (p-update-specials)
    (p-respawn-specials)
    ;; for par times
    (set! leveltime (fx+ leveltime 1))))
