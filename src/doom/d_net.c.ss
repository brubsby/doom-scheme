;;; d_net.c: tic timing and command distribution, for a single node.
;;; Included by d_main.sls.
;;;
;;; Netgames are not supported. With one node, C's NetUpdate builds the
;;; console player's commands and "sends" them to itself (the rebound
;;; packet), which lands them in netcmds and advances nettics[0] to maketic;
;;; here G_BuildTiccmd writes to netcmds directly.

(define BACKUPTICS 12)

;; netcmds[player][tic % BACKUPTICS]
(define netcmds
  (let ([v (make-vector MAXPLAYERS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS) v)
      (vector-set! v i (let ([r (make-vector BACKUPTICS)])
                         (do ([j 0 (fx+ j 1)]) ((fx= j BACKUPTICS) r)
                           (vector-set! r j (make-ticcmd))))))))

;; C's localcmds: where NetUpdate builds commands under -timedemo (the
;; singletics path builds into netcmds itself and never sends these).
(define localcmds
  (let ([r (make-vector BACKUPTICS)])
    (do ([j 0 (fx+ j 1)]) ((fx= j BACKUPTICS) r)
      (vector-set! r j (make-ticcmd)))))

(define nettics0 0)
(define gametime 0)
(define skiptics 0)

;; Test hook: if set, called with no arguments after every gametic.

;; NetUpdate: builds ticcmds for console player, sends out a packet.
(define (net-update)
  ;; check time
  (let* ([nowtime (fxquotient (i-get-time) ticdup)]
         [newtics (fx- nowtime gametime)])
    (set! gametime nowtime)
    (when (fx> newtics 0)               ; (else nothing new to update)
      (let ([newtics (if (fx<= skiptics newtics)
                         (let ([n (fx- newtics skiptics)]) (set! skiptics 0) n)
                         (begin (set! skiptics (fx- skiptics newtics)) 0))]
            [gameticdiv (fxquotient gametic ticdup)])
        ;; build new ticcmds for console player
        (let loop ([i 0])
          (when (fx< i newtics)
            (i-start-tic d-post-event)
            (d-process-events)
            (unless (fx>= (fx- maketic gameticdiv) (fx- (fxquotient BACKUPTICS 2) 1)) ; can't hold any more
              (g-build-ticcmd (if singletics
                                  (vector-ref localcmds (fxremainder maketic BACKUPTICS))
                                  (vector-ref (vector-ref netcmds consoleplayer)
                                              (fxremainder maketic BACKUPTICS))))
              (set! maketic (fx+ maketic 1))
              (loop (fx+ i 1)))))
        ;; singletic update is syncronous; otherwise the packet rebounds to
        ;; ourselves (GetPackets).
        (unless singletics
          (set! nettics0 maketic))))))

;; D_CheckNetGame: works out player numbers among the net participants.
;; Here: a single player game.
(define (d-check-net-game)
  (set! netgame #f)
  (set! consoleplayer 0)
  (set! displayplayer 0)
  (set! ticdup 1)
  (vector-set! playeringame 0 #t))

;; TryRunTics
(define oldentertics 0)

(define (run-one-tic)
  (when advancedemo
    (d-do-advance-demo))
  (m-ticker)
  (g-ticker)
  (set! gametic (fx+ gametic 1))
  (when d-tic-hook (d-tic-hook)))

(define (try-run-tics)
  ;; get real tics
  (let* ([entertic (fxquotient (i-get-time) ticdup)]
         [realtics (fx- entertic oldentertics)])
    (set! oldentertics entertic)
    ;; get available tics
    (net-update)
    (let* ([lowtic nettics0]
           [availabletics (fx- lowtic (fxquotient gametic ticdup))]
           ;; decide how many tics to run
           [counts (cond [(fx< realtics (fx- availabletics 1)) (fx+ realtics 1)]
                         [(fx< realtics availabletics) realtics]
                         [else availabletics])]
           [counts (if (fx< counts 1) 1 counts)])
      ;; (the key player does not adapt)
      ;; wait for new tics if needed
      (let wait ([lowtic lowtic])
        (if (fx< lowtic (fx+ (fxquotient gametic ticdup) counts))
            (begin
              (net-update)
              (let ([lowtic nettics0])
                (when (fx< lowtic (fxquotient gametic ticdup))
                  (i-error "TryRunTics: lowtic < gametic"))
                ;; don't stay in here forever -- give the menu a chance to
                ;; work
                (if (fx>= (fx- (fxquotient (i-get-time) ticdup) entertic) 20)
                    (m-ticker)
                    (wait lowtic))))
            ;; run the count * ticdup dics
            (do ([c counts (fx- c 1)]) ((fx= c 0))
              (do ([i 0 (fx+ i 1)]) ((fx= i ticdup))
                (when (fx> (fxquotient gametic ticdup) lowtic)
                  (i-error "gametic>lowtic"))
                (run-one-tic)
                ;; (modify command for duplicated tics: ticdup is 1)
                )
              (net-update)))))))        ; check for new console commands
