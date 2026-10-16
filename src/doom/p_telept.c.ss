;;; p_telept.c: teleportation. Included by p_local.sls.

;; EV_Teleport
(define (ev-teleport line side thing)
  (cond
    ;; don't teleport missiles
    [(fxlogtest (mobj-flags thing) MF_MISSILE) 0]
    ;; Don't teleport if hit back of line, so you can get out of
    ;; teleporter.
    [(fx= side 1) 0]
    [else
     (let ([tag (line-tag line)])
       (let sector-loop ([i 0])
         (cond
           [(fx= i numsectors) 0]
           [(not (fx= (sector-tag (vector-ref sectors i)) tag)) (sector-loop (fx+ i 1))]
           [else
            (let thinker-loop ([th (thinker-next thinkercap)])
              (cond
                [(eq? th thinkercap) (sector-loop (fx+ i 1))]
                ;; not a mobj, not a teleportman, or wrong sector
                [(or (not (eq? (thinker-func th) p-mobj-thinker))
                     (not (fx= (mobj-type th) MT_TELEPORTMAN))
                     (not (fx= (sector-index (subsector-sector (mobj-subsector th))) i)))
                 (thinker-loop (thinker-next th))]
                [else
                 (let ([m th] [oldx (mobj-x thing)] [oldy (mobj-y thing)] [oldz (mobj-z thing)])
                   (if (not (p-teleport-move thing (mobj-x m) (mobj-y m)))
                       0
                       (begin
                         (mobj-z-set! thing (mobj-floorz thing)) ; fixme: not needed?
                         (when (mobj-player thing)
                           (player-viewz-set! (mobj-player thing)
                                              (i+ (mobj-z thing) (player-viewheight (mobj-player thing)))))
                         ;; spawn teleport fog at source and destination
                         (s-start-sound (p-spawn-mobj oldx oldy oldz MT_TFOG) sfx_telept)
                         (let ([an (fxsrl (mobj-angle m) ANGLETOFINESHIFT)])
                           ;; emit sound, where?
                           (s-start-sound
                            (p-spawn-mobj (i+ (mobj-x m) (int32 (fx* 20 (finecosine an))))
                                          (i+ (mobj-y m) (int32 (fx* 20 (fxvector-ref finesine an))))
                                          (mobj-z thing) MT_TFOG)
                            sfx_telept))
                         ;; don't move for a bit
                         (when (mobj-player thing)
                           (mobj-reactiontime-set! thing 18))
                         (mobj-angle-set! thing (mobj-angle m))
                         (mobj-momx-set! thing 0)
                         (mobj-momy-set! thing 0)
                         (mobj-momz-set! thing 0)
                         1)))]))])))]))
