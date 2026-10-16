;;; p_mobj: map objects.
;;;
;;; Milestone 2 has only the mobj_t record and enough of P_SpawnMapThing to
;;; record player starts and put a player mobj on the map, so the automap has
;;; something to follow. The real spawning (info tables, P_SpawnMobj,
;;; P_SetThingPosition) arrives with the playsim in milestone 4.
(library (doom p_mobj)
  (export mobj make-mobj mobj?
          mobj-x mobj-x-set! mobj-y mobj-y-set! mobj-z mobj-z-set!
          mobj-snext mobj-snext-set! mobj-sprev mobj-sprev-set!
          mobj-angle mobj-angle-set! mobj-sprite mobj-sprite-set!
          mobj-frame mobj-frame-set! mobj-bnext mobj-bnext-set!
          mobj-bprev mobj-bprev-set! mobj-subsector mobj-subsector-set!
          mobj-floorz mobj-floorz-set! mobj-ceilingz mobj-ceilingz-set!
          mobj-radius mobj-radius-set! mobj-height mobj-height-set!
          mobj-momx mobj-momx-set! mobj-momy mobj-momy-set! mobj-momz mobj-momz-set!
          mobj-validcount mobj-validcount-set! mobj-type mobj-type-set!
          mobj-info mobj-info-set! mobj-tics mobj-tics-set! mobj-state mobj-state-set!
          mobj-flags mobj-flags-set! mobj-health mobj-health-set!
          mobj-movedir mobj-movedir-set! mobj-movecount mobj-movecount-set!
          mobj-target mobj-target-set! mobj-reactiontime mobj-reactiontime-set!
          mobj-threshold mobj-threshold-set! mobj-player mobj-player-set!
          mobj-lastlook mobj-lastlook-set! mobj-spawnpoint mobj-spawnpoint-set!
          mobj-tracer mobj-tracer-set!
          p-spawn-map-thing p-spawn-player)
  (import (chezscheme) (doom struct) (doom r_defs) (doom doomdef) (doom doomdata)
          (doom doomstat) (doom d_player) (doom tables) (doom m_fixed) (doom p_local)
          (doom i_system))

  ;; mobj_t extends degenmobj_t (thinker, x, y, z).
  (define-struct mobj (degenmobj)
    (snext #f) (sprev #f)       ; sector thing list links
    (angle 0)
    (sprite 0) (frame 0)
    (bnext #f) (bprev #f)       ; blockmap links
    (subsector #f)
    (floorz 0) (ceilingz 0)
    (radius 0) (height 0)
    (momx 0) (momy 0) (momz 0)
    (validcount 0)
    (type 0) (info #f)
    (tics 0) (state #f)
    (flags 0) (health 0)
    (movedir 0) (movecount 0)
    (target #f)
    (reactiontime 0) (threshold 0)
    (player #f)
    (lastlook 0)
    (spawnpoint (make-mapthing))
    (tracer #f))

  (define-syntax mobj-x (identifier-syntax degenmobj-x))
  (define-syntax mobj-x-set! (identifier-syntax degenmobj-x-set!))
  (define-syntax mobj-y (identifier-syntax degenmobj-y))
  (define-syntax mobj-y-set! (identifier-syntax degenmobj-y-set!))
  (define-syntax mobj-z (identifier-syntax degenmobj-z))
  (define-syntax mobj-z-set! (identifier-syntax degenmobj-z-set!))

  ;; P_SpawnPlayer (partial): place the player's mobj at the start spot.
  ;; TODO(milestone 4): P_SpawnMobj (MT_PLAYER), P_SetupPsprites, etc.
  (define (p-spawn-player mthing)
    (let ([n (fx- (mapthing-type mthing) 1)])
      (when (vector-ref playeringame n)
        (let ([p (vector-ref players n)]
              [mo (make-mobj)])
          (mobj-x-set! mo (int32 (fxsll (mapthing-x mthing) FRACBITS)))
          (mobj-y-set! mo (int32 (fxsll (mapthing-y mthing) FRACBITS)))
          (mobj-z-set! mo ONFLOORZ)
          ;; ANG45 * (angle/45): C integer division, then unsigned multiply.
          (mobj-angle-set! mo (uint32 (* ANG45 (quotient (mapthing-angle mthing) 45))))
          (mobj-player-set! mo p)
          (mobj-health-set! mo (player-health p))
          (player-mo-set! p mo)))))

  ;; P_SpawnMapThing (partial): deathmatch and player starts only.
  ;; TODO(milestone 4): everything else.
  (define (p-spawn-map-thing mthing)
    (let ([type (mapthing-type mthing)])
      (cond
        [(fx= type 11)
         ;; count deathmatch start positions; extras are dropped
         (when (fx< deathmatch_p MAX_DM_STARTS)
           (vector-set! deathmatchstarts deathmatch_p (copy-mapthing mthing))
           (set! deathmatch_p (fx+ deathmatch_p 1)))]
        [(fx<= type 4)
         ;; save spots for respawning in network games. (C writes
         ;; playerstarts[-1] and worse for type <= 0.)
         (when (fx<= type 0) (i-error "P_SpawnMapThing: bad player start type ~a" type))
         (vector-set! playerstarts (fx- type 1) (copy-mapthing mthing))
         (when (fx= deathmatch 0)
           (p-spawn-player mthing))]
        [else (void)]))))
