;;; p_mobj.c: map objects. Included by p_local.sls.
;;;
;;; Status: spawning (P_SpawnMobj, P_SpawnPlayer, P_SpawnMapThing) is
;;; complete except for the weapon sprites (P_SetupPsprites, milestone 5)
;;; and the status bar / HUD wake-up (milestone 7). Things don't think yet.

;; P_MobjThinker. Its identity marks a thinker as an mobj (C compares the
;; function pointer).
;; TODO(milestone 4): movement and state ticking.
(define (p-mobj-thinker mobj) (void))

;; P_SpawnMobj
(define (p-spawn-mobj x y z type)
  (let ([mobj (make-mobj)]
        [info (vector-ref mobjinfo-table type)])
    (mobj-type-set! mobj type)
    (mobj-info-set! mobj info)
    (mobj-x-set! mobj x)
    (mobj-y-set! mobj y)
    (mobj-radius-set! mobj (mobjinfo-radius info))
    (mobj-height-set! mobj (mobjinfo-height info))
    (mobj-flags-set! mobj (mobjinfo-flags info))
    (mobj-health-set! mobj (mobjinfo-spawnhealth info))
    (unless (fx= gameskill sk_nightmare)
      (mobj-reactiontime-set! mobj (mobjinfo-reactiontime info)))
    (mobj-lastlook-set! mobj (fxremainder (p-random) MAXPLAYERS))
    ;; do not set the state with P_SetMobjState, because action routines
    ;; can not be called yet
    (let ([st (vector-ref states (mobjinfo-spawnstate info))])
      (mobj-state-set! mobj st)
      (mobj-tics-set! mobj (state-tics st))
      (mobj-sprite-set! mobj (state-sprite st))
      (mobj-frame-set! mobj (state-frame st)))
    ;; set subsector and/or block links
    (p-set-thing-position mobj)
    (let ([sec (subsector-sector (mobj-subsector mobj))])
      (mobj-floorz-set! mobj (sector-floorheight sec))
      (mobj-ceilingz-set! mobj (sector-ceilingheight sec)))
    (mobj-z-set! mobj (cond [(fx= z ONFLOORZ) (mobj-floorz mobj)]
                            [(fx= z ONCEILINGZ) (i- (mobj-ceilingz mobj) (mobjinfo-height info))]
                            [else z]))
    (thinker-func-set! mobj p-mobj-thinker)
    (p-add-thinker mobj)
    mobj))

;; mobj->angle = ANG45 * (mthing->angle/45): C integer division, then an
;; unsigned multiply.
(define (mapthing-mobj-angle mthing)
  (uint32 (* ANG45 (quotient (mapthing-angle mthing) 45))))

;; G_PlayerReborn (g_game.c). Here because the playsim calls it and g_game
;; is above the playsim. Resets the player, keeping frags and counts.
(define (g-player-reborn n)
  (let* ([p (vector-ref players n)]
         [frags (player-frags p)]
         [killcount (player-killcount p)]
         [itemcount (player-itemcount p)]
         [secretcount (player-secretcount p)])
    (struct-copy! p (make-player))
    (player-frags-set! p frags)
    (player-killcount-set! p killcount)
    (player-itemcount-set! p itemcount)
    (player-secretcount-set! p secretcount)
    ;; don't do anything immediately
    (player-usedown-set! p #t)
    (player-attackdown-set! p #t)
    (player-playerstate-set! p PST_LIVE)
    (player-health-set! p MAXHEALTH)
    (player-readyweapon-set! p wp_pistol)
    (player-pendingweapon-set! p wp_pistol)
    (vector-set! (player-weaponowned p) wp_fist #t)
    (vector-set! (player-weaponowned p) wp_pistol #t)
    (fxvector-set! (player-ammo p) am_clip 50)
    (do ([i 0 (fx+ i 1)]) ((fx= i NUMAMMO))
      (fxvector-set! (player-maxammo p) i (fxvector-ref maxammo i)))))

;; P_SpawnPlayer: called when a player is spawned on the level. Most of the
;; player structure stays unchanged between levels.
(define (p-spawn-player mthing)
  (let ([n (fx- (mapthing-type mthing) 1)])
    ;; not playing?
    (when (vector-ref playeringame n)
      (let ([p (vector-ref players n)])
        (when (fx= (player-playerstate p) PST_REBORN)
          (g-player-reborn n))
        (let ([mobj (p-spawn-mobj (int32 (fxsll (mapthing-x mthing) FRACBITS))
                                  (int32 (fxsll (mapthing-y mthing) FRACBITS))
                                  ONFLOORZ MT_PLAYER)])
          ;; set color translations for player sprites
          (when (fx> (mapthing-type mthing) 1)
            (mobj-flags-set! mobj (fxior (mobj-flags mobj)
                                         (fxsll (fx- (mapthing-type mthing) 1) MF_TRANSSHIFT))))
          (mobj-angle-set! mobj (mapthing-mobj-angle mthing))
          (mobj-player-set! mobj p)
          (mobj-health-set! mobj (player-health p))
          (player-mo-set! p mobj)
          (player-playerstate-set! p PST_LIVE)
          (player-refire-set! p 0)
          (player-message-set! p #f)
          (player-damagecount-set! p 0)
          (player-bonuscount-set! p 0)
          (player-extralight-set! p 0)
          (player-fixedcolormap-set! p 0)
          (player-viewheight-set! p VIEWHEIGHT)
          ;; TODO(milestone 5): P_SetupPsprites (setup gun psprite).
          ;; give all cards in death match mode
          (unless (fx= deathmatch 0)
            (vector-fill! (player-cards p) #t))
          ;; TODO(milestone 7): if this is the console player, ST_Start and
          ;; HU_Start (wake up the status bar and the heads up text).
          (void))))))

;; P_SpawnMapThing: the fields of the mapthing should already be in host
;; byte order.
(define (p-spawn-map-thing mthing)
  (let ([type (mapthing-type mthing)])
    (cond
      [(fx= type 11)
       ;; count deathmatch start positions; extras are dropped
       (when (fx< deathmatch_p MAX_DM_STARTS)
         (vector-set! deathmatchstarts deathmatch_p (copy-mapthing mthing))
         (set! deathmatch_p (fx+ deathmatch_p 1)))]
      [(fx<= type 4)
       ;; check for players specially. (C writes playerstarts[-1] and worse
       ;; for type <= 0.)
       (when (fx<= type 0) (i-error "P_SpawnMapThing: bad player start type ~a" type))
       ;; save spots for respawning in network games
       (vector-set! playerstarts (fx- type 1) (copy-mapthing mthing))
       (when (fx= deathmatch 0)
         (p-spawn-player mthing))]
      ;; check for apropriate skill level
      [(and (not netgame) (fxlogtest (mapthing-options mthing) 16)) (void)]
      [(not (fxlogtest (mapthing-options mthing)
                       (cond [(fx= gameskill sk_baby) 1]
                             [(fx= gameskill sk_nightmare) 4]
                             [else (fxsll 1 (fx- gameskill 1))])))
       (void)]
      [else
       ;; find which type to spawn
       (let ([i (let loop ([i 0])
                  (cond [(fx= i NUMMOBJTYPES)
                         (i-error "P_SpawnMapThing: Unknown type ~a at (~a, ~a)"
                                  type (mapthing-x mthing) (mapthing-y mthing))]
                        [(fx= type (mobjinfo-doomednum (vector-ref mobjinfo-table i))) i]
                        [else (loop (fx+ i 1))]))])
         (let ([flags (mobjinfo-flags (vector-ref mobjinfo-table i))])
           (cond
             ;; don't spawn keycards and players in deathmatch
             [(and (not (fx= deathmatch 0)) (fxlogtest flags MF_NOTDMATCH)) (void)]
             ;; don't spawn any monsters if -nomonsters
             [(and nomonsters (or (fx= i MT_SKULL) (fxlogtest flags MF_COUNTKILL))) (void)]
             [else
              ;; spawn it
              (let ([mobj (p-spawn-mobj (int32 (fxsll (mapthing-x mthing) FRACBITS))
                                        (int32 (fxsll (mapthing-y mthing) FRACBITS))
                                        (if (fxlogtest flags MF_SPAWNCEILING) ONCEILINGZ ONFLOORZ)
                                        i)])
                (mobj-spawnpoint-set! mobj (copy-mapthing mthing))
                (when (fx> (mobj-tics mobj) 0)
                  (mobj-tics-set! mobj (fx+ 1 (fxremainder (p-random) (mobj-tics mobj)))))
                (when (fxlogtest (mobj-flags mobj) MF_COUNTKILL)
                  (set! totalkills (fx+ totalkills 1)))
                (when (fxlogtest (mobj-flags mobj) MF_COUNTITEM)
                  (set! totalitems (fx+ totalitems 1)))
                (mobj-angle-set! mobj (mapthing-mobj-angle mthing))
                (when (fxlogtest (mapthing-options mthing) MTF_AMBUSH)
                  (mobj-flags-set! mobj (fxior (mobj-flags mobj) MF_AMBUSH))))])))])))
