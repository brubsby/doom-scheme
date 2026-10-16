;;; p_mobj.c: map objects. Included by p_local.sls.
;;;
;;; Status: complete, except P_SpawnPlayer's weapon sprites
;;; (P_SetupPsprites, milestone 5) and status bar / HUD wake-up
;;; (milestone 7).

;; P_SetMobjState: returns true if the mobj is still present. Calls the
;; action function of each state entered; zero-tic states chain on.
(define (p-set-mobj-state mobj state)
  (let loop ([state state])
    (if (fx= state S_NULL)
        (begin
          (mobj-state-set! mobj #f)
          (p-remove-mobj mobj)
          #f)
        (let ([st (vector-ref states state)])
          (mobj-state-set! mobj st)
          (mobj-tics-set! mobj (state-tics st))
          (mobj-sprite-set! mobj (state-sprite st))
          (mobj-frame-set! mobj (state-frame st))
          ;; Modified handling. Call action functions when the state is set
          (let ([action (state-action st)])
            (when action (action mobj)))
          (if (fx= (mobj-tics mobj) 0)
              (loop (state-nextstate st))
              #t)))))

;; P_ExplodeMissile
(define (p-explode-missile mo)
  (mobj-momx-set! mo 0)
  (mobj-momy-set! mo 0)
  (mobj-momz-set! mo 0)
  (p-set-mobj-state mo (mobjinfo-deathstate (vector-ref mobjinfo-table (mobj-type mo))))
  (mobj-tics-set! mo (fx- (mobj-tics mo) (fxand (p-random) 3)))
  (when (fx< (mobj-tics mo) 1)
    (mobj-tics-set! mo 1))
  (mobj-flags-set! mo (fxand (mobj-flags mo) (fxnot MF_MISSILE)))
  (unless (fx= (mobjinfo-deathsound (mobj-info mo)) 0)
    (s-start-sound mo (mobjinfo-deathsound (mobj-info mo)))))

;; P_XYMovement
(define STOPSPEED #x1000)
(define FRICTION #xe800)

(define (p-xy-movement mo)
  (if (and (fx= (mobj-momx mo) 0) (fx= (mobj-momy mo) 0))
      (when (fxlogtest (mobj-flags mo) MF_SKULLFLY)
        ;; the skull slammed into something
        (mobj-flags-set! mo (fxand (mobj-flags mo) (fxnot MF_SKULLFLY)))
        (mobj-momx-set! mo 0)
        (mobj-momy-set! mo 0)
        (mobj-momz-set! mo 0)
        (p-set-mobj-state mo (mobjinfo-spawnstate (mobj-info mo))))
      (let ([player (mobj-player mo)])
        (define (clamp v) (cond [(fx> v MAXMOVE) MAXMOVE] [(fx< v (- MAXMOVE)) (- MAXMOVE)] [else v]))
        (mobj-momx-set! mo (clamp (mobj-momx mo)))
        (mobj-momy-set! mo (clamp (mobj-momy mo)))
        (let ([removed
               (let loop ([xmove (mobj-momx mo)] [ymove (mobj-momy mo)])
                 (let-values ([(ptryx ptryy xmove ymove)
                               (if (or (fx> xmove (fxquotient MAXMOVE 2)) (fx> ymove (fxquotient MAXMOVE 2)))
                                   (values (i+ (mobj-x mo) (fxquotient xmove 2))
                                           (i+ (mobj-y mo) (fxquotient ymove 2))
                                           (fxsra xmove 1) (fxsra ymove 1))
                                   (values (i+ (mobj-x mo) xmove) (i+ (mobj-y mo) ymove) 0 0))])
                   (let ([removed
                          (and (not (p-try-move mo ptryx ptryy))
                               ;; blocked move
                               (cond
                                 ;; try to slide along it
                                 [(mobj-player mo) (p-slide-move mo) #f]
                                 [(fxlogtest (mobj-flags mo) MF_MISSILE)
                                  ;; explode a missile
                                  (if (and ceilingline
                                           (line-backsector ceilingline)
                                           (fx= (sector-ceilingpic (line-backsector ceilingline))
                                                skyflatnum))
                                      ;; Hack to prevent missiles exploding against
                                      ;; the sky. Does not handle sky floors.
                                      (begin (p-remove-mobj mo) #t)
                                      (begin (p-explode-missile mo) #f))]
                                 [else (mobj-momx-set! mo 0)
                                       (mobj-momy-set! mo 0)
                                       #f]))])
                     (cond [removed #t]
                           [(or (not (fx= xmove 0)) (not (fx= ymove 0))) (loop xmove ymove)]
                           [else #f]))))])
          (unless removed
            ;; slow down
            (cond
              [(and player (fxlogtest (player-cheats player) CF_NOMOMENTUM))
               ;; debug option for no sliding at all
               (mobj-momx-set! mo 0)
               (mobj-momy-set! mo 0)]
              ;; no friction for missiles ever
              [(fxlogtest (mobj-flags mo) (fxior MF_MISSILE MF_SKULLFLY)) (void)]
              ;; no friction when airborne
              [(fx> (mobj-z mo) (mobj-floorz mo)) (void)]
              ;; do not stop sliding if halfway off a step with some momentum
              [(and (fxlogtest (mobj-flags mo) MF_CORPSE)
                    (or (fx> (mobj-momx mo) (fxquotient FRACUNIT 4))
                        (fx< (mobj-momx mo) (- (fxquotient FRACUNIT 4)))
                        (fx> (mobj-momy mo) (fxquotient FRACUNIT 4))
                        (fx< (mobj-momy mo) (- (fxquotient FRACUNIT 4))))
                    (not (fx= (mobj-floorz mo) (sector-floorheight (subsector-sector (mobj-subsector mo))))))
               (void)]
              [(and (fx< (- STOPSPEED) (mobj-momx mo) STOPSPEED)
                    (fx< (- STOPSPEED) (mobj-momy mo) STOPSPEED)
                    (or (not player)
                        (and (fx= (ticcmd-forwardmove (player-cmd player)) 0)
                             (fx= (ticcmd-sidemove (player-cmd player)) 0))))
               ;; if in a walking frame, stop moving. (The unsigned compare
               ;; is false for a NULL state's huge negative difference.)
               (when (and player
                          (let ([st (mobj-state (player-mo player))])
                            (and st (fx< -1 (fx- (state-index st) S_PLAY_RUN1) 4))))
                 (p-set-mobj-state (player-mo player) S_PLAY))
               (mobj-momx-set! mo 0)
               (mobj-momy-set! mo 0)]
              [else
               (mobj-momx-set! mo (fixed-mul (mobj-momx mo) FRICTION))
               (mobj-momy-set! mo (fixed-mul (mobj-momy mo) FRICTION))]))))))

;; P_ZMovement
(define (p-z-movement mo)
  (let ([player (mobj-player mo)])
    ;; check for smooth step up
    (when (and player (fx< (mobj-z mo) (mobj-floorz mo)))
      (player-viewheight-set! player (i- (player-viewheight player) (i- (mobj-floorz mo) (mobj-z mo))))
      (player-deltaviewheight-set! player (fxsra (i- VIEWHEIGHT (player-viewheight player)) 3)))
    ;; adjust height
    (mobj-z-set! mo (i+ (mobj-z mo) (mobj-momz mo)))
    (when (and (fxlogtest (mobj-flags mo) MF_FLOAT) (mobj-target mo))
      ;; float down towards target if too close
      (unless (fxlogtest (mobj-flags mo) (fxior MF_SKULLFLY MF_INFLOAT))
        (let* ([target (mobj-target mo)]
               [dist (p-aprox-distance (i- (mobj-x mo) (mobj-x target)) (i- (mobj-y mo) (mobj-y target)))]
               [delta (i- (i+ (mobj-z target) (fxsra (mobj-height mo) 1)) (mobj-z mo))])
          (cond
            [(and (fx< delta 0) (fx< dist (int32 (fx- 0 (fx* delta 3)))))
             (mobj-z-set! mo (i- (mobj-z mo) FLOATSPEED))]
            [(and (fx> delta 0) (fx< dist (int32 (fx* delta 3))))
             (mobj-z-set! mo (i+ (mobj-z mo) FLOATSPEED))]))))
    (let ([exploded
           ;; clip movement
           (if (fx<= (mobj-z mo) (mobj-floorz mo))
               ;; hit the floor
               (begin
                 ;; Note (id): somebody left this after the setting momz to
                 ;; 0, kinda useless there.
                 (when (fxlogtest (mobj-flags mo) MF_SKULLFLY)
                   ;; the skull slammed into something
                   (mobj-momz-set! mo (ineg (mobj-momz mo))))
                 (when (fx< (mobj-momz mo) 0)
                   (when (and player (fx< (mobj-momz mo) (* GRAVITY -8)))
                     ;; Squat down. Decrease viewheight for a moment after
                     ;; hitting the ground (hard), and utter appropriate
                     ;; sound.
                     (player-deltaviewheight-set! player (fxsra (mobj-momz mo) 3))
                     (s-start-sound mo sfx_oof))
                   (mobj-momz-set! mo 0))
                 (mobj-z-set! mo (mobj-floorz mo))
                 (and (fxlogtest (mobj-flags mo) MF_MISSILE)
                      (not (fxlogtest (mobj-flags mo) MF_NOCLIP))
                      (begin (p-explode-missile mo) #t)))
               (begin
                 (unless (fxlogtest (mobj-flags mo) MF_NOGRAVITY)
                   (if (fx= (mobj-momz mo) 0)
                       (mobj-momz-set! mo (* GRAVITY -2))
                       (mobj-momz-set! mo (i- (mobj-momz mo) GRAVITY))))
                 #f))])
      (when (and (not exploded)
                 (fx> (i+ (mobj-z mo) (mobj-height mo)) (mobj-ceilingz mo)))
        ;; hit the ceiling
        (when (fx> (mobj-momz mo) 0)
          (mobj-momz-set! mo 0))
        (mobj-z-set! mo (i- (mobj-ceilingz mo) (mobj-height mo)))
        (when (fxlogtest (mobj-flags mo) MF_SKULLFLY)
          ;; the skull slammed into something
          (mobj-momz-set! mo (ineg (mobj-momz mo))))
        (when (and (fxlogtest (mobj-flags mo) MF_MISSILE)
                   (not (fxlogtest (mobj-flags mo) MF_NOCLIP)))
          (p-explode-missile mo))))))

;; P_NightmareRespawn
(define (p-nightmare-respawn mobj)
  (let* ([sp (mobj-spawnpoint mobj)]
         [x (int32 (fxsll (mapthing-x sp) FRACBITS))]
         [y (int32 (fxsll (mapthing-y sp) FRACBITS))])
    ;; somthing is occupying it's position?
    (when (p-check-position mobj x y)
      ;; spawn a teleport fog at old spot because of removal of the body?
      (let ([mo (p-spawn-mobj (mobj-x mobj) (mobj-y mobj)
                              (sector-floorheight (subsector-sector (mobj-subsector mobj))) MT_TFOG)])
        ;; initiate teleport sound
        (s-start-sound mo sfx_telept))
      ;; spawn a teleport fog at the new spot
      (let* ([ss (r-point-in-subsector x y)]
             [mo (p-spawn-mobj x y (sector-floorheight (subsector-sector ss)) MT_TFOG)])
        (s-start-sound mo sfx_telept))
      ;; spawn the new monster, inheriting attributes from the deceased one
      (let ([mo (p-spawn-mobj x y
                              (if (fxlogtest (mobjinfo-flags (mobj-info mobj)) MF_SPAWNCEILING)
                                  ONCEILINGZ
                                  ONFLOORZ)
                              (mobj-type mobj))])
        (mobj-spawnpoint-set! mo (copy-mapthing sp))
        (mobj-angle-set! mo (mapthing-mobj-angle sp))
        (when (fxlogtest (mapthing-options sp) MTF_AMBUSH)
          (mobj-flags-set! mo (fxior (mobj-flags mo) MF_AMBUSH)))
        (mobj-reactiontime-set! mo 18))
      ;; remove the old monster,
      (p-remove-mobj mobj))))

;; P_MobjThinker. Its identity also marks a thinker as an mobj (C compares
;; the function pointer).
(define (p-mobj-thinker mobj)
  (define (removed?) (eq? (thinker-func mobj) 'removed))
  ;; momentum movement
  (unless (or (and (or (not (fx= (mobj-momx mobj) 0)) (not (fx= (mobj-momy mobj) 0))
                       (fxlogtest (mobj-flags mobj) MF_SKULLFLY))
                   (begin (p-xy-movement mobj) (removed?)))
              (and (or (not (fx= (mobj-z mobj) (mobj-floorz mobj))) (not (fx= (mobj-momz mobj) 0)))
                   (begin (p-z-movement mobj) (removed?))))
    ;; cycle through states, calling action functions at transitions
    (if (not (fx= (mobj-tics mobj) -1))
        (begin
          (mobj-tics-set! mobj (fx- (mobj-tics mobj) 1))
          ;; you can cycle through multiple states in a tic
          (when (fx= (mobj-tics mobj) 0)
            (p-set-mobj-state mobj (state-nextstate (mobj-state mobj)))))
        ;; check for nightmare respawn
        (when (and (fxlogtest (mobj-flags mobj) MF_COUNTKILL)
                   respawnmonsters
                   (begin (mobj-movecount-set! mobj (fx+ (mobj-movecount mobj) 1))
                          (fx>= (mobj-movecount mobj) (* 12 35)))
                   (fx= (fxand leveltime 31) 0)
                   (not (fx> (p-random) 4)))
          (p-nightmare-respawn mobj)))))

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

;; P_RemoveMobj. Removed items (in a deathmatch 2 game) are queued for
;; respawning.
(define itemrespawnque (make-vector ITEMQUESIZE #f))
(define itemrespawntime (make-fxvector ITEMQUESIZE 0))
(define iquehead 0)
(define iquetail 0)

(define (p-remove-mobj mobj)
  (when (and (fxlogtest (mobj-flags mobj) MF_SPECIAL)
             (not (fxlogtest (mobj-flags mobj) MF_DROPPED))
             (not (fx= (mobj-type mobj) MT_INV))
             (not (fx= (mobj-type mobj) MT_INS)))
    (vector-set! itemrespawnque iquehead (copy-mapthing (mobj-spawnpoint mobj)))
    (fxvector-set! itemrespawntime iquehead leveltime)
    (set! iquehead (fxand (fx+ iquehead 1) (fx- ITEMQUESIZE 1)))
    ;; lose one off the end?
    (when (fx= iquehead iquetail)
      (set! iquetail (fxand (fx+ iquetail 1) (fx- ITEMQUESIZE 1)))))
  ;; unlink from sector and block lists
  (p-unset-thing-position mobj)
  ;; stop any playing sound
  (s-stop-sound mobj)
  ;; free block
  (p-remove-thinker mobj))

;; P_RespawnSpecials
(define (p-respawn-specials)
  ;; only respawn items in deathmatch; nothing left to respawn?; wait at
  ;; least 30 seconds
  (when (and (fx= deathmatch 2)
             (not (fx= iquehead iquetail))
             (not (fx< (fx- leveltime (fxvector-ref itemrespawntime iquetail)) (* 30 35))))
    (let* ([mthing (vector-ref itemrespawnque iquetail)]
           [x (int32 (fxsll (mapthing-x mthing) FRACBITS))]
           [y (int32 (fxsll (mapthing-y mthing) FRACBITS))])
      ;; spawn a teleport fog at the new spot
      (let* ([ss (r-point-in-subsector x y)]
             [mo (p-spawn-mobj x y (sector-floorheight (subsector-sector ss)) MT_IFOG)])
        (s-start-sound mo sfx_itmbk))
      ;; find which type to spawn. (C reads past mobjinfo if none matches.)
      (let* ([i (let loop ([i 0])
                  (cond [(fx= i NUMMOBJTYPES) (i-error "P_RespawnSpecials: unknown type")]
                        [(fx= (mapthing-type mthing) (mobjinfo-doomednum (vector-ref mobjinfo-table i))) i]
                        [else (loop (fx+ i 1))]))]
             ;; spawn it
             [mo (p-spawn-mobj x y
                               (if (fxlogtest (mobjinfo-flags (vector-ref mobjinfo-table i)) MF_SPAWNCEILING)
                                   ONCEILINGZ
                                   ONFLOORZ)
                               i)])
        (mobj-spawnpoint-set! mo (copy-mapthing mthing))
        (mobj-angle-set! mo (mapthing-mobj-angle mthing)))
      ;; pull it from the que
      (set! iquetail (fxand (fx+ iquetail 1) (fx- ITEMQUESIZE 1))))))

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
          ;; setup gun psprite
          (p-setup-psprites p)
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

;; GAME SPAWN FUNCTIONS

;; P_SpawnPuff
(define (p-spawn-puff x y z)
  (let* ([z (i+ z (fxsll (p-random-diff) 10))]
         [th (p-spawn-mobj x y z MT_PUFF)])
    (mobj-momz-set! th FRACUNIT)
    (mobj-tics-set! th (fx- (mobj-tics th) (fxand (p-random) 3)))
    (when (fx< (mobj-tics th) 1)
      (mobj-tics-set! th 1))
    ;; don't make punches spark on the wall
    (when (fx= attackrange MELEERANGE)
      (p-set-mobj-state th S_PUFF3))))

;; P_SpawnBlood
(define (p-spawn-blood x y z damage)
  (let* ([z (i+ z (fxsll (p-random-diff) 10))]
         [th (p-spawn-mobj x y z MT_BLOOD)])
    (mobj-momz-set! th (* FRACUNIT 2))
    (mobj-tics-set! th (fx- (mobj-tics th) (fxand (p-random) 3)))
    (when (fx< (mobj-tics th) 1)
      (mobj-tics-set! th 1))
    (cond [(fx<= 9 damage 12) (p-set-mobj-state th S_BLOOD2)]
          [(fx< damage 9) (p-set-mobj-state th S_BLOOD3)])))

;; P_CheckMissileSpawn: moves the missile forward a bit and possibly
;; explodes it right there.
(define (p-check-missile-spawn th)
  (mobj-tics-set! th (fx- (mobj-tics th) (fxand (p-random) 3)))
  (when (fx< (mobj-tics th) 1)
    (mobj-tics-set! th 1))
  ;; move a little forward so an angle can be computed if it immediately
  ;; explodes
  (mobj-x-set! th (i+ (mobj-x th) (fxsra (mobj-momx th) 1)))
  (mobj-y-set! th (i+ (mobj-y th) (fxsra (mobj-momy th) 1)))
  (mobj-z-set! th (i+ (mobj-z th) (fxsra (mobj-momz th) 1)))
  (unless (p-try-move th (mobj-x th) (mobj-y th))
    (p-explode-missile th)))

;; P_SpawnMissile
(define (p-spawn-missile source dest type)
  (let ([th (p-spawn-mobj (mobj-x source) (mobj-y source) (i+ (mobj-z source) (* 4 8 FRACUNIT)) type)])
    (unless (fx= (mobjinfo-seesound (mobj-info th)) 0)
      (s-start-sound th (mobjinfo-seesound (mobj-info th))))
    (mobj-target-set! th source)        ; where it came from
    (let* ([an (r-point-to-angle2 (mobj-x source) (mobj-y source) (mobj-x dest) (mobj-y dest))]
           ;; fuzzy player
           [an (if (fxlogtest (mobj-flags dest) MF_SHADOW)
                   (angle+ an (fxand (fxsll (p-random-diff) 20) #xFFFFFFFF))
                   an)]
           [speed (mobjinfo-speed (mobj-info th))])
      (mobj-angle-set! th an)
      (let ([an (fxsrl an ANGLETOFINESHIFT)])
        (mobj-momx-set! th (fixed-mul speed (finecosine an)))
        (mobj-momy-set! th (fixed-mul speed (fxvector-ref finesine an))))
      (let* ([dist (p-aprox-distance (i- (mobj-x dest) (mobj-x source)) (i- (mobj-y dest) (mobj-y source)))]
             [dist (fxquotient dist speed)]
             [dist (if (fx< dist 1) 1 dist)])
        (mobj-momz-set! th (fxquotient (i- (mobj-z dest) (mobj-z source)) dist))))
    (p-check-missile-spawn th)
    th))

;; P_SpawnPlayerMissile: tries to aim at a nearby monster.
(define (p-spawn-player-missile source type)
  ;; see which target is to be aimed at
  (let*-values ([(an slope)
                 (let* ([an (mobj-angle source)]
                        [slope (p-aim-line-attack source an (* 16 64 FRACUNIT))])
                   (if linetarget
                       (values an slope)
                       (let* ([an (angle+ an (fxsll 1 26))]
                              [slope (p-aim-line-attack source an (* 16 64 FRACUNIT))])
                         (if linetarget
                             (values an slope)
                             (let* ([an (angle- an (fxsll 2 26))]
                                    [slope (p-aim-line-attack source an (* 16 64 FRACUNIT))])
                               (if linetarget
                                   (values an slope)
                                   (values (mobj-angle source) 0)))))))])
    (let ([th (p-spawn-mobj (mobj-x source) (mobj-y source) (i+ (mobj-z source) (* 4 8 FRACUNIT)) type)])
      (unless (fx= (mobjinfo-seesound (mobj-info th)) 0)
        (s-start-sound th (mobjinfo-seesound (mobj-info th))))
      (mobj-target-set! th source)
      (mobj-angle-set! th an)
      (let ([speed (mobjinfo-speed (mobj-info th))] [fan (fxsrl an ANGLETOFINESHIFT)])
        (mobj-momx-set! th (fixed-mul speed (finecosine fan)))
        (mobj-momy-set! th (fixed-mul speed (fxvector-ref finesine fan)))
        (mobj-momz-set! th (fixed-mul speed slope)))
      (p-check-missile-spawn th))))
