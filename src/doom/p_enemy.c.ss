;;; p_enemy.c: enemy thinking, AI, and the monsters' action functions.
;;; Included by p_local.sls. Actions take the mobj (C: acp1).

;; dirtype_t
(define DI_EAST 0)
(define DI_NORTHEAST 1)
(define DI_NORTH 2)
(define DI_NORTHWEST 3)
(define DI_WEST 4)
(define DI_SOUTHWEST 5)
(define DI_SOUTH 6)
(define DI_SOUTHEAST 7)
(define DI_NODIR 8)
(define NUMDIRS 9)

;; P_NewChaseDir related LUT.
(define opposite
  (fxvector DI_WEST DI_SOUTHWEST DI_SOUTH DI_SOUTHEAST
            DI_EAST DI_NORTHEAST DI_NORTH DI_NORTHWEST DI_NODIR))
(define diags (fxvector DI_NORTHWEST DI_NORTHEAST DI_SOUTHWEST DI_SOUTHEAST))

;; ENEMY THINKING
;; Enemies are allways spawned with targetplayer = -1, threshold = 0. Most
;; monsters are spawned unaware of all players, but some can be made
;; preaware.

;; Called by P_NoiseAlert. Recursively traverse adjacent sectors, sound
;; blocking lines cut off traversal.
(define soundtarget #f)

(define (p-recursive-sound sec soundblocks)
  ;; wake up all monsters in this sector
  (unless (and (fx= (sector-validcount sec) validcount)
               (fx<= (sector-soundtraversed sec) (fx+ soundblocks 1)))
    ;; (not already flooded)
    (sector-validcount-set! sec validcount)
    (sector-soundtraversed-set! sec (fx+ soundblocks 1))
    (sector-soundtarget-set! sec soundtarget)
    (do ([i 0 (fx+ i 1)]) ((fx= i (sector-linecount sec)))
      (let ([check (vector-ref (sector-lines sec) i)])
        (when (fxlogtest (line-flags check) ML_TWOSIDED)
          (p-line-opening check)
          (when (fx> openrange 0)       ; (else closed door)
            (let* ([sn (line-sidenum check)]
                   [other (if (eq? (side-sector (vector-ref sides (fxvector-ref sn 0))) sec)
                              (side-sector (vector-ref sides (fxvector-ref sn 1)))
                              (side-sector (vector-ref sides (fxvector-ref sn 0))))])
              (if (fxlogtest (line-flags check) ML_SOUNDBLOCK)
                  (when (fx= soundblocks 0)
                    (p-recursive-sound other 1))
                  (p-recursive-sound other soundblocks)))))))))

;; P_NoiseAlert: if a monster yells at a player, it will alert other
;; monsters to the player.
(define (p-noise-alert target emmiter)
  (set! soundtarget target)
  (set! validcount (fx+ validcount 1))
  (p-recursive-sound (subsector-sector (mobj-subsector emmiter)) 0))

;; P_CheckMeleeRange
(define (p-check-melee-range actor)
  (let ([pl (mobj-target actor)])
    (and pl
         (fx< (p-aprox-distance (i- (mobj-x pl) (mobj-x actor)) (i- (mobj-y pl) (mobj-y actor)))
              (i+ (i- MELEERANGE (* 20 FRACUNIT)) (mobjinfo-radius (mobj-info pl))))
         (p-check-sight actor pl))))

;; P_CheckMissileRange
(define (p-check-missile-range actor)
  (cond
    [(not (p-check-sight actor (mobj-target actor))) #f]
    [(fxlogtest (mobj-flags actor) MF_JUSTHIT)
     ;; the target just hit the enemy, so fight back!
     (mobj-flags-set! actor (fxand (mobj-flags actor) (fxnot MF_JUSTHIT)))
     #t]
    [(not (fx= (mobj-reactiontime actor) 0)) #f] ; do not attack yet
    [else
     ;; OPTIMIZE: get this from a global checksight
     (let* ([target (mobj-target actor)]
            [type (mobj-type actor)]
            [dist (i- (p-aprox-distance (i- (mobj-x actor) (mobj-x target)) (i- (mobj-y actor) (mobj-y target)))
                      (* 64 FRACUNIT))]
            ;; no melee attack, so fire more
            [dist (if (fx= (mobjinfo-meleestate (mobj-info actor)) 0) (i- dist (* 128 FRACUNIT)) dist)]
            [dist (fxsra dist 16)])
       (cond
         [(and (fx= type MT_VILE) (fx> dist (* 14 64))) #f] ; too far away
         [(and (fx= type MT_UNDEAD) (fx< dist 196)) #f]     ; close for fist attack
         [else
          (let* ([dist (if (fx= type MT_UNDEAD) (fxsra dist 1) dist)]
                 [dist (if (or (fx= type MT_CYBORG) (fx= type MT_SPIDER) (fx= type MT_SKULL))
                           (fxsra dist 1)
                           dist)]
                 [dist (fxmin dist 200)]
                 [dist (if (and (fx= type MT_CYBORG) (fx> dist 160)) 160 dist)])
            (not (fx< (p-random) dist)))]))]))

;; P_Move: move in the current direction, returns false if the move is
;; blocked.
(define xspeed (fxvector FRACUNIT 47000 0 -47000 (- FRACUNIT) -47000 0 47000))
(define yspeed (fxvector 0 47000 FRACUNIT 47000 0 -47000 (- FRACUNIT) -47000))

(define (p-move actor)
  (let ([movedir (mobj-movedir actor)])
    (cond
      [(fx= movedir DI_NODIR) #f]
      [(not (fx< -1 movedir 8)) (i-error "Weird actor->movedir!")]
      [else
       (let* ([speed (mobjinfo-speed (mobj-info actor))]
              [tryx (i+ (mobj-x actor) (int32 (fx* speed (fxvector-ref xspeed movedir))))]
              [tryy (i+ (mobj-y actor) (int32 (fx* speed (fxvector-ref yspeed movedir))))])
         (if (not (p-try-move actor tryx tryy))
             ;; open any specials
             (cond
               [(and (fxlogtest (mobj-flags actor) MF_FLOAT) floatok)
                ;; must adjust height
                (if (fx< (mobj-z actor) tmfloorz)
                    (mobj-z-set! actor (i+ (mobj-z actor) FLOATSPEED))
                    (mobj-z-set! actor (i- (mobj-z actor) FLOATSPEED)))
                (mobj-flags-set! actor (fxior (mobj-flags actor) MF_INFLOAT))
                #t]
               [(fx= numspechit 0) #f]
               [else
                (mobj-movedir-set! actor DI_NODIR)
                (let loop ([good #f])
                  (if (fx= numspechit 0)
                      (begin (set! numspechit -1) good) ; (as C's while (numspechit--))
                      (begin
                        (set! numspechit (fx- numspechit 1))
                        ;; if the special is not a door that can be opened,
                        ;; return false
                        (loop (or (p-use-special-line actor (vector-ref spechit numspechit) 0) good)))))])
             (begin
               (mobj-flags-set! actor (fxand (mobj-flags actor) (fxnot MF_INFLOAT)))
               (unless (fxlogtest (mobj-flags actor) MF_FLOAT)
                 (mobj-z-set! actor (mobj-floorz actor)))
               #t)))])))

;; TryWalk: attempts to move actor on in its current (ob->moveangle)
;; direction. If blocked by either a wall or an actor returns FALSE. If
;; move is either clear or blocked only by a door, returns TRUE and sets...
;; If a door is in the way, an OpenDoor call is made to start it opening.
(define (p-try-walk actor)
  (and (p-move actor)
       (begin
         (mobj-movecount-set! actor (fxand (p-random) 15))
         #t)))

(define (p-new-chase-dir actor)
  (unless (mobj-target actor)
    (i-error "P_NewChaseDir: called with no target"))
  (let* ([olddir (mobj-movedir actor)]
         [turnaround (fxvector-ref opposite olddir)]
         [deltax (i- (mobj-x (mobj-target actor)) (mobj-x actor))]
         [deltay (i- (mobj-y (mobj-target actor)) (mobj-y actor))]
         [d1 (cond [(fx> deltax (* 10 FRACUNIT)) DI_EAST]
                   [(fx< deltax (* -10 FRACUNIT)) DI_WEST]
                   [else DI_NODIR])]
         [d2 (cond [(fx< deltay (* -10 FRACUNIT)) DI_SOUTH]
                   [(fx> deltay (* 10 FRACUNIT)) DI_NORTH]
                   [else DI_NODIR])]
         (try (lambda (dir) (mobj-movedir-set! actor dir) (p-try-walk actor))))
    (unless
        (or
         ;; try direct route
         (and (not (fx= d1 DI_NODIR)) (not (fx= d2 DI_NODIR))
              (let ([dir (fxvector-ref diags (fx+ (fxsll (if (fx< deltay 0) 1 0) 1) (if (fx> deltax 0) 1 0)))])
                (mobj-movedir-set! actor dir)
                (and (not (fx= dir turnaround)) (p-try-walk actor))))
         ;; try other directions
         (let*-values ([(d1 d2) (if (or (fx> (p-random) 200) (fx> (c-abs deltay) (c-abs deltax)))
                                    (values d2 d1)
                                    (values d1 d2))]
                       [(d1) (if (fx= d1 turnaround) DI_NODIR d1)]
                       [(d2) (if (fx= d2 turnaround) DI_NODIR d2)])
           (or (and (not (fx= d1 DI_NODIR)) (try d1)) ; either moved forward or attacked
               (and (not (fx= d2 DI_NODIR)) (try d2))))
         ;; there is no direct path to the player, so pick another
         ;; direction.
         (and (not (fx= olddir DI_NODIR)) (try olddir))
         ;; randomly determine direction of search
         (if (fxlogtest (p-random) 1)
             (let loop ([tdir DI_EAST])
               (and (fx<= tdir DI_SOUTHEAST)
                    (or (and (not (fx= tdir turnaround)) (try tdir))
                        (loop (fx+ tdir 1)))))
             (let loop ([tdir DI_SOUTHEAST])
               (and (not (fx= tdir (fx- DI_EAST 1)))
                    (or (and (not (fx= tdir turnaround)) (try tdir))
                        (loop (fx- tdir 1))))))
         (and (not (fx= turnaround DI_NODIR)) (try turnaround)))
      (mobj-movedir-set! actor DI_NODIR)))) ; can not move

;; P_LookForPlayers: if allaround is false, only look 180 degrees in front.
;; Returns true if a player is targeted.
(define (p-look-for-players actor allaround)
  (let ([stop (fxand (fx- (mobj-lastlook actor) 1) 3)])
    (let loop ([c 0])
      (let ([next (lambda (c)
                    (mobj-lastlook-set! actor (fxand (fx+ (mobj-lastlook actor) 1) 3))
                    (loop c))])
        (if (not (vector-ref playeringame (mobj-lastlook actor)))
            (next c)
            (if (or (fx= c 2) (fx= (mobj-lastlook actor) stop))
                ;; done looking
                #f
                (let* ([c (fx+ c 1)]
                       [player (vector-ref players (mobj-lastlook actor))]
                       [pmo (player-mo player)])
                  (cond
                    [(fx<= (player-health player) 0) (next c)] ; dead
                    [(not (p-check-sight actor pmo)) (next c)] ; out of sight
                    [(and (not allaround)
                          (let ([an (angle- (r-point-to-angle2 (mobj-x actor) (mobj-y actor)
                                                               (mobj-x pmo) (mobj-y pmo))
                                            (mobj-angle actor))])
                            (and (fx> an ANG90) (fx< an ANG270)
                                 ;; if real close, react anyway
                                 (fx> (p-aprox-distance (i- (mobj-x pmo) (mobj-x actor))
                                                        (i- (mobj-y pmo) (mobj-y actor)))
                                      MELEERANGE))))
                     (next c)]                                 ; behind back
                    [else
                     (mobj-target-set! actor pmo)
                     #t]))))))))

;; A line with just a tag, for the boss specials (C: a local line_t with
;; only the tag set).
(define (tag-line tag)
  (let ([l (make-line)])
    (line-tag-set! l tag)
    l))

;; Every other live mobj of the same type is dead?
(define (all-others-dead? mo)
  (let loop ([th (thinker-next thinkercap)])
    (cond
      [(eq? th thinkercap) #t]
      [(and (eq? (thinker-func th) p-mobj-thinker)
            (not (eq? th mo))
            (fx= (mobj-type th) (mobj-type mo))
            (fx> (mobj-health th) 0))
       #f]
      [else (loop (thinker-next th))])))

;; A_KeenDie: DOOM II special, map 32. Uses special tag 666.
(define (a-keen-die mo)
  (a-fall mo)
  ;; scan the remaining thinkers to see if all Keens are dead
  (when (all-others-dead? mo)
    (ev-do-door (tag-line 666) open)))

;; ACTION ROUTINES

;; A_Look: stay in state until a player is sighted.
(define (a-look actor)
  (mobj-threshold-set! actor 0)         ; any shot will wake up
  (when (let ([targ (sector-soundtarget (subsector-sector (mobj-subsector actor)))])
          (or (and targ
                   (fxlogtest (mobj-flags targ) MF_SHOOTABLE)
                   (begin
                     (mobj-target-set! actor targ)
                     (or (not (fxlogtest (mobj-flags actor) MF_AMBUSH))
                         (p-check-sight actor (mobj-target actor)))))
              (p-look-for-players actor #f)))
    ;; go into chase state
    (let ([seesound (mobjinfo-seesound (mobj-info actor))])
      (unless (fx= seesound 0)
        (let ([sound (cond
                       [(memv seesound (list sfx_posit1 sfx_posit2 sfx_posit3))
                        (fx+ sfx_posit1 (fxremainder (p-random) 3))]
                       [(memv seesound (list sfx_bgsit1 sfx_bgsit2))
                        (fx+ sfx_bgsit1 (fxremainder (p-random) 2))]
                       [else seesound])])
          (if (or (fx= (mobj-type actor) MT_SPIDER) (fx= (mobj-type actor) MT_CYBORG))
              ;; full volume
              (s-start-sound #f sound)
              (s-start-sound actor sound)))))
    (p-set-mobj-state actor (mobjinfo-seestate (mobj-info actor)))))

;; A_Chase: actor has a melee attack, so it tries to close as fast as
;; possible.
(define (a-chase actor)
  (unless (fx= (mobj-reactiontime actor) 0)
    (mobj-reactiontime-set! actor (fx- (mobj-reactiontime actor) 1)))
  ;; modify target threshold
  (unless (fx= (mobj-threshold actor) 0)
    (if (or (not (mobj-target actor)) (fx<= (mobj-health (mobj-target actor)) 0))
        (mobj-threshold-set! actor 0)
        (mobj-threshold-set! actor (fx- (mobj-threshold actor) 1))))
  ;; turn towards movement direction if not there yet
  (when (fx< (mobj-movedir actor) 8)
    (mobj-angle-set! actor (fxand (mobj-angle actor) (fxsll 7 29)))
    ;; delta is an int: the unsigned difference's sign
    (let ([delta (int32 (fx- (mobj-angle actor) (fxsll (mobj-movedir actor) 29)))])
      (cond [(fx> delta 0) (mobj-angle-set! actor (angle- (mobj-angle actor) (fxquotient ANG90 2)))]
            [(fx< delta 0) (mobj-angle-set! actor (angle+ (mobj-angle actor) (fxquotient ANG90 2)))])))
  (let ([info (mobj-info actor)])
    (cond
      [(or (not (mobj-target actor))
           (not (fxlogtest (mobj-flags (mobj-target actor)) MF_SHOOTABLE)))
       ;; look for a new target
       (unless (p-look-for-players actor #t) ; got a new target
         (p-set-mobj-state actor (mobjinfo-spawnstate info)))]
      ;; do not attack twice in a row
      [(fxlogtest (mobj-flags actor) MF_JUSTATTACKED)
       (mobj-flags-set! actor (fxand (mobj-flags actor) (fxnot MF_JUSTATTACKED)))
       (when (and (not (fx= gameskill sk_nightmare)) (not fastparm))
         (p-new-chase-dir actor))]
      ;; check for melee attack
      [(and (not (fx= (mobjinfo-meleestate info) 0)) (p-check-melee-range actor))
       (unless (fx= (mobjinfo-attacksound info) 0)
         (s-start-sound actor (mobjinfo-attacksound info)))
       (p-set-mobj-state actor (mobjinfo-meleestate info))]
      ;; check for missile attack
      [(and (not (fx= (mobjinfo-missilestate info) 0))
            (not (and (fx< gameskill sk_nightmare) (not fastparm)
                      (not (fx= (mobj-movecount actor) 0))))
            (p-check-missile-range actor))
       (p-set-mobj-state actor (mobjinfo-missilestate info))
       (mobj-flags-set! actor (fxior (mobj-flags actor) MF_JUSTATTACKED))]
      ;; ? nomissile:
      ;; possibly choose another target
      [(and netgame
            (fx= (mobj-threshold actor) 0)
            (not (p-check-sight actor (mobj-target actor)))
            (p-look-for-players actor #t))
       (void)]                          ; got a new target
      [else
       ;; chase towards player
       (mobj-movecount-set! actor (fx- (mobj-movecount actor) 1))
       (when (or (fx< (mobj-movecount actor) 0) (not (p-move actor)))
         (p-new-chase-dir actor))
       ;; make active sound
       (when (and (not (fx= (mobjinfo-activesound info) 0)) (fx< (p-random) 3))
         (s-start-sound actor (mobjinfo-activesound info)))])))

;; A_FaceTarget
(define (a-face-target actor)
  (let ([target (mobj-target actor)])
    (when target
      (mobj-flags-set! actor (fxand (mobj-flags actor) (fxnot MF_AMBUSH)))
      (mobj-angle-set! actor (r-point-to-angle2 (mobj-x actor) (mobj-y actor) (mobj-x target) (mobj-y target)))
      (when (fxlogtest (mobj-flags target) MF_SHADOW)
        (mobj-angle-set! actor (angle+ (mobj-angle actor) (fxand (fxsll (p-random-diff) 21) #xFFFFFFFF)))))))

(define (spread angle shift)
  (angle+ angle (fxand (fxsll (p-random-diff) shift) #xFFFFFFFF)))

;; A_PosAttack
(define (a-pos-attack actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (let* ([angle (mobj-angle actor)]
           [slope (p-aim-line-attack actor angle MISSILERANGE)])
      (s-start-sound actor sfx_pistol)
      (let* ([angle (spread angle 20)]
             [damage (fx* (fx+ (fxremainder (p-random) 5) 1) 3)])
        (p-line-attack actor angle MISSILERANGE slope damage)))))

(define (a-spos-attack actor)
  (when (mobj-target actor)
    (s-start-sound actor sfx_shotgn)
    (a-face-target actor)
    (let* ([bangle (mobj-angle actor)]
           [slope (p-aim-line-attack actor bangle MISSILERANGE)])
      (do ([i 0 (fx+ i 1)]) ((fx= i 3))
        (let* ([angle (spread bangle 20)]
               [damage (fx* (fx+ (fxremainder (p-random) 5) 1) 3)])
          (p-line-attack actor angle MISSILERANGE slope damage))))))

(define (a-cpos-attack actor)
  (when (mobj-target actor)
    (s-start-sound actor sfx_shotgn)
    (a-face-target actor)
    (let* ([bangle (mobj-angle actor)]
           [slope (p-aim-line-attack actor bangle MISSILERANGE)]
           [angle (spread bangle 20)]
           [damage (fx* (fx+ (fxremainder (p-random) 5) 1) 3)])
      (p-line-attack actor angle MISSILERANGE slope damage))))

;; keep firing unless target got out of sight
(define (refire actor chance)
  (a-face-target actor)
  (unless (fx< (p-random) chance)
    (when (or (not (mobj-target actor))
              (fx<= (mobj-health (mobj-target actor)) 0)
              (not (p-check-sight actor (mobj-target actor))))
      (p-set-mobj-state actor (mobjinfo-seestate (mobj-info actor))))))

(define (a-cpos-refire actor) (refire actor 40))
(define (a-spid-refire actor) (refire actor 10))

(define (a-bspi-attack actor)
  (when (mobj-target actor)
    (a-face-target actor)
    ;; launch a missile
    (p-spawn-missile actor (mobj-target actor) MT_ARACHPLAZ)))

;; A melee attack if in range, else (if missile) launch one.
(define (melee-or-missile actor sound damage-fn missile)
  (when (mobj-target actor)
    (a-face-target actor)
    (if (p-check-melee-range actor)
        (begin
          (when sound (s-start-sound actor sound))
          (let ([damage (damage-fn)])
            (p-damage-mobj (mobj-target actor) actor actor damage)))
        (when missile
          ;; launch a missile
          (p-spawn-missile actor (mobj-target actor) missile)))))

;; A_TroopAttack
(define (a-troop-attack actor)
  (melee-or-missile actor sfx_claw (lambda () (fx* (fx+ (fxremainder (p-random) 8) 1) 3)) MT_TROOPSHOT))

(define (a-sarg-attack actor)
  (melee-or-missile actor #f (lambda () (fx* (fx+ (fxremainder (p-random) 10) 1) 4)) #f))

(define (a-head-attack actor)
  (melee-or-missile actor #f (lambda () (fx* (fx+ (fxremainder (p-random) 6) 1) 10)) MT_HEADSHOT))

(define (a-cyber-attack actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (p-spawn-missile actor (mobj-target actor) MT_ROCKET)))

;; (no A_FaceTarget here)
(define (a-bruis-attack actor)
  (when (mobj-target actor)
    (if (p-check-melee-range actor)
        (begin
          (s-start-sound actor sfx_claw)
          (p-damage-mobj (mobj-target actor) actor actor (fx* (fx+ (fxremainder (p-random) 8) 1) 10)))
        ;; launch a missile
        (p-spawn-missile actor (mobj-target actor) MT_BRUISERSHOT))))

;; A_SkelMissile
(define (a-skel-missile actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (mobj-z-set! actor (i+ (mobj-z actor) (* 16 FRACUNIT))) ; so missile spawns higher
    (let ([mo (p-spawn-missile actor (mobj-target actor) MT_TRACER)])
      (mobj-z-set! actor (i- (mobj-z actor) (* 16 FRACUNIT))) ; back to normal
      (mobj-x-set! mo (i+ (mobj-x mo) (mobj-momx mo)))
      (mobj-y-set! mo (i+ (mobj-y mo) (mobj-momy mo)))
      (mobj-tracer-set! mo (mobj-target actor)))))

(define TRACEANGLE #xc000000)

(define (a-tracer actor)
  (unless (fxlogtest gametic 3)
    ;; spawn a puff of smoke behind the rocket
    (p-spawn-puff (mobj-x actor) (mobj-y actor) (mobj-z actor))
    (let ([th (p-spawn-mobj (i- (mobj-x actor) (mobj-momx actor)) (i- (mobj-y actor) (mobj-momy actor))
                            (mobj-z actor) MT_SMOKE)])
      (mobj-momz-set! th FRACUNIT)
      (mobj-tics-set! th (fx- (mobj-tics th) (fxand (p-random) 3)))
      (when (fx< (mobj-tics th) 1)
        (mobj-tics-set! th 1)))
    ;; adjust direction
    (let ([dest (mobj-tracer actor)])
      (when (and dest (fx> (mobj-health dest) 0))
        ;; change angle
        (let ([exact (r-point-to-angle2 (mobj-x actor) (mobj-y actor) (mobj-x dest) (mobj-y dest))])
          (unless (fx= exact (mobj-angle actor))
            (if (fx> (angle- exact (mobj-angle actor)) #x80000000)
                (begin
                  (mobj-angle-set! actor (angle- (mobj-angle actor) TRACEANGLE))
                  (when (fx< (angle- exact (mobj-angle actor)) #x80000000)
                    (mobj-angle-set! actor exact)))
                (begin
                  (mobj-angle-set! actor (angle+ (mobj-angle actor) TRACEANGLE))
                  (when (fx> (angle- exact (mobj-angle actor)) #x80000000)
                    (mobj-angle-set! actor exact))))))
        (let ([exact (fxsrl (mobj-angle actor) ANGLETOFINESHIFT)]
              [speed (mobjinfo-speed (mobj-info actor))])
          (mobj-momx-set! actor (fixed-mul speed (finecosine exact)))
          (mobj-momy-set! actor (fixed-mul speed (fxvector-ref finesine exact)))
          ;; change slope
          (let* ([dist (fxquotient (p-aprox-distance (i- (mobj-x dest) (mobj-x actor))
                                                     (i- (mobj-y dest) (mobj-y actor)))
                                   speed)]
                 [dist (if (fx< dist 1) 1 dist)]
                 [slope (fxquotient (i- (i+ (mobj-z dest) (* 40 FRACUNIT)) (mobj-z actor)) dist)])
            (if (fx< slope (mobj-momz actor))
                (mobj-momz-set! actor (i- (mobj-momz actor) (fxquotient FRACUNIT 8)))
                (mobj-momz-set! actor (i+ (mobj-momz actor) (fxquotient FRACUNIT 8))))))))))

(define (a-skel-whoosh actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (s-start-sound actor sfx_skeswg)))

(define (a-skel-fist actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (when (p-check-melee-range actor)
      (let ([damage (fx* (fx+ (fxremainder (p-random) 10) 1) 6)])
        (s-start-sound actor sfx_skepch)
        (p-damage-mobj (mobj-target actor) actor actor damage)))))

;; PIT_VileCheck: detect a corpse that could be raised.
(define corpsehit #f)
(define vileobj #f)
(define viletryx 0)
(define viletryy 0)

(define (pit-vile-check thing)
  (cond
    [(not (fxlogtest (mobj-flags thing) MF_CORPSE)) #t] ; not a monster
    [(not (fx= (mobj-tics thing) -1)) #t]              ; not lying still yet
    [(fx= (mobjinfo-raisestate (mobj-info thing)) S_NULL) #t] ; monster doesn't have a raise state
    [(let ([maxdist (fx+ (mobjinfo-radius (mobj-info thing))
                         (mobjinfo-radius (vector-ref mobjinfo-table MT_VILE)))])
       (or (fx> (c-abs (i- (mobj-x thing) viletryx)) maxdist)
           (fx> (c-abs (i- (mobj-y thing) viletryy)) maxdist)))
     #t]                                               ; not actually touching
    [else
     (set! corpsehit thing)
     (mobj-momx-set! corpsehit 0)
     (mobj-momy-set! corpsehit 0)
     (mobj-height-set! corpsehit (int32 (fxsll (mobj-height corpsehit) 2)))
     (let ([check (p-check-position corpsehit (mobj-x corpsehit) (mobj-y corpsehit))])
       (mobj-height-set! corpsehit (fxsra (mobj-height corpsehit) 2))
       ;; doesn't fit here / got one, so stop checking
       (not check))]))

;; A_VileChase: check for ressurecting a body.
(define (a-vile-chase actor)
  (unless
      (and (not (fx= (mobj-movedir actor) DI_NODIR))
           ;; check for corpses to raise
           (let* ([speed (mobjinfo-speed (mobj-info actor))]
                  [dir (mobj-movedir actor)])
             (set! viletryx (i+ (mobj-x actor) (int32 (fx* speed (fxvector-ref xspeed dir)))))
             (set! viletryy (i+ (mobj-y actor) (int32 (fx* speed (fxvector-ref yspeed dir)))))
             (let ([xl (fxsra (i- (i- viletryx bmaporgx) (* MAXRADIUS 2)) MAPBLOCKSHIFT)]
                   [xh (fxsra (i+ (i- viletryx bmaporgx) (* MAXRADIUS 2)) MAPBLOCKSHIFT)]
                   [yl (fxsra (i- (i- viletryy bmaporgy) (* MAXRADIUS 2)) MAPBLOCKSHIFT)]
                   [yh (fxsra (i+ (i- viletryy bmaporgy) (* MAXRADIUS 2)) MAPBLOCKSHIFT)])
               (set! vileobj actor)
               ;; Call PIT_VileCheck to check whether object is a corpse
               ;; that can be raised.
               (and (not (blocks-every xl xh yl yh p-block-things-iterator pit-vile-check))
                    ;; got one!
                    (let ([temp (mobj-target actor)])
                      (mobj-target-set! actor corpsehit)
                      (a-face-target actor)
                      (mobj-target-set! actor temp)
                      (p-set-mobj-state actor S_VILE_HEAL1)
                      (s-start-sound corpsehit sfx_slop)
                      (let ([info (mobj-info corpsehit)])
                        (p-set-mobj-state corpsehit (mobjinfo-raisestate info))
                        (mobj-height-set! corpsehit (int32 (fxsll (mobj-height corpsehit) 2)))
                        (mobj-flags-set! corpsehit (mobjinfo-flags info))
                        (mobj-health-set! corpsehit (mobjinfo-spawnhealth info))
                        (mobj-target-set! corpsehit #f))
                      #t)))))
    ;; Return to normal attack.
    (a-chase actor)))

;; A_VileStart
(define (a-vile-start actor)
  (s-start-sound actor sfx_vilatk))

;; A_Fire: keep fire in front of player unless out of sight.
(define (a-start-fire actor)
  (s-start-sound actor sfx_flamst)
  (a-fire actor))

(define (a-fire-crackle actor)
  (s-start-sound actor sfx_flame)
  (a-fire actor))

(define (a-fire actor)
  (let ([dest (mobj-tracer actor)])
    ;; don't move it if the vile lost sight
    (when (and dest (p-check-sight (mobj-target actor) dest))
      (let ([an (fxsrl (mobj-angle dest) ANGLETOFINESHIFT)])
        (p-unset-thing-position actor)
        (mobj-x-set! actor (i+ (mobj-x dest) (fixed-mul (* 24 FRACUNIT) (finecosine an))))
        (mobj-y-set! actor (i+ (mobj-y dest) (fixed-mul (* 24 FRACUNIT) (fxvector-ref finesine an))))
        (mobj-z-set! actor (mobj-z dest))
        (p-set-thing-position actor)))))

;; A_VileTarget: spawn the hellfire. (C passes target->x for y too.)
(define (a-vile-target actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (let ([fog (p-spawn-mobj (mobj-x (mobj-target actor)) (mobj-x (mobj-target actor))
                             (mobj-z (mobj-target actor)) MT_FIRE)])
      (mobj-tracer-set! actor fog)
      (mobj-target-set! fog actor)
      (mobj-tracer-set! fog (mobj-target actor))
      (a-fire fog))))

;; A_VileAttack
(define (a-vile-attack actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (when (p-check-sight actor (mobj-target actor))
      (s-start-sound actor sfx_barexp)
      (p-damage-mobj (mobj-target actor) actor actor 20)
      (mobj-momz-set! (mobj-target actor)
                      (fxquotient (* 1000 FRACUNIT) (mobjinfo-mass (mobj-info (mobj-target actor)))))
      (let ([an (fxsrl (mobj-angle actor) ANGLETOFINESHIFT)]
            [fire (mobj-tracer actor)])
        (when fire
          ;; move the fire between the vile and the player
          (mobj-x-set! fire (i- (mobj-x (mobj-target actor)) (fixed-mul (* 24 FRACUNIT) (finecosine an))))
          (mobj-y-set! fire (i- (mobj-y (mobj-target actor)) (fixed-mul (* 24 FRACUNIT) (fxvector-ref finesine an))))
          (p-radius-attack fire actor 70))))))

;; Mancubus attack, firing three missiles (bruisers) in three different
;; directions? Doesn't look like it.
(define FATSPREAD (fxquotient ANG90 8))

(define (aim-missile! mo)
  (let ([an (fxsrl (mobj-angle mo) ANGLETOFINESHIFT)] [speed (mobjinfo-speed (mobj-info mo))])
    (mobj-momx-set! mo (fixed-mul speed (finecosine an)))
    (mobj-momy-set! mo (fixed-mul speed (fxvector-ref finesine an)))))

(define (a-fat-raise actor)
  (a-face-target actor)
  (s-start-sound actor sfx_manatk))

(define (a-fat-attack1 actor)
  (a-face-target actor)
  ;; Change direction to ...
  (mobj-angle-set! actor (angle+ (mobj-angle actor) FATSPREAD))
  (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)
  (let ([mo (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)])
    (mobj-angle-set! mo (angle+ (mobj-angle mo) FATSPREAD))
    (aim-missile! mo)))

(define (a-fat-attack2 actor)
  (a-face-target actor)
  ;; Now here choose opposite deviation.
  (mobj-angle-set! actor (angle- (mobj-angle actor) FATSPREAD))
  (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)
  (let ([mo (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)])
    (mobj-angle-set! mo (angle- (mobj-angle mo) (fx* FATSPREAD 2)))
    (aim-missile! mo)))

(define (a-fat-attack3 actor)
  (a-face-target actor)
  (let ([mo (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)])
    (mobj-angle-set! mo (angle- (mobj-angle mo) (fxquotient FATSPREAD 2)))
    (aim-missile! mo))
  (let ([mo (p-spawn-missile actor (mobj-target actor) MT_FATSHOT)])
    (mobj-angle-set! mo (angle+ (mobj-angle mo) (fxquotient FATSPREAD 2)))
    (aim-missile! mo)))

;; SkullAttack: fly at the player like a missile.
(define SKULLSPEED (* 20 FRACUNIT))

(define (a-skull-attack actor)
  (let ([dest (mobj-target actor)])
    (when dest
      (mobj-flags-set! actor (fxior (mobj-flags actor) MF_SKULLFLY))
      (s-start-sound actor (mobjinfo-attacksound (mobj-info actor)))
      (a-face-target actor)
      (let ([an (fxsrl (mobj-angle actor) ANGLETOFINESHIFT)])
        (mobj-momx-set! actor (fixed-mul SKULLSPEED (finecosine an)))
        (mobj-momy-set! actor (fixed-mul SKULLSPEED (fxvector-ref finesine an))))
      (let* ([dist (fxquotient (p-aprox-distance (i- (mobj-x dest) (mobj-x actor)) (i- (mobj-y dest) (mobj-y actor)))
                               SKULLSPEED)]
             [dist (if (fx< dist 1) 1 dist)])
        (mobj-momz-set! actor (fxquotient (i- (i+ (mobj-z dest) (fxsra (mobj-height dest) 1)) (mobj-z actor))
                                          dist))))))

;; A_PainShootSkull: spawn a lost soul and launch it at the target.
(define (a-pain-shoot-skull actor angle)
  ;; count total number of skull currently on the level; if there are
  ;; allready 20 skulls on the level, don't spit another one
  (unless (fx> (let loop ([th (thinker-next thinkercap)] [count 0])
                 (if (eq? th thinkercap)
                     count
                     (loop (thinker-next th)
                           (if (and (eq? (thinker-func th) p-mobj-thinker) (fx= (mobj-type th) MT_SKULL))
                               (fx+ count 1)
                               count))))
               20)
    ;; okay, there's playe for another one
    (let* ([an (fxsrl angle ANGLETOFINESHIFT)]
           [prestep (fx+ (* 4 FRACUNIT)
                         (fxquotient (fx* 3 (fx+ (mobjinfo-radius (mobj-info actor))
                                                 (mobjinfo-radius (vector-ref mobjinfo-table MT_SKULL))))
                                     2))]
           [x (i+ (mobj-x actor) (fixed-mul prestep (finecosine an)))]
           [y (i+ (mobj-y actor) (fixed-mul prestep (fxvector-ref finesine an)))]
           [z (i+ (mobj-z actor) (* 8 FRACUNIT))]
           [newmobj (p-spawn-mobj x y z MT_SKULL)])
      ;; Check for movements.
      (if (not (p-try-move newmobj (mobj-x newmobj) (mobj-y newmobj)))
          ;; kill it immediately
          (p-damage-mobj newmobj actor actor 10000)
          (begin
            (mobj-target-set! newmobj (mobj-target actor))
            (a-skull-attack newmobj))))))

;; A_PainAttack: spawn a lost soul and launch it at the target.
(define (a-pain-attack actor)
  (when (mobj-target actor)
    (a-face-target actor)
    (a-pain-shoot-skull actor (mobj-angle actor))))

(define (a-pain-die actor)
  (a-fall actor)
  (a-pain-shoot-skull actor (angle+ (mobj-angle actor) ANG90))
  (a-pain-shoot-skull actor (angle+ (mobj-angle actor) ANG180))
  (a-pain-shoot-skull actor (angle+ (mobj-angle actor) ANG270)))

(define (a-scream actor)
  (let ([deathsound (mobjinfo-deathsound (mobj-info actor))])
    (unless (fx= deathsound 0)
      (let ([sound (cond
                     [(memv deathsound (list sfx_podth1 sfx_podth2 sfx_podth3))
                      (fx+ sfx_podth1 (fxremainder (p-random) 3))]
                     [(memv deathsound (list sfx_bgdth1 sfx_bgdth2))
                      (fx+ sfx_bgdth1 (fxremainder (p-random) 2))]
                     [else deathsound])])
        ;; Check for bosses.
        (if (or (fx= (mobj-type actor) MT_SPIDER) (fx= (mobj-type actor) MT_CYBORG))
            ;; full volume
            (s-start-sound #f sound)
            (s-start-sound actor sound))))))

(define (a-xscream actor)
  (s-start-sound actor sfx_slop))

(define (a-pain actor)
  (unless (fx= (mobjinfo-painsound (mobj-info actor)) 0)
    (s-start-sound actor (mobjinfo-painsound (mobj-info actor)))))

(define (a-fall actor)
  ;; actor is on ground, it can be walked over. So change this if corpse
  ;; objects are meant to be obstacles.
  (mobj-flags-set! actor (fxand (mobj-flags actor) (fxnot MF_SOLID))))

;; A_Explode
(define (a-explode thingy)
  (p-radius-attack thingy (mobj-target thingy) 128))

;; A_BossDeath: possibly trigger special effects if on first boss level.
(define (a-boss-death mo)
  (let ([type (mobj-type mo)])
    (when (and (if (fx= gamemode commercial)
                   (and (fx= gamemap 7) (or (fx= type MT_FATSO) (fx= type MT_BABY)))
                   (case gameepisode
                     [(1) (and (fx= gamemap 8) (fx= type MT_BRUISER))]
                     [(2) (and (fx= gamemap 8) (fx= type MT_CYBORG))]
                     [(3) (and (fx= gamemap 8) (fx= type MT_SPIDER))]
                     [(4) (case gamemap
                            [(6) (fx= type MT_CYBORG)]
                            [(8) (fx= type MT_SPIDER)]
                            [else #f])]
                     [else (fx= gamemap 8)]))
               ;; make sure there is a player alive for victory
               (let loop ([i 0])
                 (and (fx< i MAXPLAYERS)
                      (or (and (vector-ref playeringame i) (fx> (player-health (vector-ref players i)) 0))
                          (loop (fx+ i 1)))))
               ;; scan the remaining thinkers to see if all bosses are dead
               (all-others-dead? mo))
      ;; victory!
      (cond
        [(and (fx= gamemode commercial) (fx= gamemap 7) (fx= type MT_FATSO))
         (ev-do-floor (tag-line 666) lowerFloorToLowest)]
        [(and (fx= gamemode commercial) (fx= gamemap 7) (fx= type MT_BABY))
         (ev-do-floor (tag-line 667) raiseToTexture)]
        [(and (not (fx= gamemode commercial)) (fx= gameepisode 1))
         (ev-do-floor (tag-line 666) lowerFloorToLowest)]
        [(and (not (fx= gamemode commercial)) (fx= gameepisode 4) (fx= gamemap 6))
         (ev-do-door (tag-line 666) blazeOpen)]
        [(and (not (fx= gamemode commercial)) (fx= gameepisode 4) (fx= gamemap 8))
         (ev-do-floor (tag-line 666) lowerFloorToLowest)]
        [else (g-exit-level)]))))

(define (a-hoof mo)
  (s-start-sound mo sfx_hoof)
  (a-chase mo))

(define (a-metal mo)
  (s-start-sound mo sfx_metal)
  (a-chase mo))

(define (a-baby-metal mo)
  (s-start-sound mo sfx_bspwlk)
  (a-chase mo))

;; (psprite actions, but defined here)
(define (a-open-shotgun2 player psp)
  (s-start-sound (player-mo player) sfx_dbopn))

(define (a-load-shotgun2 player psp)
  (s-start-sound (player-mo player) sfx_dbload))

(define (a-close-shotgun2 player psp)
  (s-start-sound (player-mo player) sfx_dbcls)
  (a-re-fire player psp))

;; DOOM II's boss brain.
(define braintargets (make-vector 32 #f))
(define numbraintargets 0)
(define braintargeton 0)

(define (a-brain-awake mo)
  ;; find all the target spots
  (set! numbraintargets 0)
  (set! braintargeton 0)
  (let loop ([th (thinker-next thinkercap)])
    (unless (eq? th thinkercap)
      (when (and (eq? (thinker-func th) p-mobj-thinker) (fx= (mobj-type th) MT_BOSSTARGET))
        (vector-set! braintargets numbraintargets th)
        (set! numbraintargets (fx+ numbraintargets 1)))
      (loop (thinker-next th))))
  (s-start-sound #f sfx_bossit))

(define (a-brain-pain mo)
  (s-start-sound #f sfx_bospn))

(define (brain-explosion x y)
  (let* ([z (fx+ 128 (fx* (p-random) (* 2 FRACUNIT)))]
         [th (p-spawn-mobj x y z MT_ROCKET)])
    (mobj-momz-set! th (fx* (p-random) 512))
    (p-set-mobj-state th S_BRAINEXPLODE1)
    (mobj-tics-set! th (fx- (mobj-tics th) (fxand (p-random) 7)))
    (when (fx< (mobj-tics th) 1)
      (mobj-tics-set! th 1))))

(define (a-brain-scream mo)
  (do ([x (i- (mobj-x mo) (* 196 FRACUNIT)) (i+ x (* FRACUNIT 8))])
      ((not (fx< x (i+ (mobj-x mo) (* 320 FRACUNIT)))))
    (brain-explosion x (i- (mobj-y mo) (* 320 FRACUNIT))))
  (s-start-sound #f sfx_bosdth))

(define (a-brain-explode mo)
  (brain-explosion (i+ (mobj-x mo) (int32 (fx* (p-random-diff) 2048))) (mobj-y mo)))

(define (a-brain-die mo)
  (g-exit-level))

(define brain-easy 0)
(define (a-brain-spit mo)
  (set! brain-easy (fxxor brain-easy 1))
  (unless (and (fx<= gameskill sk_easy) (fx= brain-easy 0))
    ;; shoot a cube at current target
    (let ([targ (vector-ref braintargets braintargeton)])
      (set! braintargeton (fxremainder (fx+ braintargeton 1) numbraintargets))
      ;; spawn brain missile
      (let ([newmobj (p-spawn-missile mo targ MT_SPAWNSHOT)])
        (mobj-target-set! newmobj targ)
        (mobj-reactiontime-set! newmobj
                                (fxquotient (fxquotient (i- (mobj-y targ) (mobj-y mo)) (mobj-momy newmobj))
                                            (state-tics (mobj-state newmobj))))
        (s-start-sound #f sfx_bospit)))))

;; travelling cube sound
(define (a-spawn-sound mo)
  (s-start-sound mo sfx_boscub)
  (a-spawn-fly mo))

(define (a-spawn-fly mo)
  (mobj-reactiontime-set! mo (fx- (mobj-reactiontime mo) 1))
  (when (fx= (mobj-reactiontime mo) 0)  ; (else still flying)
    (let* ([targ (mobj-target mo)]
           ;; First spawn teleport fog.
           [fog (p-spawn-mobj (mobj-x targ) (mobj-y targ) (mobj-z targ) MT_SPAWNFIRE)])
      (s-start-sound fog sfx_telept)
      ;; Randomly select monster to spawn. Probability distribution (kind
      ;; of :), decreasing likelihood.
      (let* ([r (p-random)]
             [type (cond [(fx< r 50) MT_TROOP] [(fx< r 90) MT_SERGEANT] [(fx< r 120) MT_SHADOWS]
                         [(fx< r 130) MT_PAIN] [(fx< r 160) MT_HEAD] [(fx< r 162) MT_VILE]
                         [(fx< r 172) MT_UNDEAD] [(fx< r 192) MT_BABY] [(fx< r 222) MT_FATSO]
                         [(fx< r 246) MT_KNIGHT] [else MT_BRUISER])]
             [newmobj (p-spawn-mobj (mobj-x targ) (mobj-y targ) (mobj-z targ) type)])
        (when (p-look-for-players newmobj #t)
          (p-set-mobj-state newmobj (mobjinfo-seestate (mobj-info newmobj))))
        ;; telefrag anything in this spot
        (p-teleport-move newmobj (mobj-x newmobj) (mobj-y newmobj))
        ;; remove self (i.e., cube).
        (p-remove-mobj mo)))))

(define (a-player-scream mo)
  ;; Default death sound. IF THE PLAYER DIES LESS THAN -50% WITHOUT
  ;; GIBBING (in DOOM II), the louder one.
  (s-start-sound mo (if (and (fx= gamemode commercial) (fx< (mobj-health mo) -50))
                        sfx_pdiehi
                        sfx_pldeth)))

(define p-enemy-actions
  `((A_Look . ,a-look) (A_Chase . ,a-chase) (A_FaceTarget . ,a-face-target)
    (A_PosAttack . ,a-pos-attack) (A_SPosAttack . ,a-spos-attack) (A_CPosAttack . ,a-cpos-attack)
    (A_CPosRefire . ,a-cpos-refire) (A_SpidRefire . ,a-spid-refire) (A_BspiAttack . ,a-bspi-attack)
    (A_TroopAttack . ,a-troop-attack) (A_SargAttack . ,a-sarg-attack) (A_HeadAttack . ,a-head-attack)
    (A_CyberAttack . ,a-cyber-attack) (A_BruisAttack . ,a-bruis-attack)
    (A_SkelMissile . ,a-skel-missile) (A_Tracer . ,a-tracer) (A_SkelWhoosh . ,a-skel-whoosh)
    (A_SkelFist . ,a-skel-fist) (A_VileChase . ,a-vile-chase) (A_VileStart . ,a-vile-start)
    (A_StartFire . ,a-start-fire) (A_FireCrackle . ,a-fire-crackle) (A_Fire . ,a-fire)
    (A_VileTarget . ,a-vile-target) (A_VileAttack . ,a-vile-attack) (A_FatRaise . ,a-fat-raise)
    (A_FatAttack1 . ,a-fat-attack1) (A_FatAttack2 . ,a-fat-attack2) (A_FatAttack3 . ,a-fat-attack3)
    (A_SkullAttack . ,a-skull-attack) (A_PainAttack . ,a-pain-attack) (A_PainDie . ,a-pain-die)
    (A_Scream . ,a-scream) (A_XScream . ,a-xscream) (A_Pain . ,a-pain) (A_Fall . ,a-fall)
    (A_Explode . ,a-explode) (A_BossDeath . ,a-boss-death) (A_KeenDie . ,a-keen-die)
    (A_Hoof . ,a-hoof) (A_Metal . ,a-metal) (A_BabyMetal . ,a-baby-metal)
    (A_OpenShotgun2 . ,a-open-shotgun2) (A_LoadShotgun2 . ,a-load-shotgun2)
    (A_CloseShotgun2 . ,a-close-shotgun2) (A_BrainAwake . ,a-brain-awake)
    (A_BrainPain . ,a-brain-pain) (A_BrainScream . ,a-brain-scream)
    (A_BrainExplode . ,a-brain-explode) (A_BrainDie . ,a-brain-die) (A_BrainSpit . ,a-brain-spit)
    (A_SpawnSound . ,a-spawn-sound) (A_SpawnFly . ,a-spawn-fly) (A_PlayerScream . ,a-player-scream)))
