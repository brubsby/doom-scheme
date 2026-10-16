;;; p_inter.c: item pickups, damage and death. Included by p_local.sls.

(define BONUSADD 6)

;; a weapon is found with two clip loads, a big item has five clip loads.
;; (maxammo[] is in doomstat.)
(define clipammo (fxvector 10 4 20 1))

(define (player-index p)
  (let loop ([i 0])
    (if (eq? (vector-ref players i) p) i (loop (fx+ i 1)))))

;; GET STUFF

;; P_GiveAmmo: num is the number of clip loads, not the individual count
;; (0= 1/2 clip). Returns false if the ammo can't be picked up at all.
(define (p-give-ammo player ammo num)
  (cond
    [(fx= ammo am_noammo) #f]
    ;; (C's check is ammo > NUMAMMO, one too lenient)
    [(or (fx< ammo 0) (fx> ammo NUMAMMO)) (i-error "P_GiveAmmo: bad type ~a" ammo)]
    [(fx= (fxvector-ref (player-ammo player) ammo) (fxvector-ref (player-maxammo player) ammo)) #f]
    [else
     (let* ([num (if (fx= num 0)
                     (fxquotient (fxvector-ref clipammo ammo) 2)
                     (fx* num (fxvector-ref clipammo ammo)))]
            ;; give double ammo in trainer mode, you'll need in nightmare
            [num (if (or (fx= gameskill sk_baby) (fx= gameskill sk_nightmare)) (fxsll num 1) num)]
            [oldammo (fxvector-ref (player-ammo player) ammo)])
       (fxvector-set! (player-ammo player) ammo
                      (fxmin (fx+ oldammo num) (fxvector-ref (player-maxammo player) ammo)))
       ;; If non zero ammo, don't change up weapons, player was lower on
       ;; purpose.
       (when (fx= oldammo 0)
         ;; We were down to zero, so select a new weapon. Preferences are
         ;; not user selectable.
         (let ([ready (player-readyweapon player)] [owned (player-weaponowned player)])
           (cond
             [(fx= ammo am_clip)
              (when (fx= ready wp_fist)
                (player-pendingweapon-set! player (if (vector-ref owned wp_chaingun) wp_chaingun wp_pistol)))]
             [(fx= ammo am_shell)
              (when (and (or (fx= ready wp_fist) (fx= ready wp_pistol)) (vector-ref owned wp_shotgun))
                (player-pendingweapon-set! player wp_shotgun))]
             [(fx= ammo am_cell)
              (when (and (or (fx= ready wp_fist) (fx= ready wp_pistol)) (vector-ref owned wp_plasma))
                (player-pendingweapon-set! player wp_plasma))]
             [(fx= ammo am_misl)
              (when (and (fx= ready wp_fist) (vector-ref owned wp_missile))
                (player-pendingweapon-set! player wp_missile))])))
       #t)]))

;; P_GiveWeapon: the weapon name may have a MF_DROPPED flag ored in.
(define (p-give-weapon player weapon dropped)
  (let ([ammo (weaponinfo-ammo (vector-ref weaponinfo-table weapon))]
        [owned (player-weaponowned player)])
    (if (and netgame (not (fx= deathmatch 2)) (not dropped))
        ;; leave placed weapons forever on net games
        (begin
          (unless (vector-ref owned weapon)
            (player-bonuscount-set! player (fx+ (player-bonuscount player) BONUSADD))
            (vector-set! owned weapon #t)
            (p-give-ammo player ammo (if (fx= deathmatch 0) 2 5))
            (player-pendingweapon-set! player weapon)
            (when (eq? player (vector-ref players consoleplayer))
              (s-start-sound #f sfx_wpnup)))
          #f)
        (let ([gaveammo (and (not (fx= ammo am_noammo))
                             ;; give one clip with a dropped weapon, two
                             ;; clips with a found weapon
                             (p-give-ammo player ammo (if dropped 1 2)))]
              [gaveweapon (and (not (vector-ref owned weapon))
                               (begin
                                 (vector-set! owned weapon #t)
                                 (player-pendingweapon-set! player weapon)
                                 #t))])
          (or gaveweapon gaveammo)))))

;; P_GiveBody: returns false if the body isn't needed at all.
(define (p-give-body player num)
  (and (fx< (player-health player) MAXHEALTH)
       (begin
         (player-health-set! player (fxmin (fx+ (player-health player) num) MAXHEALTH))
         (mobj-health-set! (player-mo player) (player-health player))
         #t)))

;; P_GiveArmor: returns false if the armor is worse than the current armor.
(define (p-give-armor player armortype)
  (let ([hits (fx* armortype 100)])
    (and (fx< (player-armorpoints player) hits) ; don't pick up
         (begin
           (player-armortype-set! player armortype)
           (player-armorpoints-set! player hits)
           #t))))

;; P_GiveCard
(define (p-give-card player card)
  (unless (vector-ref (player-cards player) card)
    (player-bonuscount-set! player BONUSADD)
    (vector-set! (player-cards player) card #t)))

;; P_GivePower
(define (p-give-power player power)
  (let ([powers (player-powers player)])
    (cond
      [(fx= power pw_invulnerability) (fxvector-set! powers power INVULNTICS) #t]
      [(fx= power pw_invisibility)
       (fxvector-set! powers power INVISTICS)
       (mobj-flags-set! (player-mo player) (fxior (mobj-flags (player-mo player)) MF_SHADOW))
       #t]
      [(fx= power pw_infrared) (fxvector-set! powers power INFRATICS) #t]
      [(fx= power pw_ironfeet) (fxvector-set! powers power IRONTICS) #t]
      [(fx= power pw_strength)
       (p-give-body player 100)
       (fxvector-set! powers power 1)
       #t]
      [(not (fx= (fxvector-ref powers power) 0)) #f] ; already got it
      [else (fxvector-set! powers power 1) #t])))

;; P_TouchSpecialThing
(define (p-touch-special-thing special toucher)
  (let ([delta (i- (mobj-z special) (mobj-z toucher))])
    (unless (or (fx> delta (mobj-height toucher)) (fx< delta (* -8 FRACUNIT)) ; out of reach
                ;; Dead thing touching. Can happen with a sliding player
                ;; corpse.
                (fx<= (mobj-health toucher) 0))
      (let* ([player (mobj-player toucher)]
             [sound sfx_itemup]
             [message (lambda (m) (player-message-set! player m))]
             [ammo (lambda (type num msg)
                     (and (p-give-ammo player type num) (begin (message msg) #t)))]
             [weapon (lambda (w dropped msg)
                       (and (p-give-weapon player w dropped)
                            (begin (message msg) (set! sound sfx_wpnup) #t)))]
             [power (lambda (pw msg)
                      (and (p-give-power player pw)
                           (begin (message msg) (set! sound sfx_getpow) #t)))]
             ;; Cards are left for everyone in a netgame.
             [card (lambda (c msg)
                     (unless (vector-ref (player-cards player) c)
                       (message msg))
                     (p-give-card player c)
                     (not netgame))]
             [dropped (fxlogtest (mobj-flags special) MF_DROPPED)]
             [s (mobj-sprite special)]
             ;; Identify by sprite. #f means don't pick it up.
             [taken
              (cond
                ;; armor
                [(fx= s SPR_ARM1) (and (p-give-armor player 1) (begin (message GOTARMOR) #t))]
                [(fx= s SPR_ARM2) (and (p-give-armor player 2) (begin (message GOTMEGA) #t))]
                ;; bonus items
                [(fx= s SPR_BON1)
                 ;; can go over 100%
                 (player-health-set! player (fxmin (fx+ (player-health player) 1) 200))
                 (mobj-health-set! (player-mo player) (player-health player))
                 (message GOTHTHBONUS)
                 #t]
                [(fx= s SPR_BON2)
                 ;; can go over 100%
                 (player-armorpoints-set! player (fxmin (fx+ (player-armorpoints player) 1) 200))
                 (when (fx= (player-armortype player) 0)
                   (player-armortype-set! player 1))
                 (message GOTARMBONUS)
                 #t]
                [(fx= s SPR_SOUL)
                 (player-health-set! player (fxmin (fx+ (player-health player) 100) 200))
                 (mobj-health-set! (player-mo player) (player-health player))
                 (message GOTSUPER)
                 (set! sound sfx_getpow)
                 #t]
                [(fx= s SPR_MEGA)
                 (and (fx= gamemode commercial)
                      (begin
                        (player-health-set! player 200)
                        (mobj-health-set! (player-mo player) (player-health player))
                        (p-give-armor player 2)
                        (message GOTMSPHERE)
                        (set! sound sfx_getpow)
                        #t))]
                ;; cards
                [(fx= s SPR_BKEY) (card it_bluecard GOTBLUECARD)]
                [(fx= s SPR_YKEY) (card it_yellowcard GOTYELWCARD)]
                [(fx= s SPR_RKEY) (card it_redcard GOTREDCARD)]
                [(fx= s SPR_BSKU) (card it_blueskull GOTBLUESKUL)]
                [(fx= s SPR_YSKU) (card it_yellowskull GOTYELWSKUL)]
                [(fx= s SPR_RSKU) (card it_redskull GOTREDSKULL)]
                ;; medikits, heals
                [(fx= s SPR_STIM) (and (p-give-body player 10) (begin (message GOTSTIM) #t))]
                [(fx= s SPR_MEDI)
                 (and (p-give-body player 25)
                      (begin
                        ;; (health was just raised, so this never says "need")
                        (message (if (fx< (player-health player) 25) GOTMEDINEED GOTMEDIKIT))
                        #t))]
                ;; power ups
                [(fx= s SPR_PINV) (power pw_invulnerability GOTINVUL)]
                [(fx= s SPR_PSTR)
                 (and (power pw_strength GOTBERSERK)
                      (begin
                        (unless (fx= (player-readyweapon player) wp_fist)
                          (player-pendingweapon-set! player wp_fist))
                        #t))]
                [(fx= s SPR_PINS) (power pw_invisibility GOTINVIS)]
                [(fx= s SPR_SUIT) (power pw_ironfeet GOTSUIT)]
                [(fx= s SPR_PMAP) (power pw_allmap GOTMAP)]
                [(fx= s SPR_PVIS) (power pw_infrared GOTVISOR)]
                ;; ammo
                [(fx= s SPR_CLIP) (ammo am_clip (if dropped 0 1) GOTCLIP)]
                [(fx= s SPR_AMMO) (ammo am_clip 5 GOTCLIPBOX)]
                [(fx= s SPR_ROCK) (ammo am_misl 1 GOTROCKET)]
                [(fx= s SPR_BROK) (ammo am_misl 5 GOTROCKBOX)]
                [(fx= s SPR_CELL) (ammo am_cell 1 GOTCELL)]
                [(fx= s SPR_CELP) (ammo am_cell 5 GOTCELLBOX)]
                [(fx= s SPR_SHEL) (ammo am_shell 1 GOTSHELLS)]
                [(fx= s SPR_SBOX) (ammo am_shell 5 GOTSHELLBOX)]
                [(fx= s SPR_BPAK)
                 (unless (player-backpack player)
                   (do ([i 0 (fx+ i 1)]) ((fx= i NUMAMMO))
                     (fxvector-set! (player-maxammo player) i (fx* 2 (fxvector-ref (player-maxammo player) i))))
                   (player-backpack-set! player #t))
                 (do ([i 0 (fx+ i 1)]) ((fx= i NUMAMMO))
                   (p-give-ammo player i 1))
                 (message GOTBACKPACK)
                 #t]
                ;; weapons
                [(fx= s SPR_BFUG) (weapon wp_bfg #f GOTBFG9000)]
                [(fx= s SPR_MGUN) (weapon wp_chaingun dropped GOTCHAINGUN)]
                [(fx= s SPR_CSAW) (weapon wp_chainsaw #f GOTCHAINSAW)]
                [(fx= s SPR_LAUN) (weapon wp_missile #f GOTLAUNCHER)]
                [(fx= s SPR_PLAS) (weapon wp_plasma #f GOTPLASMA)]
                [(fx= s SPR_SHOT) (weapon wp_shotgun dropped GOTSHOTGUN)]
                [(fx= s SPR_SGN2) (weapon wp_supershotgun dropped GOTSHOTGUN2)]
                [else (i-error "P_SpecialThing: Unknown gettable thing")])])
        (when taken
          (when (fxlogtest (mobj-flags special) MF_COUNTITEM)
            (player-itemcount-set! player (fx+ (player-itemcount player) 1)))
          (p-remove-mobj special)
          (player-bonuscount-set! player (fx+ (player-bonuscount player) BONUSADD))
          (when (eq? player (vector-ref players consoleplayer))
            (s-start-sound #f sound)))))))

;; KillMobj
(define (p-kill-mobj source target)
  (mobj-flags-set! target (fxand (mobj-flags target) (fxnot (fxior MF_SHOOTABLE MF_FLOAT MF_SKULLFLY))))
  (unless (fx= (mobj-type target) MT_SKULL)
    (mobj-flags-set! target (fxand (mobj-flags target) (fxnot MF_NOGRAVITY))))
  (mobj-flags-set! target (fxior (mobj-flags target) MF_CORPSE MF_DROPOFF))
  (mobj-height-set! target (fxsra (mobj-height target) 2))
  (cond
    [(and source (mobj-player source))
     ;; count for intermission
     (when (fxlogtest (mobj-flags target) MF_COUNTKILL)
       (player-killcount-set! (mobj-player source) (fx+ (player-killcount (mobj-player source)) 1)))
     (when (mobj-player target)
       (let ([frags (player-frags (mobj-player source))] [i (player-index (mobj-player target))])
         (fxvector-set! frags i (fx+ (fxvector-ref frags i) 1))))]
    [(and (not netgame) (fxlogtest (mobj-flags target) MF_COUNTKILL))
     ;; count all monster deaths, even those caused by other monsters
     (player-killcount-set! (vector-ref players 0) (fx+ (player-killcount (vector-ref players 0)) 1))])
  (let ([tp (mobj-player target)])
    (when tp
      ;; count environment kills against you
      (unless source
        (let ([frags (player-frags tp)] [i (player-index tp)])
          (fxvector-set! frags i (fx+ (fxvector-ref frags i) 1))))
      (mobj-flags-set! target (fxand (mobj-flags target) (fxnot MF_SOLID)))
      (player-playerstate-set! tp PST_DEAD)
      (p-drop-weapon tp)
      (when (and (eq? tp (vector-ref players consoleplayer)) automapactive)
        ;; don't die in auto map, switch view prior to dying
        (am-stop-hook))))
  (let ([info (mobj-info target)])
    (if (and (fx< (mobj-health target) (fx- 0 (mobjinfo-spawnhealth info)))
             (not (fx= (mobjinfo-xdeathstate info) 0)))
        (p-set-mobj-state target (mobjinfo-xdeathstate info))
        (p-set-mobj-state target (mobjinfo-deathstate info))))
  (mobj-tics-set! target (fx- (mobj-tics target) (fxand (p-random) 3)))
  (when (fx< (mobj-tics target) 1)
    (mobj-tics-set! target 1))
  ;; Drop stuff. This determines the kind of object spawned during the
  ;; death frame of a thing.
  (let ([item (let ([t (mobj-type target)])
                (cond [(or (fx= t MT_WOLFSS) (fx= t MT_POSSESSED)) MT_CLIP]
                      [(fx= t MT_SHOTGUY) MT_SHOTGUN]
                      [(fx= t MT_CHAINGUY) MT_CHAINGUN]
                      [else #f]))])
    (when item
      (let ([mo (p-spawn-mobj (mobj-x target) (mobj-y target) ONFLOORZ item)])
        (mobj-flags-set! mo (fxior (mobj-flags mo) MF_DROPPED)))))) ; special versions of items

;; P_DamageMobj: damages both enemies and players. "inflictor" is the
;; thing that caused the damage, creature or missile, can be NULL (slime,
;; etc). "source" is the thing to target after taking damage, creature or
;; NULL. Source and inflictor are the same for melee attacks. Source can be
;; NULL for slime, barrel explosions and other environmental stuff.
(define (p-damage-mobj target inflictor source damage)
  (when (and (fxlogtest (mobj-flags target) MF_SHOOTABLE) ; shouldn't happen...
             (fx> (mobj-health target) 0))
    (when (fxlogtest (mobj-flags target) MF_SKULLFLY)
      (mobj-momx-set! target 0)
      (mobj-momy-set! target 0)
      (mobj-momz-set! target 0))
    (let* ([player (mobj-player target)]
           ;; take half damage in trainer mode
           [damage (if (and player (fx= gameskill sk_baby)) (fxsra damage 1) damage)])
      ;; Some close combat weapons should not inflict thrust and push the
      ;; victim out of reach, thus kick away unless using the chainsaw.
      (when (and inflictor
                 (not (fxlogtest (mobj-flags target) MF_NOCLIP))
                 (or (not source)
                     (not (mobj-player source))
                     (not (fx= (player-readyweapon (mobj-player source)) wp_chainsaw))))
        (let* ([ang (r-point-to-angle2 (mobj-x inflictor) (mobj-y inflictor) (mobj-x target) (mobj-y target))]
               [thrust (fxquotient (int32 (fx* (int32 (fx* damage (fxsra FRACUNIT 3))) 100))
                                   (mobjinfo-mass (mobj-info target)))])
          ;; make fall forwards sometimes
          (let-values ([(ang thrust)
                        (if (and (fx< damage 40)
                                 (fx> damage (mobj-health target))
                                 (fx> (i- (mobj-z target) (mobj-z inflictor)) (* 64 FRACUNIT))
                                 (fxlogtest (p-random) 1))
                            (values (angle+ ang ANG180) (int32 (fx* thrust 4)))
                            (values ang thrust))])
            (let ([ang (fxsrl ang ANGLETOFINESHIFT)])
              (mobj-momx-set! target (i+ (mobj-momx target) (fixed-mul thrust (finecosine ang))))
              (mobj-momy-set! target (i+ (mobj-momy target) (fixed-mul thrust (fxvector-ref finesine ang))))))))
      (let ([damage
             (if (not player)
                 damage
                 ;; player specific
                 (let ([damage
                        ;; end of game hell hack
                        (if (and (fx= (sector-special (subsector-sector (mobj-subsector target))) 11)
                                 (fx>= damage (mobj-health target)))
                            (fx- (mobj-health target) 1)
                            damage)])
                   ;; Below certain threshold, ignore damage in GOD mode, or
                   ;; with INVUL power.
                   (if (and (fx< damage 1000)
                            (or (fxlogtest (player-cheats player) CF_GODMODE)
                                (not (fx= (fxvector-ref (player-powers player) pw_invulnerability) 0))))
                       #f
                       (let ([damage
                              (if (fx= (player-armortype player) 0)
                                  damage
                                  (let* ([saved (if (fx= (player-armortype player) 1)
                                                    (fxquotient damage 3)
                                                    (fxquotient damage 2))]
                                         [saved (if (fx<= (player-armorpoints player) saved)
                                                    ;; armor is used up
                                                    (let ([s (player-armorpoints player)])
                                                      (player-armortype-set! player 0)
                                                      s)
                                                    saved)])
                                    (player-armorpoints-set! player (fx- (player-armorpoints player) saved))
                                    (fx- damage saved)))])
                         ;; mirror mobj health here for Dave
                         (player-health-set! player (fxmax (fx- (player-health player) damage) 0))
                         (player-attacker-set! player source)
                         ;; add damage after armor / invuln. (teleport stomp
                         ;; does 10k points...)
                         (player-damagecount-set! player (fxmin (fx+ (player-damagecount player) damage) 100))
                         ;; (I_Tactile: no force feedback here)
                         damage))))])
        (when damage
          ;; do the damage
          (mobj-health-set! target (fx- (mobj-health target) damage))
          (if (fx<= (mobj-health target) 0)
              (p-kill-mobj source target)
              (let ([info (mobj-info target)])
                (when (and (fx< (p-random) (mobjinfo-painchance info))
                           (not (fxlogtest (mobj-flags target) MF_SKULLFLY)))
                  ;; fight back!
                  (mobj-flags-set! target (fxior (mobj-flags target) MF_JUSTHIT))
                  (p-set-mobj-state target (mobjinfo-painstate info)))
                ;; we're awake now...
                (mobj-reactiontime-set! target 0)
                (when (and (or (fx= (mobj-threshold target) 0) (fx= (mobj-type target) MT_VILE))
                           source
                           (not (eq? source target))
                           (not (fx= (mobj-type source) MT_VILE)))
                  ;; if not intent on another player, chase after this one
                  (mobj-target-set! target source)
                  (mobj-threshold-set! target BASETHRESHOLD)
                  (when (and (eq? (mobj-state target) (vector-ref states (mobjinfo-spawnstate info)))
                             (not (fx= (mobjinfo-seestate info) S_NULL)))
                    (p-set-mobj-state target (mobjinfo-seestate info)))))))))))
