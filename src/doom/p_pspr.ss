;;; p_pspr.c: weapon sprites and weapon attacks. Included by p_local.sls.
;;; Psprite actions take (player psp); the BFG spray, an mobj state's
;;; action, takes (mobj).

(define LOWERSPEED (* FRACUNIT 6))
(define RAISESPEED (* FRACUNIT 6))
(define WEAPONBOTTOM (* 128 FRACUNIT))
(define WEAPONTOP (* 32 FRACUNIT))

;; plasma cells for a bfg attack
(define BFGCELLS 40)

(define (weapon-of player) (vector-ref weaponinfo-table (player-readyweapon player)))

;; P_SetPsprite
(define (p-set-psprite player position stnum)
  (let ([psp (vector-ref (player-psprites player) position)])
    (let loop ([stnum stnum])
      (if (fx= stnum 0)
          ;; object removed itself
          (pspdef-state-set! psp #f)
          (let ([state (vector-ref states stnum)])
            (pspdef-state-set! psp state)
            (pspdef-tics-set! psp (state-tics state)) ; could be 0
            (unless (fx= (state-misc1 state) 0)
              ;; coordinate set
              (pspdef-sx-set! psp (fxsll (state-misc1 state) FRACBITS))
              (pspdef-sy-set! psp (fxsll (state-misc2 state) FRACBITS)))
            ;; Call action routine. Modified handling.
            (let ([action (state-action state)])
              (when action (action player psp)))
            ;; (an initial state of 0 could cycle through)
            (when (and (pspdef-state psp) (fx= (pspdef-tics psp) 0))
              (loop (state-nextstate (pspdef-state psp)))))))))

;; P_CalcSwing
(define swingx 0)
(define swingy 0)
(define (p-calc-swing player)
  ;; OPTIMIZE: tablify this. A LUT would allow for different modes, and
  ;; add flexibility.
  (let ([swing (player-bob player)])
    (set! swingx (fixed-mul swing (fxvector-ref finesine (fxand (fx* (fxquotient FINEANGLES 70) leveltime) FINEMASK))))
    (set! swingy (ineg (fixed-mul swingx (fxvector-ref finesine
                                                       (fxand (fx+ (fx* (fxquotient FINEANGLES 70) leveltime)
                                                                   (fxquotient FINEANGLES 2))
                                                              FINEMASK)))))))

;; P_BringUpWeapon: starts bringing the pending weapon up from the bottom
;; of the screen.
(define (p-bring-up-weapon player)
  (when (fx= (player-pendingweapon player) wp_nochange)
    (player-pendingweapon-set! player (player-readyweapon player)))
  (when (fx= (player-pendingweapon player) wp_chainsaw)
    (s-start-sound (player-mo player) sfx_sawup))
  (let ([newstate (weaponinfo-upstate (vector-ref weaponinfo-table (player-pendingweapon player)))])
    (player-pendingweapon-set! player wp_nochange)
    (pspdef-sy-set! (vector-ref (player-psprites player) ps_weapon) WEAPONBOTTOM)
    (p-set-psprite player ps_weapon newstate)))

;; P_CheckAmmo: returns true if there is enough ammo to shoot. If not,
;; selects the next weapon to use.
(define (p-check-ammo player)
  (let* ([ready (player-readyweapon player)]
         [ammo (weaponinfo-ammo (vector-ref weaponinfo-table ready))]
         ;; Minimal amount for one shot varies.
         [count (cond [(fx= ready wp_bfg) BFGCELLS]
                      [(fx= ready wp_supershotgun) 2] ; Double barrel.
                      [else 1])])                     ; Regular.
    ;; Some do not need ammunition anyway. Return if current ammunition
    ;; sufficient.
    (if (or (fx= ammo am_noammo) (fx>= (fxvector-ref (player-ammo player) ammo) count))
        #t
        (let ([owned (player-weaponowned player)] [ammos (player-ammo player)])
          ;; Out of ammo, pick a weapon to change to. Preferences are set
          ;; here. (C loops while wp_nochange, which can't happen.)
          (player-pendingweapon-set!
           player
           (cond
             [(and (vector-ref owned wp_plasma) (not (fx= (fxvector-ref ammos am_cell) 0))
                   (not (fx= gamemode shareware)))
              wp_plasma]
             [(and (vector-ref owned wp_supershotgun) (fx> (fxvector-ref ammos am_shell) 2)
                   (fx= gamemode commercial))
              wp_supershotgun]
             [(and (vector-ref owned wp_chaingun) (not (fx= (fxvector-ref ammos am_clip) 0))) wp_chaingun]
             [(and (vector-ref owned wp_shotgun) (not (fx= (fxvector-ref ammos am_shell) 0))) wp_shotgun]
             [(not (fx= (fxvector-ref ammos am_clip) 0)) wp_pistol]
             [(vector-ref owned wp_chainsaw) wp_chainsaw]
             [(and (vector-ref owned wp_missile) (not (fx= (fxvector-ref ammos am_misl) 0))) wp_missile]
             [(and (vector-ref owned wp_bfg) (fx> (fxvector-ref ammos am_cell) 40)
                   (not (fx= gamemode shareware)))
              wp_bfg]
             ;; If everything fails.
             [else wp_fist]))
          ;; Now set appropriate weapon overlay.
          (p-set-psprite player ps_weapon (weaponinfo-downstate (weapon-of player)))
          #f))))

;; P_FireWeapon.
(define (p-fire-weapon player)
  (when (p-check-ammo player)
    (p-set-mobj-state (player-mo player) S_PLAY_ATK1)
    (p-set-psprite player ps_weapon (weaponinfo-atkstate (weapon-of player)))
    (p-noise-alert (player-mo player) (player-mo player))))

;; P_DropWeapon: player died, so put the weapon away.
(define (p-drop-weapon player)
  (p-set-psprite player ps_weapon (weaponinfo-downstate (weapon-of player))))

;; A_WeaponReady: the player can fire the weapon or change to another
;; weapon at this time. Follows after getting weapon up, or after previous
;; attack/fire sequence.
(define (a-weapon-ready player psp)
  (let ([mo (player-mo player)])
    ;; get out of attack state
    (when (or (eq? (mobj-state mo) (vector-ref states S_PLAY_ATK1))
              (eq? (mobj-state mo) (vector-ref states S_PLAY_ATK2)))
      (p-set-mobj-state mo S_PLAY))
    (when (and (fx= (player-readyweapon player) wp_chainsaw)
               (eq? (pspdef-state psp) (vector-ref states S_SAW)))
      (s-start-sound mo sfx_sawidl))
    (cond
      ;; check for change: if player is dead, put the weapon away
      [(or (not (fx= (player-pendingweapon player) wp_nochange))
           (fx= (player-health player) 0))
       ;; change weapon (pending weapon should allready be validated)
       (p-set-psprite player ps_weapon (weaponinfo-downstate (weapon-of player)))]
      ;; check for fire: the missile launcher and bfg do not auto fire
      [(and (fxlogtest (ticcmd-buttons (player-cmd player)) BT_ATTACK)
            (or (not (player-attackdown player))
                (and (not (fx= (player-readyweapon player) wp_missile))
                     (not (fx= (player-readyweapon player) wp_bfg)))))
       (player-attackdown-set! player #t)
       (p-fire-weapon player)]
      [else
       (unless (fxlogtest (ticcmd-buttons (player-cmd player)) BT_ATTACK)
         (player-attackdown-set! player #f))
       ;; bob the weapon based on movement speed
       (let ([angle (fxand (fx* 128 leveltime) FINEMASK)])
         (pspdef-sx-set! psp (i+ FRACUNIT (fixed-mul (player-bob player) (finecosine angle))))
         (let ([angle (fxand angle (fx- (fxquotient FINEANGLES 2) 1))])
           (pspdef-sy-set! psp (i+ WEAPONTOP (fixed-mul (player-bob player) (fxvector-ref finesine angle))))))])))

;; A_ReFire: the player can re-fire the weapon without lowering it
;; entirely.
(define (a-re-fire player psp)
  ;; check for fire (if a weaponchange is pending, let it go through
  ;; instead)
  (if (and (fxlogtest (ticcmd-buttons (player-cmd player)) BT_ATTACK)
           (fx= (player-pendingweapon player) wp_nochange)
           (not (fx= (player-health player) 0)))
      (begin
        (player-refire-set! player (fx+ (player-refire player) 1))
        (p-fire-weapon player))
      (begin
        (player-refire-set! player 0)
        (p-check-ammo player))))

(define (a-check-reload player psp)
  (p-check-ammo player))

;; A_Lower: lowers current weapon, and changes weapon at bottom.
(define (a-lower player psp)
  (pspdef-sy-set! psp (i+ (pspdef-sy psp) LOWERSPEED))
  ;; Is already down.
  (unless (fx< (pspdef-sy psp) WEAPONBOTTOM)
    (cond
      ;; Player is dead.
      [(fx= (player-playerstate player) PST_DEAD)
       ;; don't bring weapon back up
       (pspdef-sy-set! psp WEAPONBOTTOM)]
      ;; The old weapon has been lowered off the screen, so change the
      ;; weapon and start raising it
      [(fx= (player-health player) 0)
       ;; Player is dead, so keep the weapon off screen.
       (p-set-psprite player ps_weapon S_NULL)]
      [else
       (player-readyweapon-set! player (player-pendingweapon player))
       (p-bring-up-weapon player)])))

;; A_Raise
(define (a-raise player psp)
  (pspdef-sy-set! psp (i- (pspdef-sy psp) RAISESPEED))
  (unless (fx> (pspdef-sy psp) WEAPONTOP)
    (pspdef-sy-set! psp WEAPONTOP)
    ;; The weapon has been raised all the way, so change to the ready
    ;; state.
    (p-set-psprite player ps_weapon (weaponinfo-readystate (weapon-of player)))))

;; A_GunFlash
(define (a-gun-flash player psp)
  (p-set-mobj-state (player-mo player) S_PLAY_ATK2)
  (p-set-psprite player ps_flash (weaponinfo-flashstate (weapon-of player))))

;; WEAPON ATTACKS

(define (use-ammo! player n)
  (let ([ammo (weaponinfo-ammo (weapon-of player))] [ammos (player-ammo player)])
    (fxvector-set! ammos ammo (fx- (fxvector-ref ammos ammo) n))))

;; A_Punch
(define (a-punch player psp)
  (let* ([mo (player-mo player)]
         [damage (fxsll (fx+ (fxremainder (p-random) 10) 1) 1)]
         [damage (if (fx= (fxvector-ref (player-powers player) pw_strength) 0) damage (fx* damage 10))]
         [angle (angle+ (mobj-angle mo) (fxand (fxsll (p-random-diff) 18) #xFFFFFFFF))]
         [slope (p-aim-line-attack mo angle MELEERANGE)])
    (p-line-attack mo angle MELEERANGE slope damage)
    ;; turn to face target
    (when linetarget
      (s-start-sound mo sfx_punch)
      (mobj-angle-set! mo (r-point-to-angle2 (mobj-x mo) (mobj-y mo)
                                             (mobj-x linetarget) (mobj-y linetarget))))))

;; A_Saw
(define (a-saw player psp)
  (let* ([mo (player-mo player)]
         [damage (fx* 2 (fx+ (fxremainder (p-random) 10) 1))]
         [angle (angle+ (mobj-angle mo) (fxand (fxsll (p-random-diff) 18) #xFFFFFFFF))]
         ;; use meleerange + 1 se the puff doesn't skip the flash
         [slope (p-aim-line-attack mo angle (fx+ MELEERANGE 1))])
    (p-line-attack mo angle (fx+ MELEERANGE 1) slope damage)
    (if (not linetarget)
        (s-start-sound mo sfx_sawful)
        (begin
          (s-start-sound mo sfx_sawhit)
          ;; turn to face target
          (let* ([angle (r-point-to-angle2 (mobj-x mo) (mobj-y mo) (mobj-x linetarget) (mobj-y linetarget))]
                 [d (angle- angle (mobj-angle mo))])
            (if (fx> d ANG180)
                (if (fx< d (uneg (fxquotient ANG90 20)))
                    (mobj-angle-set! mo (angle+ angle (fxquotient ANG90 21)))
                    (mobj-angle-set! mo (angle- (mobj-angle mo) (fxquotient ANG90 20))))
                (if (fx> d (fxquotient ANG90 20))
                    (mobj-angle-set! mo (angle- angle (fxquotient ANG90 21)))
                    (mobj-angle-set! mo (angle+ (mobj-angle mo) (fxquotient ANG90 20))))))
          (mobj-flags-set! mo (fxior (mobj-flags mo) MF_JUSTATTACKED))))))

;; A_FireMissile
(define (a-fire-missile player psp)
  (use-ammo! player 1)
  (p-spawn-player-missile (player-mo player) MT_ROCKET))

;; A_FireBFG
(define (a-fire-bfg player psp)
  (use-ammo! player BFGCELLS)
  (p-spawn-player-missile (player-mo player) MT_BFG))

;; A_FirePlasma
(define (a-fire-plasma player psp)
  (use-ammo! player 1)
  (p-set-psprite player ps_flash (fx+ (weaponinfo-flashstate (weapon-of player)) (fxand (p-random) 1)))
  (p-spawn-player-missile (player-mo player) MT_PLASMA))

;; P_BulletSlope: sets a slope so a near miss is at aproximately the height
;; of the intended target.
(define bulletslope 0)
(define (p-bullet-slope mo)
  ;; see which target is to be aimed at
  (let ([an (mobj-angle mo)])
    (set! bulletslope (p-aim-line-attack mo an (* 16 64 FRACUNIT)))
    (unless linetarget
      (let ([an (angle+ an (fxsll 1 26))])
        (set! bulletslope (p-aim-line-attack mo an (* 16 64 FRACUNIT)))
        (unless linetarget
          (set! bulletslope (p-aim-line-attack mo (angle- an (fxsll 2 26)) (* 16 64 FRACUNIT))))))))

;; P_GunShot
(define (p-gun-shot mo accurate)
  (let* ([damage (fx* 5 (fx+ (fxremainder (p-random) 3) 1))]
         [angle (if accurate
                    (mobj-angle mo)
                    (angle+ (mobj-angle mo) (fxand (fxsll (p-random-diff) 18) #xFFFFFFFF)))])
    (p-line-attack mo angle MISSILERANGE bulletslope damage)))

;; A_FirePistol
(define (a-fire-pistol player psp)
  (s-start-sound (player-mo player) sfx_pistol)
  (p-set-mobj-state (player-mo player) S_PLAY_ATK2)
  (use-ammo! player 1)
  (p-set-psprite player ps_flash (weaponinfo-flashstate (weapon-of player)))
  (p-bullet-slope (player-mo player))
  (p-gun-shot (player-mo player) (fx= (player-refire player) 0)))

;; A_FireShotgun
(define (a-fire-shotgun player psp)
  (s-start-sound (player-mo player) sfx_shotgn)
  (p-set-mobj-state (player-mo player) S_PLAY_ATK2)
  (use-ammo! player 1)
  (p-set-psprite player ps_flash (weaponinfo-flashstate (weapon-of player)))
  (p-bullet-slope (player-mo player))
  (do ([i 0 (fx+ i 1)]) ((fx= i 7))
    (p-gun-shot (player-mo player) #f)))

;; A_FireShotgun2
(define (a-fire-shotgun2 player psp)
  (let ([mo (player-mo player)])
    (s-start-sound mo sfx_dshtgn)
    (p-set-mobj-state mo S_PLAY_ATK2)
    (use-ammo! player 2)
    (p-set-psprite player ps_flash (weaponinfo-flashstate (weapon-of player)))
    (p-bullet-slope mo)
    (do ([i 0 (fx+ i 1)]) ((fx= i 20))
      (let* ([damage (fx* 5 (fx+ (fxremainder (p-random) 3) 1))]
             [angle (angle+ (mobj-angle mo) (fxand (fxsll (p-random-diff) 19) #xFFFFFFFF))])
        (p-line-attack mo angle MISSILERANGE (i+ bulletslope (fxsll (p-random-diff) 5)) damage)))))

;; A_FireCGun
(define (a-fire-cgun player psp)
  (s-start-sound (player-mo player) sfx_pistol)
  (let ([ammo (weaponinfo-ammo (weapon-of player))])
    (unless (fx= (fxvector-ref (player-ammo player) ammo) 0)
      (p-set-mobj-state (player-mo player) S_PLAY_ATK2)
      (use-ammo! player 1)
      (p-set-psprite player ps_flash
                     (fx+ (weaponinfo-flashstate (weapon-of player))
                          (fx- (state-index (pspdef-state psp)) S_CHAIN1)))
      (p-bullet-slope (player-mo player))
      (p-gun-shot (player-mo player) (fx= (player-refire player) 0)))))

;; ?
(define (a-light0 player psp) (player-extralight-set! player 0))
(define (a-light1 player psp) (player-extralight-set! player 1))
(define (a-light2 player psp) (player-extralight-set! player 2))

;; A_BFGSpray: spawn a BFG explosion on every monster in view.
(define (a-bfg-spray mo)
  ;; offset angles from its attack angle
  (do ([i 0 (fx+ i 1)]) ((fx= i 40))
    (let ([an (angle+ (angle- (mobj-angle mo) (fxquotient ANG90 2)) (fx* (fxquotient ANG90 40) i))])
      ;; mo->target is the originator (player) of the missile
      (p-aim-line-attack (mobj-target mo) an (* 16 64 FRACUNIT))
      (when linetarget
        (p-spawn-mobj (mobj-x linetarget) (mobj-y linetarget)
                      (i+ (mobj-z linetarget) (fxsra (mobj-height linetarget) 2))
                      MT_EXTRABFG)
        (let ([damage (let loop ([j 0] [damage 0])
                        (if (fx= j 15) damage (loop (fx+ j 1) (fx+ damage (fx+ (fxand (p-random) 7) 1)))))])
          (p-damage-mobj linetarget (mobj-target mo) (mobj-target mo) damage))))))

;; A_BFGsound
(define (a-bfg-sound player psp)
  (s-start-sound (player-mo player) sfx_bfg))

;; P_SetupPsprites: called at start of level for each player.
(define (p-setup-psprites player)
  ;; remove all psprites
  (vector-for-each (lambda (psp) (pspdef-state-set! psp #f)) (player-psprites player))
  ;; spawn the gun
  (player-pendingweapon-set! player (player-readyweapon player))
  (p-bring-up-weapon player))

;; P_MovePsprites: called every tic by player thinking routine.
(define (p-move-psprites player)
  (do ([i 0 (fx+ i 1)]) ((fx= i NUMPSPRITES))
    (let ([psp (vector-ref (player-psprites player) i)])
      ;; a null state means not active
      (when (pspdef-state psp)
        ;; drop tic count and possibly change state; a -1 tic count never
        ;; changes
        (unless (fx= (pspdef-tics psp) -1)
          (pspdef-tics-set! psp (fx- (pspdef-tics psp) 1))
          (when (fx= (pspdef-tics psp) 0)
            (p-set-psprite player i (state-nextstate (pspdef-state psp))))))))
  (let ([weapon (vector-ref (player-psprites player) ps_weapon)]
        [flash (vector-ref (player-psprites player) ps_flash)])
    (pspdef-sx-set! flash (pspdef-sx weapon))
    (pspdef-sy-set! flash (pspdef-sy weapon))))

(define p-pspr-actions
  `((A_WeaponReady . ,a-weapon-ready) (A_ReFire . ,a-re-fire) (A_CheckReload . ,a-check-reload)
    (A_Lower . ,a-lower) (A_Raise . ,a-raise) (A_GunFlash . ,a-gun-flash) (A_Punch . ,a-punch)
    (A_Saw . ,a-saw) (A_FireMissile . ,a-fire-missile) (A_FireBFG . ,a-fire-bfg)
    (A_FirePlasma . ,a-fire-plasma) (A_FirePistol . ,a-fire-pistol)
    (A_FireShotgun . ,a-fire-shotgun) (A_FireShotgun2 . ,a-fire-shotgun2)
    (A_FireCGun . ,a-fire-cgun) (A_Light0 . ,a-light0) (A_Light1 . ,a-light1)
    (A_Light2 . ,a-light2) (A_BFGSpray . ,a-bfg-spray) (A_BFGsound . ,a-bfg-sound)))
