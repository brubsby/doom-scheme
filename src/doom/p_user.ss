;;; p_user.c: player movement, view height and bobbing, and the per-tic
;;; player think. Included by p_local.sls.

;; Index of the special effects (INVUL inverse) map.
(define INVERSECOLORMAP 32)

;; Movement: 16 pixels of bob.
(define MAXBOB #x100000)

(define onground #f)

;; P_Thrust: moves the given origin along a given angle.
(define (p-thrust player angle move)
  (let ([mo (player-mo player)] [angle (fxsrl angle ANGLETOFINESHIFT)])
    (mobj-momx-set! mo (i+ (mobj-momx mo) (fixed-mul move (finecosine angle))))
    (mobj-momy-set! mo (i+ (mobj-momy mo) (fixed-mul move (fxvector-ref finesine angle))))))

;; P_CalcHeight: calculate the walking / running height adjustment.
(define (p-calc-height player)
  (let ([mo (player-mo player)])
    ;; Regular movement bobbing (needs to be calculated for gun swing even
    ;; if not on ground)
    (player-bob-set! player (fxsra (i+ (fixed-mul (mobj-momx mo) (mobj-momx mo))
                                       (fixed-mul (mobj-momy mo) (mobj-momy mo)))
                                   2))
    (when (fx> (player-bob player) MAXBOB)
      (player-bob-set! player MAXBOB))
    (if (or (fxlogtest (player-cheats player) CF_NOMOMENTUM) (not onground))
        (begin
          ;; (the first two assignments are overwritten, as in C)
          (player-viewz-set! player (i+ (mobj-z mo) VIEWHEIGHT))
          (when (fx> (player-viewz player) (i- (mobj-ceilingz mo) (* 4 FRACUNIT)))
            (player-viewz-set! player (i- (mobj-ceilingz mo) (* 4 FRACUNIT))))
          (player-viewz-set! player (i+ (mobj-z mo) (player-viewheight player))))
        (let* ([angle (fxand (fx* (fxquotient FINEANGLES 20) leveltime) FINEMASK)]
               [bob (fixed-mul (fxquotient (player-bob player) 2) (fxvector-ref finesine angle))])
          ;; move viewheight
          (when (fx= (player-playerstate player) PST_LIVE)
            (player-viewheight-set! player (i+ (player-viewheight player)
                                               (player-deltaviewheight player)))
            (when (fx> (player-viewheight player) VIEWHEIGHT)
              (player-viewheight-set! player VIEWHEIGHT)
              (player-deltaviewheight-set! player 0))
            (when (fx< (player-viewheight player) (fxquotient VIEWHEIGHT 2))
              (player-viewheight-set! player (fxquotient VIEWHEIGHT 2))
              (when (fx<= (player-deltaviewheight player) 0)
                (player-deltaviewheight-set! player 1)))
            (unless (fx= (player-deltaviewheight player) 0)
              (player-deltaviewheight-set! player (i+ (player-deltaviewheight player)
                                                      (fxquotient FRACUNIT 4)))
              (when (fx= (player-deltaviewheight player) 0)
                (player-deltaviewheight-set! player 1))))
          (player-viewz-set! player (i+ (i+ (mobj-z mo) (player-viewheight player)) bob))
          (when (fx> (player-viewz player) (i- (mobj-ceilingz mo) (* 4 FRACUNIT)))
            (player-viewz-set! player (i- (mobj-ceilingz mo) (* 4 FRACUNIT))))))))

;; P_MovePlayer
(define (p-move-player player)
  (let ([cmd (player-cmd player)] [mo (player-mo player)])
    (mobj-angle-set! mo (angle+ (mobj-angle mo) (fxand (fxsll (ticcmd-angleturn cmd) 16) #xFFFFFFFF)))
    ;; Do not let the player control movement if not onground.
    (set! onground (fx<= (mobj-z mo) (mobj-floorz mo)))
    (when (and (not (fx= (ticcmd-forwardmove cmd) 0)) onground)
      (p-thrust player (mobj-angle mo) (fx* (ticcmd-forwardmove cmd) 2048)))
    (when (and (not (fx= (ticcmd-sidemove cmd) 0)) onground)
      (p-thrust player (angle- (mobj-angle mo) ANG90) (fx* (ticcmd-sidemove cmd) 2048)))
    (when (and (or (not (fx= (ticcmd-forwardmove cmd) 0)) (not (fx= (ticcmd-sidemove cmd) 0)))
               (eq? (mobj-state mo) (vector-ref states S_PLAY)))
      (p-set-mobj-state mo S_PLAY_RUN1))))

;; P_DeathThink: fall on your face when dying. Decrease POV height to floor
;; height.
(define ANG5 (fxquotient ANG90 18))

(define (p-death-think player)
  (let ([mo (player-mo player)])
    (p-move-psprites player)
    ;; fall to the ground
    (when (fx> (player-viewheight player) (* 6 FRACUNIT))
      (player-viewheight-set! player (fx- (player-viewheight player) FRACUNIT)))
    (when (fx< (player-viewheight player) (* 6 FRACUNIT))
      (player-viewheight-set! player (* 6 FRACUNIT)))
    (player-deltaviewheight-set! player 0)
    (set! onground (fx<= (mobj-z mo) (mobj-floorz mo)))
    (p-calc-height player)
    (let ([attacker (player-attacker player)])
      (if (and attacker (not (eq? attacker mo)))
          (let* ([angle (r-point-to-angle2 (mobj-x mo) (mobj-y mo)
                                           (mobj-x attacker) (mobj-y attacker))]
                 [delta (angle- angle (mobj-angle mo))])
            (cond
              [(or (fx< delta ANG5) (fx> delta (uneg ANG5)))
               ;; Looking at killer, so fade damage flash down.
               (mobj-angle-set! mo angle)
               (unless (fx= (player-damagecount player) 0)
                 (player-damagecount-set! player (fx- (player-damagecount player) 1)))]
              [(fx< delta ANG180) (mobj-angle-set! mo (angle+ (mobj-angle mo) ANG5))]
              [else (mobj-angle-set! mo (angle- (mobj-angle mo) ANG5))]))
          (unless (fx= (player-damagecount player) 0)
            (player-damagecount-set! player (fx- (player-damagecount player) 1)))))
    (when (fxlogtest (ticcmd-buttons (player-cmd player)) BT_USE)
      (player-playerstate-set! player PST_REBORN))))

(define-syntax power (syntax-rules () [(_ p n) (fxvector-ref (player-powers p) n)]))
(define-syntax power-set! (syntax-rules () [(_ p n v) (fxvector-set! (player-powers p) n v)]))

;; P_PlayerThink
(define (p-player-think player)
  (let ([mo (player-mo player)] [cmd (player-cmd player)])
    ;; fixme: do this in the cheat code
    (mobj-flags-set! mo (if (fxlogtest (player-cheats player) CF_NOCLIP)
                            (fxior (mobj-flags mo) MF_NOCLIP)
                            (fxand (mobj-flags mo) (fxnot MF_NOCLIP))))
    ;; chain saw run forward
    (when (fxlogtest (mobj-flags mo) MF_JUSTATTACKED)
      (ticcmd-angleturn-set! cmd 0)
      (ticcmd-forwardmove-set! cmd (fxquotient #xc800 512))
      (ticcmd-sidemove-set! cmd 0)
      (mobj-flags-set! mo (fxand (mobj-flags mo) (fxnot MF_JUSTATTACKED))))
    (if (fx= (player-playerstate player) PST_DEAD)
        (p-death-think player)
        (begin
          ;; Move around. Reactiontime is used to prevent movement for a
          ;; bit after a teleport.
          (if (fx= (mobj-reactiontime mo) 0)
              (p-move-player player)
              (mobj-reactiontime-set! mo (fx- (mobj-reactiontime mo) 1)))
          (p-calc-height player)
          (unless (fx= (sector-special (subsector-sector (mobj-subsector mo))) 0)
            (p-player-in-special-sector player))
          ;; Check for weapon change. A special event has no other buttons.
          (when (fxlogtest (ticcmd-buttons cmd) BT_SPECIAL)
            (ticcmd-buttons-set! cmd 0))
          (when (fxlogtest (ticcmd-buttons cmd) BT_CHANGE)
            ;; The actual changing of the weapon is done when the weapon
            ;; psprite can do it (read: not in the middle of an attack).
            (let* ([owned (player-weaponowned player)]
                   [newweapon (fxsra (fxand (ticcmd-buttons cmd) BT_WEAPONMASK) BT_WEAPONSHIFT)]
                   [newweapon (if (and (fx= newweapon wp_fist)
                                       (vector-ref owned wp_chainsaw)
                                       (not (and (fx= (player-readyweapon player) wp_chainsaw)
                                                 (not (fx= (power player pw_strength) 0)))))
                                  wp_chainsaw
                                  newweapon)]
                   [newweapon (if (and (fx= gamemode commercial)
                                       (fx= newweapon wp_shotgun)
                                       (vector-ref owned wp_supershotgun)
                                       (not (fx= (player-readyweapon player) wp_supershotgun)))
                                  wp_supershotgun
                                  newweapon)])
              (when (and (vector-ref owned newweapon)
                         (not (fx= newweapon (player-readyweapon player)))
                         ;; Do not go to plasma or BFG in shareware, even if
                         ;; cheated.
                         (or (and (not (fx= newweapon wp_plasma)) (not (fx= newweapon wp_bfg)))
                             (not (fx= gamemode shareware))))
                (player-pendingweapon-set! player newweapon))))
          ;; check for use
          (if (fxlogtest (ticcmd-buttons cmd) BT_USE)
              (unless (player-usedown player)
                (p-use-lines player)
                (player-usedown-set! player #t))
              (player-usedown-set! player #f))
          ;; cycle psprites
          (p-move-psprites player)
          ;; Counters, time dependend power ups.
          ;; Strength counts up to diminish fade.
          (unless (fx= (power player pw_strength) 0)
            (power-set! player pw_strength (fx+ (power player pw_strength) 1)))
          (unless (fx= (power player pw_invulnerability) 0)
            (power-set! player pw_invulnerability (fx- (power player pw_invulnerability) 1)))
          (unless (fx= (power player pw_invisibility) 0)
            (power-set! player pw_invisibility (fx- (power player pw_invisibility) 1))
            (when (fx= (power player pw_invisibility) 0)
              (mobj-flags-set! mo (fxand (mobj-flags mo) (fxnot MF_SHADOW)))))
          (unless (fx= (power player pw_infrared) 0)
            (power-set! player pw_infrared (fx- (power player pw_infrared) 1)))
          (unless (fx= (power player pw_ironfeet) 0)
            (power-set! player pw_ironfeet (fx- (power player pw_ironfeet) 1)))
          (unless (fx= (player-damagecount player) 0)
            (player-damagecount-set! player (fx- (player-damagecount player) 1)))
          (unless (fx= (player-bonuscount player) 0)
            (player-bonuscount-set! player (fx- (player-bonuscount player) 1)))
          ;; Handling colormaps.
          (player-fixedcolormap-set!
           player
           (let ([invul (power player pw_invulnerability)] [infra (power player pw_infrared)])
             (cond
               [(not (fx= invul 0))
                (if (or (fx> invul (* 4 32)) (fxlogtest invul 8)) INVERSECOLORMAP 0)]
               [(not (fx= infra 0))
                ;; almost full bright
                (if (or (fx> infra (* 4 32)) (fxlogtest infra 8)) 1 0)]
               [else 0])))))))
