;;; Stand-ins for the playsim functions of milestone 5 (p_inter, p_spec,
;;; p_switch, p_pspr, p_enemy, and the other world-effect files). Included
;;; by p_local.sls.
;;;
;;; A stand-in that would change the simulation logs the call: tests
;;; compare against the oracle only up to the first such call (see
;;; tests/sim.ss). Ones whose effects nothing compares yet are silent.

;; Names of logged functions, in first-call order, and the gametic of the
;; first call (#f if none yet).
(define-global unported-calls '())
(define-global unported-first-tic #f)

(define (unported name)
  (unless unported-first-tic
    (set! unported-first-tic gametic))
  (unless (memq name unported-calls)
    (set! unported-calls (append unported-calls (list name)))))

;; p_inter.c
(define (p-touch-special-thing special toucher) (unported 'P_TouchSpecialThing))
(define (p-damage-mobj target inflictor source damage) (unported 'P_DamageMobj))

;; p_spec.c
(define (p-cross-special-line linenum side thing) (unported 'P_CrossSpecialLine))
(define (p-shoot-special-line thing line) (unported 'P_ShootSpecialLine))
(define (p-player-in-special-sector player) (unported 'P_PlayerInSpecialSector))
;; Light flickers, animated walls and flats, scrolling walls. These change
;; sector light levels and call P_Random, which tests don't compare yet.
(define (p-spawn-specials) (void))
(define (p-update-specials) (void))

;; p_switch.c
(define (p-use-special-line thing line side) (unported 'P_UseSpecialLine) #f)

;; p_pspr.c. P_SetupPsprites settles the weapon (pendingweapon becomes
;; wp_nochange as the gun comes up); the weapon sprites themselves only
;; matter once the player fires or switches weapons.
(define (p-setup-psprites player)
  (player-pendingweapon-set! player wp_nochange))
(define (p-move-psprites player)
  (when (or (fxlogtest (ticcmd-buttons (player-cmd player)) BT_ATTACK)
            (not (fx= (player-pendingweapon player) wp_nochange)))
    (unported 'P_MovePsprites)))

;; Action functions (p_enemy.c, p_pspr.c): every state action, by name.
(define (unported-action name)
  (lambda args (unported name)))
