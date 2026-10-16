;;; d_player: player_t, the player's game state beyond its mobj.
(library (doom d_player)
  (export PST_LIVE PST_DEAD PST_REBORN CF_NOCLIP CF_GODMODE CF_NOMOMENTUM
          ps_weapon ps_flash NUMPSPRITES FF_FULLBRIGHT FF_FRAMEMASK
          pspdef make-pspdef pspdef? pspdef-state pspdef-state-set!
          pspdef-tics pspdef-tics-set! pspdef-sx pspdef-sx-set! pspdef-sy pspdef-sy-set!
          player make-player player?
          player-mo player-mo-set! player-playerstate player-playerstate-set!
          player-cmd player-cmd-set! player-viewz player-viewz-set!
          player-viewheight player-viewheight-set!
          player-deltaviewheight player-deltaviewheight-set!
          player-bob player-bob-set! player-health player-health-set!
          player-armorpoints player-armorpoints-set! player-armortype player-armortype-set!
          player-powers player-powers-set! player-cards player-cards-set!
          player-backpack player-backpack-set! player-frags player-frags-set!
          player-readyweapon player-readyweapon-set!
          player-pendingweapon player-pendingweapon-set!
          player-weaponowned player-weaponowned-set! player-ammo player-ammo-set!
          player-maxammo player-maxammo-set! player-attackdown player-attackdown-set!
          player-usedown player-usedown-set! player-cheats player-cheats-set!
          player-refire player-refire-set! player-killcount player-killcount-set!
          player-itemcount player-itemcount-set! player-secretcount player-secretcount-set!
          player-message player-message-set! player-damagecount player-damagecount-set!
          player-bonuscount player-bonuscount-set! player-attacker player-attacker-set!
          player-extralight player-extralight-set!
          player-fixedcolormap player-fixedcolormap-set!
          player-colormap player-colormap-set! player-psprites player-psprites-set!
          player-didsecret player-didsecret-set!
          wbplayerstruct make-wbplayerstruct wbplayerstruct?
          wbplayerstruct-in wbplayerstruct-in-set! wbplayerstruct-skills wbplayerstruct-skills-set!
          wbplayerstruct-sitems wbplayerstruct-sitems-set!
          wbplayerstruct-ssecret wbplayerstruct-ssecret-set!
          wbplayerstruct-stime wbplayerstruct-stime-set! wbplayerstruct-frags wbplayerstruct-frags-set!
          wbplayerstruct-score wbplayerstruct-score-set!
          wbstartstruct make-wbstartstruct wbstartstruct?
          wbstartstruct-epsd wbstartstruct-epsd-set! wbstartstruct-didsecret wbstartstruct-didsecret-set!
          wbstartstruct-last wbstartstruct-last-set! wbstartstruct-next wbstartstruct-next-set!
          wbstartstruct-maxkills wbstartstruct-maxkills-set!
          wbstartstruct-maxitems wbstartstruct-maxitems-set!
          wbstartstruct-maxsecret wbstartstruct-maxsecret-set!
          wbstartstruct-maxfrags wbstartstruct-maxfrags-set!
          wbstartstruct-partime wbstartstruct-partime-set!
          wbstartstruct-pnum wbstartstruct-pnum-set! wbstartstruct-plyr wbstartstruct-plyr-set!)
  (import (chezscheme) (doom struct) (doom doomdef) (doom d_ticcmd))

  (define PST_LIVE 0)
  (define PST_DEAD 1)
  (define PST_REBORN 2)

  (define CF_NOCLIP 1)
  (define CF_GODMODE 2)
  (define CF_NOMOMENTUM 4)

  ;; Frame flags (p_pspr.h): a state's or mobj's frame is a frame number,
  ;; possibly ORed with FF_FULLBRIGHT.
  (define FF_FULLBRIGHT #x8000)
  (define FF_FRAMEMASK #x7fff)

  ;; psprnum_t (p_pspr.h)
  (define ps_weapon 0)
  (define ps_flash 1)
  (define NUMPSPRITES 2)

  (define-struct pspdef ()
    (state #f)                  ; #f (NULL) means not active
    (tics 0) (sx 0) (sy 0))

  (define-struct player ()
    (mo #f)
    (playerstate PST_LIVE)
    (cmd (make-ticcmd))
    (viewz 0) (viewheight 0) (deltaviewheight 0) (bob 0)
    (health 0) (armorpoints 0) (armortype 0)
    (powers (make-fxvector NUMPOWERS 0))
    (cards (make-vector NUMCARDS #f))
    (backpack #f)
    (frags (make-fxvector MAXPLAYERS 0))
    (readyweapon 0) (pendingweapon 0)
    (weaponowned (make-vector NUMWEAPONS #f))
    (ammo (make-fxvector NUMAMMO 0))
    (maxammo (make-fxvector NUMAMMO 0))
    (attackdown #f) (usedown #f)
    (cheats 0)
    (refire 0)
    (killcount 0) (itemcount 0) (secretcount 0)
    (message #f)                ; string or #f
    (damagecount 0) (bonuscount 0)
    (attacker #f)
    (extralight 0)
    (fixedcolormap 0)
    (colormap 0)
    (psprites (vector (make-pspdef) (make-pspdef)))
    (didsecret #f))

  ;; INTERMISSION: structure passed e.g. to WI_Start(wb)
  (define-struct wbplayerstruct ()
    (in #f)                     ; whether the player is in game
    ;; Player stats, kills, collected items etc.
    (skills 0) (sitems 0) (ssecret 0) (stime 0)
    (frags (make-fxvector 4 0))
    (score 0))                  ; current score on entry, modified on return

  (define-struct wbstartstruct ()
    (epsd 0)                    ; episode # (0-2)
    (didsecret #f)              ; if true, splash the secret level
    (last 0) (next 0)           ; previous and next levels, origin 0
    (maxkills 0) (maxitems 0) (maxsecret 0) (maxfrags 0)
    (partime 0)                 ; the par time
    (pnum 0)                    ; index of this player in game
    (plyr (vector (make-wbplayerstruct) (make-wbplayerstruct)
                  (make-wbplayerstruct) (make-wbplayerstruct)))))
