;;; d_items: weapon definitions (d_items.c).
(library (doom d_items)
  (export weaponinfo make-weaponinfo weaponinfo? weaponinfo-ammo weaponinfo-upstate
          weaponinfo-downstate weaponinfo-readystate weaponinfo-atkstate
          weaponinfo-flashstate weaponinfo-table)
  (import (chezscheme) (doom struct) (doom doomdef) (doom info))

  ;; weaponinfo_t: the ammo it uses, and its psprite states.
  (define-struct weaponinfo ()
    (ammo 0) (upstate 0) (downstate 0) (readystate 0) (atkstate 0) (flashstate 0))

  ;; C: weaponinfo[NUMWEAPONS], indexed by weapontype_t.
  (define weaponinfo-table
    (vector
     (make-weaponinfo am_noammo S_PUNCHUP S_PUNCHDOWN S_PUNCH S_PUNCH1 S_NULL)  ; fist
     (make-weaponinfo am_clip S_PISTOLUP S_PISTOLDOWN S_PISTOL S_PISTOL1 S_PISTOLFLASH) ; pistol
     (make-weaponinfo am_shell S_SGUNUP S_SGUNDOWN S_SGUN S_SGUN1 S_SGUNFLASH1)  ; shotgun
     (make-weaponinfo am_clip S_CHAINUP S_CHAINDOWN S_CHAIN S_CHAIN1 S_CHAINFLASH1) ; chaingun
     (make-weaponinfo am_misl S_MISSILEUP S_MISSILEDOWN S_MISSILE S_MISSILE1 S_MISSILEFLASH1) ; missile launcher
     (make-weaponinfo am_cell S_PLASMAUP S_PLASMADOWN S_PLASMA S_PLASMA1 S_PLASMAFLASH1) ; plasma rifle
     (make-weaponinfo am_cell S_BFGUP S_BFGDOWN S_BFG S_BFG1 S_BFGFLASH1)        ; bfg 9000
     (make-weaponinfo am_noammo S_SAWUP S_SAWDOWN S_SAW S_SAW1 S_NULL)          ; chainsaw
     (make-weaponinfo am_shell S_DSGUNUP S_DSGUNDOWN S_DSGUN S_DSGUN1 S_DSGUNFLASH1)))) ; super shotgun
