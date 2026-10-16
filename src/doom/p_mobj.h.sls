;;; p_mobj.h: mobj_t and its flags. A separate library from p_mobj (the
;;; playsim, in p_local) because the renderer reads mobjs and the playsim
;;; calls the renderer (R_PointInSubsector).
(library (doom p_mobj.h)
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
          MF_SPECIAL MF_SOLID MF_SHOOTABLE MF_NOSECTOR MF_NOBLOCKMAP MF_AMBUSH
          MF_JUSTHIT MF_JUSTATTACKED MF_SPAWNCEILING MF_NOGRAVITY MF_DROPOFF
          MF_PICKUP MF_NOCLIP MF_SLIDE MF_FLOAT MF_TELEPORT MF_MISSILE
          MF_DROPPED MF_SHADOW MF_NOBLOOD MF_CORPSE MF_INFLOAT MF_COUNTKILL
          MF_COUNTITEM MF_SKULLFLY MF_NOTDMATCH MF_TRANSLATION MF_TRANSSHIFT)
  (import (chezscheme) (doom struct) (doom r_defs) (doom doomdata))

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

  ;; mobjflag_t
  (define MF_SPECIAL 1)          ; call P_SpecialThing when touched
  (define MF_SOLID 2)            ; blocks
  (define MF_SHOOTABLE 4)        ; can be hit
  (define MF_NOSECTOR 8)         ; don't use the sector links (invisible but touchable)
  (define MF_NOBLOCKMAP 16)      ; don't use the blocklinks (inert but displayable)
  (define MF_AMBUSH 32)          ; not to be activated by sound, deaf monster
  (define MF_JUSTHIT 64)         ; will try to attack right back
  (define MF_JUSTATTACKED 128)   ; will take at least one step before attacking
  (define MF_SPAWNCEILING 256)   ; on level spawning (initial position), hang from ceiling
  (define MF_NOGRAVITY 512)      ; don't apply gravity (every tic)
  (define MF_DROPOFF #x400)      ; this allows jumps from high places
  (define MF_PICKUP #x800)       ; for players, will pick up items
  (define MF_NOCLIP #x1000)      ; player cheat
  (define MF_SLIDE #x2000)       ; player: keep info about sliding along walls
  (define MF_FLOAT #x4000)       ; allow moves to any height, no gravity
  (define MF_TELEPORT #x8000)    ; don't cross lines or look at heights on teleport
  (define MF_MISSILE #x10000)    ; don't hit same species, explode on block
  (define MF_DROPPED #x20000)    ; dropped by a demon, not level spawned
  (define MF_SHADOW #x40000)     ; use fuzzy draw (shadow demons or spectres)
  (define MF_NOBLOOD #x80000)    ; flag: don't bleed when shot (use puff)
  (define MF_CORPSE #x100000)    ; don't stop moving halfway off a step
  (define MF_INFLOAT #x200000)   ; floating to a height for a move
  (define MF_COUNTKILL #x400000) ; on kill, count this enemy object
  (define MF_COUNTITEM #x800000) ; on picking up, count this item object
  (define MF_SKULLFLY #x1000000) ; special handling: skull in flight
  (define MF_NOTDMATCH #x2000000) ; don't spawn this object in death match mode
  (define MF_TRANSLATION #xc000000) ; player sprite color translation
  (define MF_TRANSSHIFT 26))     ; hmm ???.
