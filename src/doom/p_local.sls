;;; p_local: the playsim.
;;;
;;; Like the renderer (r_local), the playsim's C files share state through
;;; one header, p_local.h, and call each other in cycles, so they form one
;;; library: this file holds p_local.h's constants and the blockmap/reject
;;; state (defined in p_setup.c in C), and each p_*.c is ported into its own
;;; included file.
(library (doom p_local)
  (export
   ;; p_tick
   thinkercap p-init-thinkers p-add-thinker p-remove-thinker p-run-thinkers p-ticker
   ;; p_maputl
   p-aprox-distance p-point-on-line-side p-box-on-line-side p-line-opening
   p-set-thing-position p-unset-thing-position p-block-lines-iterator
   p-block-things-iterator p-path-traverse
   ;; p_map
   p-check-position p-try-move p-teleport-move p-slide-move p-aim-line-attack
   p-line-attack p-use-lines p-radius-attack p-change-sector linetarget attackrange
   ;; p_sight
   p-check-sight
   ;; p_mobj
   p-mobj-thinker p-set-mobj-state p-spawn-mobj p-remove-mobj p-spawn-player
   p-spawn-map-thing p-spawn-puff p-spawn-blood p-spawn-missile p-spawn-player-missile
   g-player-reborn
   ;; p_user
   p-player-think p-calc-height
   ;; p_setup
   p-setup-level p-init map-lump-name
   ;; stand-ins for unported functions
   unported-calls unported-first-tic
   ;; p_local.h
   FLOATSPEED MAXHEALTH VIEWHEIGHT
          MAPBLOCKUNITS MAPBLOCKSIZE MAPBLOCKSHIFT MAPBMASK MAPBTOFRAC
          PLAYERRADIUS MAXRADIUS GRAVITY MAXMOVE USERANGE MELEERANGE MISSILERANGE
          BASETHRESHOLD ONFLOORZ ONCEILINGZ ITEMQUESIZE MAXINTERCEPTS
          PT_ADDLINES PT_ADDTHINGS PT_EARLYOUT
          blockmaplump BLOCKMAP bmapwidth bmapheight bmaporgx bmaporgy
          blocklinks rejectmatrix)
  (import (chezscheme) (doom global) (doom struct) (doom doomdef) (doom doomdata)
          (doom doomstat) (doom d_think) (doom d_player) (doom r_defs) (doom r_state)
          (doom r_data) (doom r_local) (doom p_mobj.h) (doom info) (doom m_fixed)
          (doom m_bbox) (doom m_random) (doom tables) (doom w_wad) (doom i_system)
          (doom d_ticcmd) (doom d_event) (doom s_sound) (doom sounds))

  (define FLOATSPEED (* FRACUNIT 4))
  (define MAXHEALTH 100)
  (define VIEWHEIGHT (* 41 FRACUNIT))

  ;; Mapblocks are used to check movement against lines and things.
  (define MAPBLOCKUNITS 128)
  (define MAPBLOCKSIZE (* MAPBLOCKUNITS FRACUNIT))
  (define MAPBLOCKSHIFT (+ FRACBITS 7))
  (define MAPBMASK (- MAPBLOCKSIZE 1))
  (define MAPBTOFRAC (- MAPBLOCKSHIFT FRACBITS))

  ;; Note: C's PLAYERRADIUS and MAXRADIUS are unparenthesized
  ;; (16*FRACUNIT), which matters if a use ever divides by them.
  (define PLAYERRADIUS (* 16 FRACUNIT))
  (define MAXRADIUS (* 32 FRACUNIT))

  (define GRAVITY FRACUNIT)
  (define MAXMOVE (* 30 FRACUNIT))
  (define USERANGE (* 64 FRACUNIT))
  (define MELEERANGE (* 64 FRACUNIT))
  (define MISSILERANGE (* 32 64 FRACUNIT))
  (define BASETHRESHOLD 100)

  (define ONFLOORZ MININT)
  (define ONCEILINGZ MAXINT)
  (define ITEMQUESIZE 128)
  (define MAXINTERCEPTS 128)

  (define PT_ADDLINES 1)
  (define PT_ADDTHINGS 2)
  (define PT_EARLYOUT 4)

  ;; The BLOCKMAP lump as signed shorts (C converts the cached lump in place).
  ;; C's `blockmap` pointer is blockmaplump+4: index with (+ BLOCKMAP i).
  (define-global blockmaplump (fxvector))
  (define BLOCKMAP 4)
  (define-global bmapwidth 0)
  (define-global bmapheight 0)        ; size in mapblocks
  (define-global bmaporgx 0)          ; origin of the block map
  (define-global bmaporgy 0)
  (define-global blocklinks (vector)) ; per block: first mobj (bnext chain) or #f

  ;; REJECT lump, for fast sight rejection.
  (define-global rejectmatrix (bytevector))

  ;; int / fixed_t arithmetic that wraps like 32-bit C, and angle_t negation.
  (define-syntax i+ (syntax-rules () [(_ a b) (int32 (fx+ a b))]))
  (define-syntax i- (syntax-rules () [(_ a b) (int32 (fx- a b))]))
  (define-syntax ineg (syntax-rules () [(_ a) (int32 (fx- 0 a))]))
  (define-syntax uneg (syntax-rules () [(_ a) (fxand (fx- 0 a) #xFFFFFFFF)]))

  ;; C's (P_Random()-P_Random()). C leaves the evaluation order open; the
  ;; oracle's gcc calls the left one first (checked in P_SpawnPuff's code),
  ;; as does DOS Doom.
  (define (p-random-diff)
    (let ([a (p-random)])
      (fx- a (p-random))))

  ;; Action functions by name (symbol -> procedure). The files that define
  ;; actions add to it; p-init puts the procedures into the state table.
  (define action-table (make-eq-hashtable))

  (include "p_unported.ss")
  (include "p_tick.ss")
  (include "p_maputl.ss")
  (include "p_map.ss")
  (include "p_sight.ss")
  (include "p_mobj.ss")
  (include "p_user.ss")
  (include "p_setup.ss"))
