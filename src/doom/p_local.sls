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
   thinkercap p-init-thinkers p-add-thinker p-remove-thinker
   ;; p_maputl
   p-set-thing-position p-unset-thing-position
   ;; p_mobj
   p-mobj-thinker p-spawn-mobj p-spawn-player p-spawn-map-thing g-player-reborn
   ;; p_setup
   p-setup-level p-init map-lump-name
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
          (doom m_bbox) (doom m_random) (doom tables) (doom w_wad) (doom i_system))

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

  (define-syntax i- (syntax-rules () [(_ a b) (int32 (fx- a b))]))

  (include "p_tick.ss")
  (include "p_maputl.ss")
  (include "p_mobj.ss")
  (include "p_setup.ss"))
