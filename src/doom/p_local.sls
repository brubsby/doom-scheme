;;; p_local: playsim constants and the blockmap/reject state (p_local.h).
;;; The blockmap globals are defined in p_setup.c in C.
(library (doom p_local)
  (export FLOATSPEED MAXHEALTH VIEWHEIGHT
          MAPBLOCKUNITS MAPBLOCKSIZE MAPBLOCKSHIFT MAPBMASK MAPBTOFRAC
          PLAYERRADIUS MAXRADIUS GRAVITY MAXMOVE USERANGE MELEERANGE MISSILERANGE
          BASETHRESHOLD ONFLOORZ ONCEILINGZ ITEMQUESIZE MAXINTERCEPTS
          PT_ADDLINES PT_ADDTHINGS PT_EARLYOUT
          blockmaplump BLOCKMAP bmapwidth bmapheight bmaporgx bmaporgy
          blocklinks rejectmatrix)
  (import (chezscheme) (doom global) (doom m_fixed))

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
  (define-global rejectmatrix (bytevector)))
