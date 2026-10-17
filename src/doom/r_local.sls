;;; r_local: the software renderer.
;;;
;;; The C renderer's files share their state through r_local.h and call each
;;; other in cycles (r_bsp -> r_segs -> r_main -> r_bsp, r_segs <-> r_things),
;;; which R6RS libraries can't. So the renderer is this one library, with
;;; each C file ported into its own included file. Inside it, C's globals
;;; are plain variables; the few that other modules read or set are
;;; define-globals.
(library (doom r_local)
  (export
   ;; r_main
   r-init r-set-view-size r-execute-set-view-size r-render-player-view r-net-update
   r-point-in-subsector r-point-to-angle r-point-to-angle2 r-point-to-dist
   r-point-on-side r-point-on-seg-side
   viewangleoffset validcount framecount detailshift setsizeneeded
   LIGHTLEVELS LIGHTSEGSHIFT MAXLIGHTSCALE LIGHTSCALESHIFT MAXLIGHTZ LIGHTZSHIFT
   NUMCOLORMAPS
   ;; r_draw
   viewwidth scaledviewwidth viewheight viewwindowx viewwindowy
   r-fill-back-screen r-draw-view-border r-video-erase
   ;; -hires
   r-set-hires! rwidth rheight hires-shift
   view2d-x view2d-y view2d-width view2d-scaledwidth view2d-height
   ;; r_sky
   SKYFLATNAME skyflatnum skytexture
   ;; r_things
   r-init-sprites sprites numsprites garbage-mask spritedef-spriteframes spriteframe-lump
   spriteframe-flip)
  (import (chezscheme) (doom global) (doom struct) (doom doomdef) (doom doomdata)
          (doom doomstat) (doom m_fixed) (doom m_bbox) (doom tables) (doom r_defs)
          (doom r_state) (doom r_data) (doom p_mobj.h) (doom d_player) (doom info)
          (doom v_video) (doom w_wad) (doom i_system))

  ;; Garbage-read tracking, for tests (see r_things.c.ss and r_draw.c.ss).
  (define-global garbage-mask #f)

  ;; The renderer's resolution (-hires k): rwidth x rheight, k = 2^hires-shift
  ;; times 320x200, drawn into rbuf. At k = 1 rbuf is `screens` (screen 0)
  ;; and everything is as in C. The 2D layer (status bar, menus, messages,
  ;; automap, border) stays 320x200 in screens; see "Hi-res" in DESIGN.md.
  (define-global rwidth SCREENWIDTH)
  (define-global rheight SCREENHEIGHT)
  (define-global hires-shift 0)
  (define-global rbuf screens)
  ;; The largest the renderer's arrays need to be (k = 4).
  (define MAXWIDTH 1280)
  (define MAXHEIGHT 832)
  ;; The view window in 320x200 coordinates, for the 2D layer (the border,
  ;; the heads-up erase, the pause sign): viewwindowx etc. divided by k.
  (define-global view2d-x 0)
  (define-global view2d-y 0)
  (define-global view2d-width 0)
  (define-global view2d-scaledwidth 0)
  (define-global view2d-height 0)

  (include "r_main.c.ss")
  (include "r_draw.c.ss")
  (include "r_sky.c.ss")
  (include "r_plane.c.ss")
  (include "r_segs.c.ss")
  (include "r_bsp.c.ss")
  (include "r_things.c.ss"))
