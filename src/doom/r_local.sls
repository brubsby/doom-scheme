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
   r-init r-set-view-size r-execute-set-view-size r-render-player-view
   r-point-in-subsector r-point-to-angle r-point-to-angle2 r-point-to-dist
   r-point-on-side r-point-on-seg-side
   viewangleoffset validcount framecount detailshift setsizeneeded
   LIGHTLEVELS LIGHTSEGSHIFT MAXLIGHTSCALE LIGHTSCALESHIFT MAXLIGHTZ LIGHTZSHIFT
   NUMCOLORMAPS
   ;; r_draw
   viewwidth scaledviewwidth viewheight viewwindowx viewwindowy
   r-fill-back-screen r-draw-view-border r-video-erase
   ;; r_sky
   SKYFLATNAME skyflatnum skytexture
   ;; r_things
   r-init-sprites sprites numsprites garbage-mask)
  (import (chezscheme) (doom global) (doom struct) (doom doomdef) (doom doomdata)
          (doom doomstat) (doom m_fixed) (doom m_bbox) (doom tables) (doom r_defs)
          (doom r_state) (doom r_data) (doom p_mobj.h) (doom d_player) (doom info)
          (doom v_video) (doom w_wad) (doom i_system))

  (include "r_main.c.ss")
  (include "r_draw.c.ss")
  (include "r_sky.c.ss")
  (include "r_plane.c.ss")
  (include "r_segs.c.ss")
  (include "r_bsp.c.ss")
  (include "r_things.c.ss"))
