;;; r_bsp.c: BSP traversal and the solid-wall clip list. Included by
;;; r_local.sls.

;; drawseg_t and its limits (r_defs.h).
(define SIL_NONE 0)
(define SIL_BOTTOM 1)
(define SIL_TOP 2)
(define SIL_BOTH 3)
(define MAXDRAWSEGS 256)

;; sprtopclip, sprbottomclip and maskedtexturecol are offsets into clipmem
;; (#f for NULL), adjusted so [x1] is the first value.
(define-struct drawseg ()
  (curline #f) (x1 0) (x2 0)
  (scale1 0) (scale2 0) (scalestep 0)
  (silhouette 0)          ; 0=none, 1=bottom, 2=top, 3=both
  (bsilheight 0)          ; do not clip sprites above this
  (tsilheight 0)          ; do not clip sprites below this
  (sprtopclip #f) (sprbottomclip #f) (maskedtexturecol #f))

(define curline #f)
(define sidedef #f)
(define linedef #f)
(define frontsector #f)
(define backsector #f)

;; Persistent like C's static array: fields a frame doesn't set keep their
;; old values.
(define drawsegs
  (let ([v (make-vector MAXDRAWSEGS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXDRAWSEGS) v)
      (vector-set! v i (make-drawseg)))))
(define ds-p 0)                     ; index of the next free drawseg

(define (r-clear-draw-segs)
  (set! ds-p 0))

;; ClipWallSegment: clips the given range of columns and includes it in the
;; new clip list. solidsegs is cliprange_t[MAXSEGS] as two fxvectors.
(define MAXSEGS 32)
(define solidsegs-first (make-fxvector MAXSEGS 0))
(define solidsegs-last (make-fxvector MAXSEGS 0))
;; newend is one past the last valid seg
(define newend 0)
(define-syntax seg-first (syntax-rules () [(_ i) (fxvector-ref solidsegs-first i)]))
(define-syntax seg-last (syntax-rules () [(_ i) (fxvector-ref solidsegs-last i)]))
(define (seg-set! i first last)
  (fxvector-set! solidsegs-first i first)
  (fxvector-set! solidsegs-last i last))
(define (seg-copy! dst src)
  (seg-set! dst (seg-first src) (seg-last src)))

;; R_ClipSolidWallSegment: does handle solid walls, e.g. single sided
;; LineDefs (middle texture) that entirely block the view.
(define (r-clip-solid-wall-segment first last)
  ;; Find the first range that touches the range (adjacent pixels are
  ;; touching).
  (let ([start (let loop ([s 0]) (if (fx< (seg-last s) (fx- first 1)) (loop (fx+ s 1)) s))])
    (define (crunch next)
      ;; Remove start+1 to next from the clip list, because start now covers
      ;; their area. (C's `while (next++ != newend) *++start = *next;`
      ;; copies one entry past the old end too, which then stays in the
      ;; list.)
      (unless (fx= next start)
        (let loop ([next next] [start start])
          (if (fx= next newend)
              (set! newend (fx+ start 1))
              (let ([next (fx+ next 1)] [start (fx+ start 1)])
                (seg-copy! start next)
                (loop next start))))))
    (if (and (fx< first (seg-first start)) (fx< last (fx- (seg-first start) 1)))
        ;; Post is entirely visible (above start), so insert a new clippost.
        (begin
          (r-store-wall-range first last)
          (let loop ([next newend])
            (if (fx= next start)
                (seg-set! next first last)
                (begin (seg-copy! next (fx- next 1))
                       (loop (fx- next 1)))))
          (set! newend (fx+ newend 1)))
        (begin
          (when (fx< first (seg-first start))
            ;; There is a fragment above *start.
            (r-store-wall-range first (fx- (seg-first start) 1))
            ;; Now adjust the clip size.
            (fxvector-set! solidsegs-first start first))
          ;; Bottom contained in start?
          (unless (fx<= last (seg-last start))
            (let loop ([next start])
              (if (fx>= last (fx- (seg-first (fx+ next 1)) 1))
                  ;; There is a fragment between two posts.
                  (begin
                    (r-store-wall-range (fx+ (seg-last next) 1) (fx- (seg-first (fx+ next 1)) 1))
                    (let ([next (fx+ next 1)])
                      (if (fx<= last (seg-last next))
                          ;; Bottom is contained in next. Adjust the clip size.
                          (begin (fxvector-set! solidsegs-last start (seg-last next))
                                 (crunch next))
                          (loop next))))
                  ;; There is a fragment after *next.
                  (begin
                    (r-store-wall-range (fx+ (seg-last next) 1) last)
                    ;; Adjust the clip size.
                    (fxvector-set! solidsegs-last start last)
                    (crunch next)))))))))

;; R_ClipPassWallSegment: clips the given range of columns, but does not
;; includes it in the clip list. Does handle windows, e.g. LineDefs with
;; upper and lower texture.
(define (r-clip-pass-wall-segment first last)
  (let ([start (let loop ([s 0]) (if (fx< (seg-last s) (fx- first 1)) (loop (fx+ s 1)) s))])
    (if (and (fx< first (seg-first start)) (fx< last (fx- (seg-first start) 1)))
        ;; Post is entirely visible (above start).
        (r-store-wall-range first last)
        (begin
          (when (fx< first (seg-first start))
            ;; There is a fragment above *start.
            (r-store-wall-range first (fx- (seg-first start) 1)))
          ;; Bottom contained in start?
          (unless (fx<= last (seg-last start))
            (let loop ([start start])
              (if (fx>= last (fx- (seg-first (fx+ start 1)) 1))
                  ;; There is a fragment between two posts.
                  (begin
                    (r-store-wall-range (fx+ (seg-last start) 1) (fx- (seg-first (fx+ start 1)) 1))
                    (let ([start (fx+ start 1)])
                      (unless (fx<= last (seg-last start))
                        (loop start))))
                  ;; There is a fragment after *next.
                  (r-store-wall-range (fx+ (seg-last start) 1) last))))))))

(define (r-clear-clip-segs)
  (seg-set! 0 -2147483647 -1)
  (seg-set! 1 viewwidth #x7fffffff)
  (set! newend 2))

;; Clip an angle range to the view edges (shared by R_AddLine and
;; R_CheckBBox). angle1 and angle2 are relative to viewangle. Returns the
;; clipped (values angle1 angle2), or #f if the range is totally off an
;; edge.
(define (clip-to-view angle1 angle2 span)
  (let* ([clip2 (fxand (fx* 2 clipangle) #xFFFFFFFF)]
         [tspan (angle+ angle1 clipangle)])
    (if (and (fx> tspan clip2) (fx>= (angle- tspan clip2) span))
        ;; Totally off the left edge?
        (values #f 0)
        (let ([angle1 (if (fx> tspan clip2) clipangle angle1)]
              [tspan (angle- clipangle angle2)])
          (if (and (fx> tspan clip2) (fx>= (angle- tspan clip2) span))
              ;; Totally off the left edge?
              (values #f 0)
              (values angle1 (if (fx> tspan clip2) (uneg clipangle) angle2)))))))

;; R_AddLine: clips the given segment and adds any visible pieces to the
;; line list.
(define (r-add-line line)
  (set! curline line)
  ;; OPTIMIZE: quickly reject orthogonal back sides.
  (let* ([angle1 (r-point-to-angle (vertex-x (seg-v1 line)) (vertex-y (seg-v1 line)))]
         [angle2 (r-point-to-angle (vertex-x (seg-v2 line)) (vertex-y (seg-v2 line)))]
         ;; Clip to view edges.
         [span (angle- angle1 angle2)])
    ;; Back side? I.e. backface culling?
    (unless (fx>= span ANG180)
      ;; Global angle needed by segcalc.
      (set! rw-angle1 angle1)
      (let-values ([(angle1 angle2) (clip-to-view (angle- angle1 viewangle)
                                                  (angle- angle2 viewangle) span)])
        ;; The seg is in the view range, but not necessarily visible.
        (when angle1
          (let ([x1 (fxvector-ref viewangletox (fxsrl (angle+ angle1 ANG90) ANGLETOFINESHIFT))]
                [x2 (fxvector-ref viewangletox (fxsrl (angle+ angle2 ANG90) ANGLETOFINESHIFT))])
            ;; Does not cross a pixel?
            (unless (fx= x1 x2)
              (set! backsector (seg-backsector line))
              (cond
                ;; Single sided line?
                [(not backsector) (r-clip-solid-wall-segment x1 (fx- x2 1))]
                ;; Closed door.
                [(or (fx<= (sector-ceilingheight backsector) (sector-floorheight frontsector))
                     (fx>= (sector-floorheight backsector) (sector-ceilingheight frontsector)))
                 (r-clip-solid-wall-segment x1 (fx- x2 1))]
                ;; Window.
                [(or (not (fx= (sector-ceilingheight backsector) (sector-ceilingheight frontsector)))
                     (not (fx= (sector-floorheight backsector) (sector-floorheight frontsector))))
                 (r-clip-pass-wall-segment x1 (fx- x2 1))]
                ;; Reject empty lines used for triggers and special events.
                ;; Identical floor and ceiling on both sides, identical light
                ;; levels on both sides, and no middle texture.
                [(and (fx= (sector-ceilingpic backsector) (sector-ceilingpic frontsector))
                      (fx= (sector-floorpic backsector) (sector-floorpic frontsector))
                      (fx= (sector-lightlevel backsector) (sector-lightlevel frontsector))
                      (fx= (side-midtexture (seg-sidedef curline)) 0))
                 (void)]
                [else (r-clip-pass-wall-segment x1 (fx- x2 1))]))))))))

;; R_CheckBBox: checks BSP node/subtree bounding box. Returns true if some
;; part of the bbox might be visible.
(define checkcoord
  (vector (fxvector 3 0 2 1) (fxvector 3 0 2 0) (fxvector 3 1 2 0) (fxvector 0 0 0 0)
          (fxvector 2 0 2 1) (fxvector 0 0 0 0) (fxvector 3 1 3 0) (fxvector 0 0 0 0)
          (fxvector 2 0 3 1) (fxvector 2 1 3 1) (fxvector 2 1 3 0) (fxvector 0 0 0 0)))

(define (r-check-bbox bspcoord)
  ;; Find the corners of the box that define the edges from current
  ;; viewpoint.
  (let* ([boxx (cond [(fx<= viewx (fxvector-ref bspcoord BOXLEFT)) 0]
                     [(fx< viewx (fxvector-ref bspcoord BOXRIGHT)) 1]
                     [else 2])]
         [boxy (cond [(fx>= viewy (fxvector-ref bspcoord BOXTOP)) 0]
                     [(fx> viewy (fxvector-ref bspcoord BOXBOTTOM)) 1]
                     [else 2])]
         [boxpos (fx+ (fxsll boxy 2) boxx)])
    (if (fx= boxpos 5)
        #t
        (let* ([cc (vector-ref checkcoord boxpos)]
               [x1 (fxvector-ref bspcoord (fxvector-ref cc 0))]
               [y1 (fxvector-ref bspcoord (fxvector-ref cc 1))]
               [x2 (fxvector-ref bspcoord (fxvector-ref cc 2))]
               [y2 (fxvector-ref bspcoord (fxvector-ref cc 3))]
               ;; check clip list for an open space
               [angle1 (angle- (r-point-to-angle x1 y1) viewangle)]
               [angle2 (angle- (r-point-to-angle x2 y2) viewangle)]
               [span (angle- angle1 angle2)])
          ;; Sitting on a line?
          (if (fx>= span ANG180)
              #t
              (let-values ([(angle1 angle2) (clip-to-view angle1 angle2 span)])
                (and angle1
                     ;; Find the first clippost that touches the source post
                     ;; (adjacent pixels are touching).
                     (let ([sx1 (fxvector-ref viewangletox (fxsrl (angle+ angle1 ANG90) ANGLETOFINESHIFT))]
                           [sx2 (fxvector-ref viewangletox (fxsrl (angle+ angle2 ANG90) ANGLETOFINESHIFT))])
                       ;; Does not cross a pixel.
                       (and (not (fx= sx1 sx2))
                            (let* ([sx2 (fx- sx2 1)]
                                   [start (let loop ([s 0])
                                            (if (fx< (seg-last s) sx2) (loop (fx+ s 1)) s))])
                              ;; The clippost contains the new span?
                              (not (and (fx>= sx1 (seg-first start))
                                        (fx<= sx2 (seg-last start))))))))))))))

;; R_Subsector: determine floor/ceiling planes, add sprites of things in
;; sector, draw one or more line segments.
(define (r-subsector num)
  (when (fx>= num numsubsectors)
    (i-error "R_Subsector: ss ~a with numss = ~a" num numsubsectors))
  (set! sscount (fx+ sscount 1))
  (let ([sub (vector-ref subsectors num)])
    (set! frontsector (subsector-sector sub))
    (set! floorplane
          (and (fx< (sector-floorheight frontsector) viewz)
               (r-find-plane (sector-floorheight frontsector) (sector-floorpic frontsector)
                             (sector-lightlevel frontsector))))
    (set! ceilingplane
          (and (or (fx> (sector-ceilingheight frontsector) viewz)
                   (fx= (sector-ceilingpic frontsector) skyflatnum))
               (r-find-plane (sector-ceilingheight frontsector) (sector-ceilingpic frontsector)
                             (sector-lightlevel frontsector))))
    (r-add-sprites frontsector)
    (let ([first (subsector-firstline sub)])
      (do ([i 0 (fx+ i 1)]) ((fx= i (subsector-numlines sub)))
        (r-add-line (vector-ref segs (fx+ first i)))))))

;; RenderBSPNode: renders all subsectors below a given node, traversing
;; subtree recursively. Just call with BSP root.
(define (r-render-bsp-node bspnum)
  (if (fxlogtest bspnum NF_SUBSECTOR)
      ;; Found a subsector?
      (if (fx= bspnum -1)
          (r-subsector 0)
          (r-subsector (fxand bspnum (fxnot NF_SUBSECTOR))))
      (let* ([bsp (vector-ref nodes bspnum)]
             ;; Decide which side the view point is on.
             [side (r-point-on-side viewx viewy bsp)])
        ;; Recursively divide front space.
        (r-render-bsp-node (fxvector-ref (node-children bsp) side))
        ;; Possibly divide back space.
        (when (r-check-bbox (vector-ref (node-bbox bsp) (fxxor side 1)))
          (r-render-bsp-node (fxvector-ref (node-children bsp) (fxxor side 1)))))))
