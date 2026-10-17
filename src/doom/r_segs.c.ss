;;; r_segs.c: wall segments. Included by r_local.sls.

;; True if any of the segs textures might be visible.
(define segtextured #f)
;; False if the back side is the same plane.
(define markfloor #f)
(define markceiling #f)
(define maskedtexture #f)
(define toptexture 0)
(define bottomtexture 0)
(define midtexture 0)

(define rw-normalangle 0)
;; angle to line origin
(define rw-angle1 0)

;; regular wall
(define rw-x 0)
(define rw-stopx 0)
(define rw-centerangle 0)
(define rw-offset 0)
(define rw-distance 0)
(define rw-scale 0)
(define rw-scalestep 0)
(define rw-midtexturemid 0)
(define rw-toptexturemid 0)
(define rw-bottomtexturemid 0)

(define worldtop 0)
(define worldbottom 0)
(define worldhigh 0)
(define worldlow 0)

(define pixhigh 0)
(define pixlow 0)
(define pixhighstep 0)
(define pixlowstep 0)

(define topfrac 0)
(define topstep 0)
(define bottomfrac 0)
(define bottomstep 0)

(define walllights (vector-ref scalelight 0))
;; offset into clipmem
(define maskedtexturecol 0)

(define MAXSHORT #x7fff)

;; Light row for a wall or masked seg: different light tables for
;; horizontal / vertical / diagonal.
(define (seg-lights seg sector)
  (let* ([lightnum (fx+ (fxsra (sector-lightlevel sector) LIGHTSEGSHIFT) extralight)]
         [lightnum (cond [(fx= (vertex-y (seg-v1 seg)) (vertex-y (seg-v2 seg))) (fx- lightnum 1)]
                         [(fx= (vertex-x (seg-v1 seg)) (vertex-x (seg-v2 seg))) (fx+ lightnum 1)]
                         [else lightnum])])
    (vector-ref scalelight (cond [(fx< lightnum 0) 0]
                                 [(fx>= lightnum LIGHTLEVELS) (fx- LIGHTLEVELS 1)]
                                 [else lightnum]))))

;; A scale's light index; `index` is unsigned in C.
(define-syntax scale-light-index
  (syntax-rules ()
    [(_ scale)
     (let ([index (fxsra scale (fx+ LIGHTSCALESHIFT hires-shift))])
       (if (fx< -1 index MAXLIGHTSCALE) index (fx- MAXLIGHTSCALE 1)))]))

;; R_RenderMaskedSegRange
(define (r-render-masked-seg-range ds x1 x2)
  ;; Calculate light table.
  (set! curline (drawseg-curline ds))
  (set! frontsector (seg-frontsector curline))
  (set! backsector (seg-backsector curline))
  (let ([texnum (fxvector-ref texturetranslation (side-midtexture (seg-sidedef curline)))])
    (set! walllights (seg-lights curline frontsector))
    (set! maskedtexturecol (drawseg-maskedtexturecol ds))
    (set! rw-scalestep (drawseg-scalestep ds))
    (set! spryscale (i+ (drawseg-scale1 ds) (int32 (fx* (fx- x1 (drawseg-x1 ds)) rw-scalestep))))
    (set! mfloorclip (drawseg-sprbottomclip ds))
    (set! mceilingclip (drawseg-sprtopclip ds))
    ;; (garbage tracking: check by array; a masked seg may draw anywhere)
    (set! clip-lo #f)
    (set! garbage-top 0)
    (set! garbage-bottom (fx- viewheight 1))
    ;; find positioning
    (set! dc-texturemid
          (if (fxlogtest (line-flags (seg-linedef curline)) ML_DONTPEGBOTTOM)
              (i- (i+ (fxmax (sector-floorheight frontsector) (sector-floorheight backsector))
                      (fxvector-ref textureheight texnum))
                  viewz)
              (i- (fxmin (sector-ceilingheight frontsector) (sector-ceilingheight backsector))
                  viewz)))
    (set! dc-texturemid (i+ dc-texturemid (side-rowoffset (seg-sidedef curline))))
    (when fixedcolormap
      (set! dc-colormap fixedcolormap))
    ;; draw the columns
    (set! dc-x x1)
    (let loop ()
      (when (fx<= dc-x x2)
        ;; calculate lighting
        (let ([col (fxvector-ref clipmem (fx+ maskedtexturecol dc-x))])
          (unless (fx= col MAXSHORT)
            (unless fixedcolormap
              (set! dc-colormap (fxvector-ref walllights (scale-light-index spryscale))))
            (set! sprtopscreen (i- centeryfrac (fixed-mul dc-texturemid spryscale)))
            (set! dc-iscale (fxquotient #xFFFFFFFF (fxand spryscale #xFFFFFFFF)))
            ;; draw the texture
            (let-values ([(bv ofs) (r-get-column texnum col)])
              (r-draw-masked-column bv (fx- ofs 3)))
            ;; (dc_x as left by the drawer: doubled in low detail)
            (fxvector-set! clipmem (fx+ maskedtexturecol dc-x) MAXSHORT)))
        (set! spryscale (i+ spryscale rw-scalestep))
        (set! dc-x (fx+ dc-x 1))
        (loop)))))

;; R_RenderSegLoop: draws zero, one, or two textures (and possibly a masked
;; texture) for walls. Can draw or mark the starting pixel of floor and
;; ceiling textures.
(define HEIGHTBITS 12)
(define HEIGHTUNIT (fxsll 1 HEIGHTBITS))

(define (r-render-seg-loop)
  (let loop ()
    (when (fx< rw-x rw-stopx)
      (let* ([cclip (fxvector-ref ceilingclip rw-x)]
             [fclip (fxvector-ref floorclip rw-x)]
             ;; mark floor / ceiling areas
             [yl (fxsra (i+ topfrac (fx- HEIGHTUNIT 1)) HEIGHTBITS)]
             ;; no space above wall?
             [yl (if (fx< yl (fx+ cclip 1)) (fx+ cclip 1) yl)])
        (when markceiling
          (let ([top (fx+ cclip 1)]
                [bottom (let ([b (fx- yl 1)]) (if (fx>= b fclip) (fx- fclip 1) b))])
            (when (fx<= top bottom)
              (vp-set! (visplane-top ceilingplane) rw-x top)
              (vp-set! (visplane-bottom ceilingplane) rw-x bottom))))
        (let* ([yh (fxsra bottomfrac HEIGHTBITS)]
               [yh (if (fx>= yh fclip) (fx- fclip 1) yh)])
          (when markfloor
            (let ([top (let ([t (fx+ yh 1)]) (if (fx<= t cclip) (fx+ cclip 1) t))]
                  [bottom (fx- fclip 1)])
              (when (fx<= top bottom)
                (vp-set! (visplane-top floorplane) rw-x top)
                (vp-set! (visplane-bottom floorplane) rw-x bottom))))
          ;; texturecolumn and lighting are independent of wall tiers
          (let ([texturecolumn
                 (if segtextured
                     (let ([angle (fxsrl (angle+ rw-centerangle (fxvector-ref xtoviewangle rw-x))
                                         ANGLETOFINESHIFT)])
                       ;; calculate lighting
                       (set! dc-colormap (fxvector-ref walllights (scale-light-index rw-scale)))
                       (set! dc-x rw-x)
                       (set! dc-iscale (fxquotient #xFFFFFFFF (fxand rw-scale #xFFFFFFFF)))
                       ;; calculate texture offset
                       (fxsra (i- rw-offset (fixed-mul (fxvector-ref finetangent angle) rw-distance))
                              FRACBITS))
                     0)])
            ;; draw the wall tiers
            (if (not (fx= midtexture 0))
                ;; single sided line
                (begin
                  (set! dc-yl yl)
                  (set! dc-yh yh)
                  (set! dc-texturemid rw-midtexturemid)
                  (let-values ([(bv ofs) (r-get-column midtexture texturecolumn)])
                    (set! dc-source bv)
                    (set! dc-source-ofs ofs))
                  (colfunc)
                  (fxvector-set! ceilingclip rw-x viewheight)
                  (fxvector-set! floorclip rw-x -1))
                ;; two sided line
                (begin
                  (if (not (fx= toptexture 0))
                      ;; top wall
                      (let* ([mid (fxsra pixhigh HEIGHTBITS)]
                             [mid (if (fx>= mid fclip) (fx- fclip 1) mid)])
                        (set! pixhigh (i+ pixhigh pixhighstep))
                        (if (fx>= mid yl)
                            (begin
                              (set! dc-yl yl)
                              (set! dc-yh mid)
                              (set! dc-texturemid rw-toptexturemid)
                              (let-values ([(bv ofs) (r-get-column toptexture texturecolumn)])
                                (set! dc-source bv)
                                (set! dc-source-ofs ofs))
                              (colfunc)
                              (fxvector-set! ceilingclip rw-x mid))
                            (fxvector-set! ceilingclip rw-x (fx- yl 1))))
                      ;; no top wall
                      (when markceiling
                        (fxvector-set! ceilingclip rw-x (fx- yl 1))))
                  (if (not (fx= bottomtexture 0))
                      ;; bottom wall
                      (let* ([mid (fxsra (i+ pixlow (fx- HEIGHTUNIT 1)) HEIGHTBITS)]
                             ;; no space above wall?
                             [cclip (fxvector-ref ceilingclip rw-x)]
                             [mid (if (fx<= mid cclip) (fx+ cclip 1) mid)])
                        (set! pixlow (i+ pixlow pixlowstep))
                        (if (fx<= mid yh)
                            (begin
                              (set! dc-yl mid)
                              (set! dc-yh yh)
                              (set! dc-texturemid rw-bottomtexturemid)
                              (let-values ([(bv ofs) (r-get-column bottomtexture texturecolumn)])
                                (set! dc-source bv)
                                (set! dc-source-ofs ofs))
                              (colfunc)
                              (fxvector-set! floorclip rw-x mid))
                            (fxvector-set! floorclip rw-x (fx+ yh 1))))
                      ;; no bottom wall
                      (when markfloor
                        (fxvector-set! floorclip rw-x (fx+ yh 1))))
                  (when maskedtexture
                    ;; save texturecol for backdrawing of masked mid texture
                    ;; (stored in a short)
                    (fxvector-set! clipmem (fx+ maskedtexturecol rw-x) (s16 texturecolumn))))))))
      (set! rw-scale (i+ rw-scale rw-scalestep))
      (set! topfrac (i+ topfrac topstep))
      (set! bottomfrac (i+ bottomfrac bottomstep))
      (set! rw-x (fx+ rw-x 1))
      (loop))))

;; R_StoreWallRange: a wall segment will be drawn between start and stop
;; pixels (inclusive).
(define (r-store-wall-range start stop)
  ;; don't overflow and crash
  (unless (fx= ds-p MAXDRAWSEGS)
    (when (or (fx>= start viewwidth) (fx> start stop))
      (i-error "Bad R_RenderWallRange: ~a to ~a" start stop))
    (let ([ds (vector-ref drawsegs ds-p)])
      (set! sidedef (seg-sidedef curline))
      (set! linedef (seg-linedef curline))
      ;; mark the segment as visible for auto map
      (line-flags-set! linedef (fxior (line-flags linedef) ML_MAPPED))
      ;; calculate rw_distance for scale calculation
      (set! rw-normalangle (angle+ (seg-angle curline) ANG90))
      ;; abs() of the unsigned difference taken as an int
      (let* ([offsetangle (fxand (c-abs (int32 (angle- rw-normalangle rw-angle1))) #xFFFFFFFF)]
             [offsetangle (if (fx> offsetangle ANG90) ANG90 offsetangle)]
             [distangle (angle- ANG90 offsetangle)]
             [hyp (r-point-to-dist (vertex-x (seg-v1 curline)) (vertex-y (seg-v1 curline)))]
             [sineval (fxvector-ref finesine (fxsrl distangle ANGLETOFINESHIFT))])
        (set! rw-distance (fixed-mul hyp sineval))
        (set! rw-x start)
        (drawseg-x1-set! ds start)
        (drawseg-x2-set! ds stop)
        (drawseg-curline-set! ds curline)
        (set! rw-stopx (fx+ stop 1))
        ;; calculate scale at both ends and step
        (set! rw-scale (r-scale-from-global-angle (angle+ viewangle (fxvector-ref xtoviewangle start))))
        (drawseg-scale1-set! ds rw-scale)
        (if (fx> stop start)
            (begin
              (drawseg-scale2-set! ds (r-scale-from-global-angle
                                       (angle+ viewangle (fxvector-ref xtoviewangle stop))))
              (set! rw-scalestep (fxquotient (i- (drawseg-scale2 ds) rw-scale) (fx- stop start)))
              (drawseg-scalestep-set! ds rw-scalestep))
            ;; (rw_scalestep and ds->scalestep keep their old values)
            (drawseg-scale2-set! ds (drawseg-scale1 ds)))
        ;; calculate texture boundaries and decide if floor / ceiling marks
        ;; are needed
        (set! worldtop (i- (sector-ceilingheight frontsector) viewz))
        (set! worldbottom (i- (sector-floorheight frontsector) viewz))
        (set! midtexture 0)
        (set! toptexture 0)
        (set! bottomtexture 0)
        (set! maskedtexture #f)
        (drawseg-maskedtexturecol-set! ds #f)
        (if (not backsector)
            ;; single sided line
            (begin
              (set! midtexture (fxvector-ref texturetranslation (side-midtexture sidedef)))
              ;; a single sided line is terminal, so it must mark ends
              (set! markfloor #t)
              (set! markceiling #t)
              (set! rw-midtexturemid
                    (if (fxlogtest (line-flags linedef) ML_DONTPEGBOTTOM)
                        ;; bottom of texture at bottom
                        (i- (i+ (sector-floorheight frontsector)
                                (fxvector-ref textureheight (side-midtexture sidedef)))
                            viewz)
                        ;; top of texture at top
                        worldtop))
              (set! rw-midtexturemid (i+ rw-midtexturemid (side-rowoffset sidedef)))
              (drawseg-silhouette-set! ds SIL_BOTH)
              (drawseg-sprtopclip-set! ds screenheightarray)
              (drawseg-sprbottomclip-set! ds negonearray)
              (drawseg-bsilheight-set! ds MAXINT)
              (drawseg-tsilheight-set! ds MININT))
            ;; two sided line
            (begin
              (drawseg-sprtopclip-set! ds #f)
              (drawseg-sprbottomclip-set! ds #f)
              (drawseg-silhouette-set! ds 0)
              (cond
                [(fx> (sector-floorheight frontsector) (sector-floorheight backsector))
                 (drawseg-silhouette-set! ds SIL_BOTTOM)
                 (drawseg-bsilheight-set! ds (sector-floorheight frontsector))]
                [(fx> (sector-floorheight backsector) viewz)
                 (drawseg-silhouette-set! ds SIL_BOTTOM)
                 (drawseg-bsilheight-set! ds MAXINT)])
              (cond
                [(fx< (sector-ceilingheight frontsector) (sector-ceilingheight backsector))
                 (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_TOP))
                 (drawseg-tsilheight-set! ds (sector-ceilingheight frontsector))]
                [(fx< (sector-ceilingheight backsector) viewz)
                 (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_TOP))
                 (drawseg-tsilheight-set! ds MININT)])
              (when (fx<= (sector-ceilingheight backsector) (sector-floorheight frontsector))
                (drawseg-sprbottomclip-set! ds negonearray)
                (drawseg-bsilheight-set! ds MAXINT)
                (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_BOTTOM)))
              (when (fx>= (sector-floorheight backsector) (sector-ceilingheight frontsector))
                (drawseg-sprtopclip-set! ds screenheightarray)
                (drawseg-tsilheight-set! ds MININT)
                (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_TOP)))
              (set! worldhigh (i- (sector-ceilingheight backsector) viewz))
              (set! worldlow (i- (sector-floorheight backsector) viewz))
              ;; hack to allow height changes in outdoor areas
              (when (and (fx= (sector-ceilingpic frontsector) skyflatnum)
                         (fx= (sector-ceilingpic backsector) skyflatnum))
                (set! worldtop worldhigh))
              (set! markfloor
                    (or (not (fx= worldlow worldbottom))
                        (not (fx= (sector-floorpic backsector) (sector-floorpic frontsector)))
                        (not (fx= (sector-lightlevel backsector) (sector-lightlevel frontsector)))))
              (set! markceiling
                    (or (not (fx= worldhigh worldtop))
                        (not (fx= (sector-ceilingpic backsector) (sector-ceilingpic frontsector)))
                        (not (fx= (sector-lightlevel backsector) (sector-lightlevel frontsector)))))
              (when (or (fx<= (sector-ceilingheight backsector) (sector-floorheight frontsector))
                        (fx>= (sector-floorheight backsector) (sector-ceilingheight frontsector)))
                ;; closed door
                (set! markceiling #t)
                (set! markfloor #t))
              (when (fx< worldhigh worldtop)
                ;; top texture
                (set! toptexture (fxvector-ref texturetranslation (side-toptexture sidedef)))
                (set! rw-toptexturemid
                      (if (fxlogtest (line-flags linedef) ML_DONTPEGTOP)
                          ;; top of texture at top
                          worldtop
                          ;; bottom of texture
                          (i- (i+ (sector-ceilingheight backsector)
                                  (fxvector-ref textureheight (side-toptexture sidedef)))
                              viewz))))
              (when (fx> worldlow worldbottom)
                ;; bottom texture
                (set! bottomtexture (fxvector-ref texturetranslation (side-bottomtexture sidedef)))
                (set! rw-bottomtexturemid
                      (if (fxlogtest (line-flags linedef) ML_DONTPEGBOTTOM)
                          ;; bottom of texture at bottom, top of texture at top
                          worldtop
                          ;; top of texture at top
                          worldlow)))
              (set! rw-toptexturemid (i+ rw-toptexturemid (side-rowoffset sidedef)))
              (set! rw-bottomtexturemid (i+ rw-bottomtexturemid (side-rowoffset sidedef)))
              ;; allocate space for masked texture tables
              (unless (fx= (side-midtexture sidedef) 0)
                ;; masked midtexture
                (set! maskedtexture #t)
                (set! maskedtexturecol (fx- lastopening rw-x))
                (drawseg-maskedtexturecol-set! ds maskedtexturecol)
                (set! lastopening (fx+ lastopening (fx- rw-stopx rw-x))))))
        ;; calculate rw_offset (only needed for textured lines)
        (set! segtextured (or (not (fx= midtexture 0)) (not (fx= toptexture 0))
                              (not (fx= bottomtexture 0)) maskedtexture))
        (when segtextured
          (let* ([offsetangle (angle- rw-normalangle rw-angle1)]
                 [offsetangle (if (fx> offsetangle ANG180) (uneg offsetangle) offsetangle)]
                 [offsetangle (if (fx> offsetangle ANG90) ANG90 offsetangle)]
                 [sineval (fxvector-ref finesine (fxsrl offsetangle ANGLETOFINESHIFT))]
                 [offset (fixed-mul hyp sineval)]
                 [offset (if (fx< (angle- rw-normalangle rw-angle1) ANG180) (ineg offset) offset)])
            (set! rw-offset (i+ (i+ offset (side-textureoffset sidedef)) (seg-offset curline)))
            (set! rw-centerangle (angle- (angle+ ANG90 viewangle) rw-normalangle))
            ;; calculate light table
            (unless fixedcolormap
              (set! walllights (seg-lights curline frontsector)))))
        ;; if a floor / ceiling plane is on the wrong side of the view plane,
        ;; it is definitely invisible and doesn't need to be marked.
        (when (fx>= (sector-floorheight frontsector) viewz)
          ;; above view plane
          (set! markfloor #f))
        (when (and (fx<= (sector-ceilingheight frontsector) viewz)
                   (not (fx= (sector-ceilingpic frontsector) skyflatnum)))
          ;; below view plane
          (set! markceiling #f))
        ;; calculate incremental stepping values for texture edges
        (set! worldtop (fxsra worldtop 4))
        (set! worldbottom (fxsra worldbottom 4))
        (set! topstep (ineg (fixed-mul rw-scalestep worldtop)))
        (set! topfrac (i- (fxsra centeryfrac 4) (fixed-mul worldtop rw-scale)))
        (set! bottomstep (ineg (fixed-mul rw-scalestep worldbottom)))
        (set! bottomfrac (i- (fxsra centeryfrac 4) (fixed-mul worldbottom rw-scale)))
        (when backsector
          (set! worldhigh (fxsra worldhigh 4))
          (set! worldlow (fxsra worldlow 4))
          (when (fx< worldhigh worldtop)
            (set! pixhigh (i- (fxsra centeryfrac 4) (fixed-mul worldhigh rw-scale)))
            (set! pixhighstep (ineg (fixed-mul rw-scalestep worldhigh))))
          (when (fx> worldlow worldbottom)
            (set! pixlow (i- (fxsra centeryfrac 4) (fixed-mul worldlow rw-scale)))
            (set! pixlowstep (ineg (fixed-mul rw-scalestep worldlow)))))
        ;; render it
        (when markceiling
          (set! ceilingplane (r-check-plane ceilingplane rw-x (fx- rw-stopx 1))))
        (when markfloor
          (set! floorplane (r-check-plane floorplane rw-x (fx- rw-stopx 1))))
        (r-render-seg-loop)
        ;; save sprite clipping info
        (when (and (or (fxlogtest (drawseg-silhouette ds) SIL_TOP) maskedtexture)
                   (not (drawseg-sprtopclip ds)))
          (fxvector-copy! ceilingclip start clipmem lastopening (fx- rw-stopx start))
          (drawseg-sprtopclip-set! ds (fx- lastopening start))
          (set! lastopening (fx+ lastopening (fx- rw-stopx start))))
        (when (and (or (fxlogtest (drawseg-silhouette ds) SIL_BOTTOM) maskedtexture)
                   (not (drawseg-sprbottomclip ds)))
          (fxvector-copy! floorclip start clipmem lastopening (fx- rw-stopx start))
          (drawseg-sprbottomclip-set! ds (fx- lastopening start))
          (set! lastopening (fx+ lastopening (fx- rw-stopx start))))
        (when (and maskedtexture (not (fxlogtest (drawseg-silhouette ds) SIL_TOP)))
          (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_TOP))
          (drawseg-tsilheight-set! ds MININT))
        (when (and maskedtexture (not (fxlogtest (drawseg-silhouette ds) SIL_BOTTOM)))
          (drawseg-silhouette-set! ds (fxior (drawseg-silhouette ds) SIL_BOTTOM))
          (drawseg-bsilheight-set! ds MAXINT))
        (set! ds-p (fx+ ds-p 1))))))
