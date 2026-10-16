;;; r_things.c: sprites. Included by r_local.sls.

(define MAXVISSPRITES 128)
(define MINZ (* FRACUNIT 4))
(define BASEYCENTER 100)

;; Sprite rotation 0 is facing the viewer, rotation 1 is one angle turn
;; CLOCKWISE around the axis. This is not the same as the angle, which
;; increases counter clockwise (protractor).
(define pspritescale 0)
(define pspriteiscale 0)

(define spritelights (vector-ref scalelight 0))

;; spriteframe_t. rotate is -1 (not seen yet, C's memset -1), 0 or 1;
;; lump[8] are sprite lump numbers relative to firstspritelump.
(define-struct spriteframe ()
  (rotate -1) (lump (make-fxvector 8 -1)) (flip (make-fxvector 8 #xff)))
;; spritedef_t: spriteframes is a vector of spriteframe.
(define-struct spritedef () (numframes 0) (spriteframes (vector)))

;; variables used to look up and range check thing_t sprites patches
(define-global sprites (vector))
(define-global numsprites 0)

(define sprtemp
  (let ([v (make-vector 29)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 29) v)
      (vector-set! v i (make-spriteframe)))))
(define maxframe -1)
(define spritename "")

;; R_InstallSpriteLump: local function for R_InitSprites.
(define (r-install-sprite-lump lump frame rotation flipped)
  (when (or (not (fx< -1 frame 29)) (not (fx< -1 rotation 9)))
    (i-error "R_InstallSpriteLump: Bad frame characters in lump ~a" lump))
  (when (fx> frame maxframe)
    (set! maxframe frame))
  (let ([f (vector-ref sprtemp frame)]
        [frame-char (integer->char (fx+ 65 frame))])
    (if (fx= rotation 0)
        ;; the lump should be used for all rotations
        (begin
          (when (fx= (spriteframe-rotate f) 0)
            (i-error "R_InitSprites: Sprite ~a frame ~a has multip rot=0 lump"
                     spritename frame-char))
          (when (fx= (spriteframe-rotate f) 1)
            (i-error "R_InitSprites: Sprite ~a frame ~a has rotations and a rot=0 lump"
                     spritename frame-char))
          (spriteframe-rotate-set! f 0)
          (do ([r 0 (fx+ r 1)]) ((fx= r 8))
            (fxvector-set! (spriteframe-lump f) r (fx- lump firstspritelump))
            (fxvector-set! (spriteframe-flip f) r (if flipped 1 0))))
        ;; the lump is only used for one rotation
        (begin
          (when (fx= (spriteframe-rotate f) 0)
            (i-error "R_InitSprites: Sprite ~a frame ~a has rotations and a rot=0 lump"
                     spritename frame-char))
          (spriteframe-rotate-set! f 1)
          ;; make 0 based
          (let ([rotation (fx- rotation 1)])
            (unless (fx= (fxvector-ref (spriteframe-lump f) rotation) -1)
              (i-error "R_InitSprites: Sprite ~a : ~a : ~a has two lumps mapped to it"
                       spritename frame-char (integer->char (fx+ 49 rotation))))
            (fxvector-set! (spriteframe-lump f) rotation (fx- lump firstspritelump))
            (fxvector-set! (spriteframe-flip f) rotation (if flipped 1 0)))))))

;; R_InitSpriteDefs: builds the sprite rotation matrixes to account for
;; horizontally flipped sprites from a list of 4-character sprite names.
;; Sprite lump names are 4 characters for the actor, a letter for the
;; frame, and a number for the rotation. A sprite that is flippable will
;; have an additional letter/number appended. The rotation character can
;; be 0 to signify no rotations.
(define (r-init-sprite-defs namelist)
  (set! numsprites (vector-length namelist))
  (unless (fx= numsprites 0)
    (set! sprites (make-vector numsprites #f))
    ;; scan all the lump names for each of the names, noting the highest
    ;; frame letter. (C compares the first 4 bytes as an int.)
    (do ([i 0 (fx+ i 1)]) ((fx= i numsprites))
      (set! spritename (vector-ref namelist i))
      (vector-for-each
       (lambda (f)
         (spriteframe-rotate-set! f -1)
         (fxvector-fill! (spriteframe-lump f) -1)
         (fxvector-fill! (spriteframe-flip f) #xff))
       sprtemp)
      (set! maxframe -1)
      (do ([l firstspritelump (fx+ l 1)]) ((fx> l lastspritelump))
        (let ([name (w-lump-name l)])
          (define (ch k) (if (fx< k (string-length name)) (char->integer (string-ref name k)) 0))
          (when (and (fx>= (string-length name) 4)
                     (string=? (substring name 0 4) spritename))
            (let ([patched (if modifiedgame (w-get-num-for-name name) l)])
              (r-install-sprite-lump patched (fx- (ch 4) 65) (fx- (ch 5) 48) #f)
              (unless (fx= (ch 6) 0)
                (r-install-sprite-lump l (fx- (ch 6) 65) (fx- (ch 7) 48) #t))))))
      ;; check the frames that were found for completeness
      (if (fx= maxframe -1)
          (vector-set! sprites i (make-spritedef 0 (vector)))
          (let ([n (fx+ maxframe 1)])
            (do ([frame 0 (fx+ frame 1)]) ((fx= frame n))
              (let ([f (vector-ref sprtemp frame)])
                (case (spriteframe-rotate f)
                  [(-1)
                   ;; no rotations were found for that frame at all
                   (i-error "R_InitSprites: No patches found for ~a frame ~a"
                            spritename (integer->char (fx+ frame 65)))]
                  [(1)
                   ;; must have all 8 frames
                   (do ([r 0 (fx+ r 1)]) ((fx= r 8))
                     (when (fx= (fxvector-ref (spriteframe-lump f) r) -1)
                       (i-error "R_InitSprites: Sprite ~a frame ~a is missing rotations"
                                spritename (integer->char (fx+ frame 65)))))]
                  [else (void)])))
            ;; allocate space for the frames present and copy sprtemp to it
            (vector-set! sprites i
                         (make-spritedef
                          n
                          (let ([v (make-vector n)])
                            (do ([frame 0 (fx+ frame 1)]) ((fx= frame n) v)
                              (let ([f (vector-ref sprtemp frame)])
                                (vector-set! v frame
                                             (make-spriteframe
                                              (spriteframe-rotate f)
                                              (fxvector-copy (spriteframe-lump f))
                                              (fxvector-copy (spriteframe-flip f))))))))))))))

;; vissprite_t: a thing that will be drawn during a refresh. colormap is an
;; offset into colormaps, or #f for shadow draw.
(define-struct vissprite ()
  (x1 0) (x2 0)
  (gx 0) (gy 0)               ; for line side calculation
  (gz 0) (gzt 0)              ; global bottom / top for silhouette clipping
  (startfrac 0)               ; horizontal position of x1
  (scale 0)
  (xiscale 0)                 ; negative if flipped
  (texturemid 0)
  (patch 0)
  (colormap 0)
  (mobjflags 0))

(define vissprites
  (let ([v (make-vector MAXVISSPRITES)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXVISSPRITES) v)
      (vector-set! v i (make-vissprite)))))
(define vissprite-p 0)
(define overflowsprite (make-vissprite))

;; R_InitSprites: called at program start.
(define (r-init-sprites namelist)
  (do ([i 0 (fx+ i 1)]) ((fx= i SCREENWIDTH))
    (fxvector-set! clipmem (fx+ negonearray i) -1))
  (r-init-sprite-defs namelist))

;; R_ClearSprites: called at frame start.
(define (r-clear-sprites)
  (set! vissprite-p 0))

(define (r-new-vis-sprite)
  (if (fx= vissprite-p MAXVISSPRITES)
      overflowsprite
      (begin (set! vissprite-p (fx+ vissprite-p 1))
             (vector-ref vissprites (fx- vissprite-p 1)))))

;; Garbage-read tracking, for tests. In low detail, R_DrawColumnLow doubles
;; the global dc_x, so later posts of a column read the clip arrays at
;; doubled indexes. Outside the range the arrays were filled for, C reads
;; memory the port can't reproduce: R_DrawSprite's uninitialized stack
;; arrays, or globals that happen to follow negonearray /
;; screenheightarray. When garbage-mask is a bytevector, every post whose
;; clipping reads there marks the rows its sprite (or masked seg) could
;; draw on, across the whole width; tests/render.ss allows differences from
;; the oracle only in marked pixels. See DESIGN.md, "Garbage reads".
(define-global garbage-mask #f)
(define clip-lo 0)              ; valid clip index range for sprites,
(define clip-hi -1)             ;   or clip-lo = #f for masked segs
(define garbage-top 0)          ; rows to mark
(define garbage-bottom -1)

(define (clip-garbage? base x)
  (cond
    [clip-lo (not (fx<= clip-lo x clip-hi))]
    [(or (fx= base negonearray) (fx= base screenheightarray))
     (not (fx< -1 x SCREENWIDTH))]
    [else (not (fx<= openings (fx+ base x) (fx+ openings MAXOPENINGS -1)))]))

(define (mark-garbage!)
  (let ([mask garbage-mask])
    (do ([y (fxmax garbage-top 0) (fx+ y 1)]) ((fx> y (fxmin garbage-bottom (fx- viewheight 1))))
      (let ([row (fx* (fx+ y viewwindowy) SCREENWIDTH)])
        (do ([x 0 (fx+ x 1)]) ((fx= x SCREENWIDTH))
          (bytevector-u8-set! mask (fx+ row x) 1))))))

;; R_DrawMaskedColumn: used for sprites and masked mid textures. Masked
;; means: partly transparent, i.e. stored in posts/runs of opaque pixels.
;; mfloorclip / mceilingclip are offsets into clipmem.
(define mfloorclip 0)
(define mceilingclip 0)
(define spryscale 0)
(define sprtopscreen 0)

(define (r-draw-masked-column bv column)
  (let ([basetexturemid dc-texturemid])
    (let loop ([column column])
      ;; (a post header read past the lump ends the column)
      (let ([topdelta (if (fx< -1 column (bytevector-length bv)) (bytevector-u8-ref bv column) #xff)])
        (unless (fx= topdelta #xff)
          (let* ([length (src-ref bv (fx+ column 1))]
                 ;; calculate unclipped screen coordinates for post
                 [topscreen (i+ sprtopscreen (int32 (fx* spryscale topdelta)))]
                 [bottomscreen (i+ topscreen (int32 (fx* spryscale length)))])
            (set! dc-yl (fxsra (i+ topscreen (fx- FRACUNIT 1)) FRACBITS))
            (set! dc-yh (fxsra (i- bottomscreen 1) FRACBITS))
            (when (and garbage-mask
                       (or (clip-garbage? mfloorclip dc-x) (clip-garbage? mceilingclip dc-x)))
              (mark-garbage!))
            (let ([fclip (fxvector-ref clipmem (fx+ mfloorclip dc-x))]
                  [cclip (fxvector-ref clipmem (fx+ mceilingclip dc-x))])
              (when (fx>= dc-yh fclip) (set! dc-yh (fx- fclip 1)))
              (when (fx<= dc-yl cclip) (set! dc-yl (fx+ cclip 1))))
            (when (fx<= dc-yl dc-yh)
              (set! dc-source bv)
              (set! dc-source-ofs (fx+ column 3))
              (set! dc-texturemid (i- basetexturemid (fxsll topdelta FRACBITS)))
              ;; Drawn by either R_DrawColumn or (SHADOW) R_DrawFuzzColumn.
              (colfunc))
            (loop (fx+ column length 4))))))
    (set! dc-texturemid basetexturemid)))

;; R_DrawVisSprite: mfloorclip and mceilingclip should also be set.
(define (r-draw-vis-sprite vis x1 x2)
  (let ([patch (w-cache-lump-num (fx+ (vissprite-patch vis) firstspritelump))])
    (set! dc-colormap (vissprite-colormap vis))
    (cond
      [(not dc-colormap)
       ;; NULL colormap = shadow draw
       (set! colfunc fuzzcolfunc)]
      [(fxlogtest (vissprite-mobjflags vis) MF_TRANSLATION)
       (set! colfunc r-draw-translated-column)
       (set! dc-translation (fx+ -256 (fxsra (fxand (vissprite-mobjflags vis) MF_TRANSLATION)
                                             (fx- MF_TRANSSHIFT 8))))])
    (set! dc-iscale (fxsra (c-abs (vissprite-xiscale vis)) detailshift))
    (set! dc-texturemid (vissprite-texturemid vis))
    (set! spryscale (vissprite-scale vis))
    (set! sprtopscreen (i- centeryfrac (fixed-mul dc-texturemid spryscale)))
    (when garbage-mask
      (set! garbage-top (fxsra sprtopscreen FRACBITS))
      (set! garbage-bottom
            (fxsra (+ sprtopscreen (* spryscale (bytevector-s16-ref patch 2 (endianness little))))
                   FRACBITS)))
    (let ([width (bytevector-s16-ref patch 0 (endianness little))]
          [xiscale (vissprite-xiscale vis)])
      (set! dc-x (vissprite-x1 vis))
      (let loop ([frac (vissprite-startfrac vis)])
        (when (fx<= dc-x (vissprite-x2 vis))
          (let ([texturecolumn (fxsra frac FRACBITS)])
            (when (or (fx< texturecolumn 0) (fx>= texturecolumn width))
              (i-error "R_DrawSpriteRange: bad texturecolumn"))
            (r-draw-masked-column
             patch (bytevector-s32-ref patch (fx+ 8 (fx* texturecolumn 4)) (endianness little))))
          (set! dc-x (fx+ dc-x 1))
          (loop (i+ frac xiscale)))))
    (set! colfunc basecolfunc)))

;; R_ProjectSprite: generates a vissprite for a thing if it might be
;; visible.
(define (r-project-sprite thing)
  ;; transform the origin point
  (let* ([tr-x (i- (mobj-x thing) viewx)]
         [tr-y (i- (mobj-y thing) viewy)]
         [gxt (fixed-mul tr-x viewcos)]
         [gyt (ineg (fixed-mul tr-y viewsin))]
         [tz (i- gxt gyt)])
    ;; thing is behind view plane?
    (unless (fx< tz MINZ)
      (let* ([xscale (fixed-div projection tz)]
             [gxt (ineg (fixed-mul tr-x viewsin))]
             [gyt (fixed-mul tr-y viewcos)]
             [tx (ineg (i+ gyt gxt))])
        ;; too far off the side?
        (unless (fx> (c-abs tx) (int32 (fxsll tz 2)))
          ;; decide which patch to use for sprite relative to player
          (unless (fx< -1 (mobj-sprite thing) numsprites)
            (i-error "R_ProjectSprite: invalid sprite number ~a " (mobj-sprite thing)))
          (let ([sprdef (vector-ref sprites (mobj-sprite thing))])
            (unless (fx< (fxand (mobj-frame thing) FF_FRAMEMASK) (spritedef-numframes sprdef))
              (i-error "R_ProjectSprite: invalid sprite frame ~a : ~a "
                       (mobj-sprite thing) (mobj-frame thing)))
            (let* ([sprframe (vector-ref (spritedef-spriteframes sprdef)
                                         (fxand (mobj-frame thing) FF_FRAMEMASK))]
                   [rot (if (fx= (spriteframe-rotate sprframe) 0)
                            ;; use single rotation for all views
                            0
                            ;; choose a different rotation based on player view
                            (let ([ang (r-point-to-angle (mobj-x thing) (mobj-y thing))])
                              (fxsrl (angle+ (angle- ang (mobj-angle thing))
                                             (fx* (fxquotient ANG45 2) 9))
                                     29)))]
                   [lump (fxvector-ref (spriteframe-lump sprframe) rot)]
                   [flip (not (fx= (fxvector-ref (spriteframe-flip sprframe) rot) 0))]
                   ;; calculate edges of the shape
                   [tx (i- tx (fxvector-ref spriteoffset lump))]
                   [x1 (fxsra (i+ centerxfrac (fixed-mul tx xscale)) FRACBITS)])
              ;; off the right side?
              (unless (fx> x1 viewwidth)
                (let* ([tx (i+ tx (fxvector-ref spritewidth lump))]
                       [x2 (fx- (fxsra (i+ centerxfrac (fixed-mul tx xscale)) FRACBITS) 1)])
                  ;; off the left side
                  (unless (fx< x2 0)
                    ;; store information in a vissprite
                    (let ([vis (r-new-vis-sprite)]
                          [iscale (fixed-div FRACUNIT xscale)])
                      (vissprite-mobjflags-set! vis (mobj-flags thing))
                      (vissprite-scale-set! vis (int32 (fxsll xscale detailshift)))
                      (vissprite-gx-set! vis (mobj-x thing))
                      (vissprite-gy-set! vis (mobj-y thing))
                      (vissprite-gz-set! vis (mobj-z thing))
                      (vissprite-gzt-set! vis (i+ (mobj-z thing) (fxvector-ref spritetopoffset lump)))
                      (vissprite-texturemid-set! vis (i- (vissprite-gzt vis) viewz))
                      (vissprite-x1-set! vis (if (fx< x1 0) 0 x1))
                      (vissprite-x2-set! vis (if (fx>= x2 viewwidth) (fx- viewwidth 1) x2))
                      (if flip
                          (begin (vissprite-startfrac-set! vis (fx- (fxvector-ref spritewidth lump) 1))
                                 (vissprite-xiscale-set! vis (ineg iscale)))
                          (begin (vissprite-startfrac-set! vis 0)
                                 (vissprite-xiscale-set! vis iscale)))
                      (when (fx> (vissprite-x1 vis) x1)
                        (vissprite-startfrac-set!
                         vis (i+ (vissprite-startfrac vis)
                                 (int32 (fx* (vissprite-xiscale vis) (fx- (vissprite-x1 vis) x1))))))
                      (vissprite-patch-set! vis lump)
                      ;; get light level
                      (vissprite-colormap-set!
                       vis
                       (cond
                         ;; shadow draw
                         [(fxlogtest (mobj-flags thing) MF_SHADOW) #f]
                         ;; fixed map
                         [fixedcolormap fixedcolormap]
                         ;; full bright
                         [(fxlogtest (mobj-frame thing) FF_FULLBRIGHT) 0]
                         ;; diminished light
                         [else
                          (let ([index (fxsra xscale (fx- LIGHTSCALESHIFT detailshift))])
                            (fxvector-ref spritelights (if (fx>= index MAXLIGHTSCALE)
                                                           (fx- MAXLIGHTSCALE 1)
                                                           index)))])))))))))))))

;; R_AddSprites: during BSP traversal, this adds sprites by sector.
(define (r-add-sprites sec)
  ;; BSP is traversed by subsector. A sector might have been split into
  ;; several subsectors during BSP building. Thus we check whether its
  ;; already added.
  (unless (fx= (sector-validcount sec) validcount)
    ;; Well, now it will be done.
    (sector-validcount-set! sec validcount)
    (let ([lightnum (fx+ (fxsra (sector-lightlevel sec) LIGHTSEGSHIFT) extralight)])
      (set! spritelights (vector-ref scalelight (cond [(fx< lightnum 0) 0]
                                                      [(fx>= lightnum LIGHTLEVELS) (fx- LIGHTLEVELS 1)]
                                                      [else lightnum]))))
    ;; Handle all things in sector.
    (let loop ([thing (sector-thinglist sec)])
      (when thing
        (r-project-sprite thing)
        (loop (mobj-snext thing))))))

;; R_DrawPSprite
(define avis (make-vissprite))
(define (r-draw-psprite psp)
  ;; decide which patch to use
  (let ([state (pspdef-state psp)])
    (unless (fx< -1 (state-sprite state) numsprites)
      (i-error "R_ProjectSprite: invalid sprite number ~a " (state-sprite state)))
    (let ([sprdef (vector-ref sprites (state-sprite state))])
      (unless (fx< (fxand (state-frame state) FF_FRAMEMASK) (spritedef-numframes sprdef))
        (i-error "R_ProjectSprite: invalid sprite frame ~a : ~a "
                 (state-sprite state) (state-frame state)))
      (let* ([sprframe (vector-ref (spritedef-spriteframes sprdef)
                                   (fxand (state-frame state) FF_FRAMEMASK))]
             [lump (fxvector-ref (spriteframe-lump sprframe) 0)]
             [flip (not (fx= (fxvector-ref (spriteframe-flip sprframe) 0) 0))]
             ;; calculate edges of the shape
             [tx (i- (i- (pspdef-sx psp) (* 160 FRACUNIT)) (fxvector-ref spriteoffset lump))]
             [x1 (fxsra (i+ centerxfrac (fixed-mul tx pspritescale)) FRACBITS)])
        ;; off the right side
        (unless (fx> x1 viewwidth)
          (let* ([tx (i+ tx (fxvector-ref spritewidth lump))]
                 [x2 (fx- (fxsra (i+ centerxfrac (fixed-mul tx pspritescale)) FRACBITS) 1)])
            ;; off the left side
            (unless (fx< x2 0)
              ;; store information in a vissprite
              (let ([vis avis]
                    [invis (fxvector-ref (player-powers viewplayer) pw_invisibility)])
                (vissprite-mobjflags-set! vis 0)
                (vissprite-texturemid-set!
                 vis (i- (fx+ (fxsll BASEYCENTER FRACBITS) (fxquotient FRACUNIT 2))
                         (i- (pspdef-sy psp) (fxvector-ref spritetopoffset lump))))
                (vissprite-x1-set! vis (if (fx< x1 0) 0 x1))
                (vissprite-x2-set! vis (if (fx>= x2 viewwidth) (fx- viewwidth 1) x2))
                (vissprite-scale-set! vis (int32 (fxsll pspritescale detailshift)))
                (if flip
                    (begin (vissprite-xiscale-set! vis (ineg pspriteiscale))
                           (vissprite-startfrac-set! vis (fx- (fxvector-ref spritewidth lump) 1)))
                    (begin (vissprite-xiscale-set! vis pspriteiscale)
                           (vissprite-startfrac-set! vis 0)))
                (when (fx> (vissprite-x1 vis) x1)
                  (vissprite-startfrac-set!
                   vis (i+ (vissprite-startfrac vis)
                           (int32 (fx* (vissprite-xiscale vis) (fx- (vissprite-x1 vis) x1))))))
                (vissprite-patch-set! vis lump)
                (vissprite-colormap-set!
                 vis
                 (cond
                   ;; shadow draw
                   [(or (fx> invis (* 4 32)) (fxlogtest invis 8)) #f]
                   ;; fixed color
                   [fixedcolormap fixedcolormap]
                   ;; full bright
                   [(fxlogtest (state-frame state) FF_FULLBRIGHT) 0]
                   ;; local light
                   [else (fxvector-ref spritelights (fx- MAXLIGHTSCALE 1))]))
                (r-draw-vis-sprite vis (vissprite-x1 vis) (vissprite-x2 vis))))))))))

;; R_DrawPlayerSprites
(define (r-draw-player-sprites)
  ;; get light level
  (let ([lightnum (fx+ (fxsra (sector-lightlevel
                               (subsector-sector (mobj-subsector (player-mo viewplayer))))
                              LIGHTSEGSHIFT)
                       extralight)])
    (set! spritelights (vector-ref scalelight (cond [(fx< lightnum 0) 0]
                                                    [(fx>= lightnum LIGHTLEVELS) (fx- LIGHTLEVELS 1)]
                                                    [else lightnum]))))
  ;; clip to screen bounds
  (set! mfloorclip screenheightarray)
  (set! mceilingclip negonearray)
  (set! clip-lo 0)
  (set! clip-hi (fx- SCREENWIDTH 1))
  ;; add all active psprites
  (vector-for-each
   (lambda (psp) (when (pspdef-state psp) (r-draw-psprite psp)))
   (player-psprites viewplayer)))

;; R_SortVisSprites: the vissprites in increasing scale. C's selection sort
;; takes the first of equal scales, so this is a stable sort.
(define (r-sort-vis-sprites)
  (let loop ([unsorted (let collect ([i (fx- vissprite-p 1)] [acc '()])
                         (if (fx< i 0) acc (collect (fx- i 1) (cons (vector-ref vissprites i) acc))))]
             [sorted '()])
    (if (null? unsorted)
        (reverse sorted)
        (let ([best (let find ([l unsorted] [best #f] [bestscale MAXINT])
                      (cond [(null? l) best]
                            [(fx< (vissprite-scale (car l)) bestscale)
                             (find (cdr l) (car l) (vissprite-scale (car l)))]
                            [else (find (cdr l) best bestscale)]))])
          (loop (remq best unsorted) (cons best sorted))))))

;; R_DrawSprite
(define (r-draw-sprite spr)
  (let ([sx1 (vissprite-x1 spr)] [sx2 (vissprite-x2 spr)])
    (do ([x sx1 (fx+ x 1)]) ((fx> x sx2))
      (fxvector-set! clipmem (fx+ clipbot x) -2)
      (fxvector-set! clipmem (fx+ cliptop x) -2))
    ;; Scan drawsegs from end to start for obscuring segs. The first drawseg
    ;; that has a greater scale is the clip seg.
    (do ([i (fx- ds-p 1) (fx- i 1)]) ((fx< i 0))
      (let ([ds (vector-ref drawsegs i)])
        ;; determine if the drawseg obscures the sprite
        (unless (or (fx> (drawseg-x1 ds) sx2)
                    (fx< (drawseg-x2 ds) sx1)
                    (and (fx= (drawseg-silhouette ds) 0) (not (drawseg-maskedtexturecol ds))))
          (let* ([r1 (if (fx< (drawseg-x1 ds) sx1) sx1 (drawseg-x1 ds))]
                 [r2 (if (fx> (drawseg-x2 ds) sx2) sx2 (drawseg-x2 ds))]
                 [s1 (drawseg-scale1 ds)] [s2 (drawseg-scale2 ds)]
                 [lowscale (if (fx> s1 s2) s2 s1)]
                 [scale (if (fx> s1 s2) s1 s2)])
            (if (or (fx< scale (vissprite-scale spr))
                    (and (fx< lowscale (vissprite-scale spr))
                         (fx= 0 (r-point-on-seg-side (vissprite-gx spr) (vissprite-gy spr)
                                                     (drawseg-curline ds)))))
                ;; masked mid texture?
                (when (drawseg-maskedtexturecol ds)
                  (r-render-masked-seg-range ds r1 r2))
                ;; seg is behind sprite; else clip this piece of the sprite
                (let* ([silhouette (drawseg-silhouette ds)]
                       [silhouette (if (fx>= (vissprite-gz spr) (drawseg-bsilheight ds))
                                       (fxand silhouette (fxnot SIL_BOTTOM))
                                       silhouette)]
                       [silhouette (if (fx<= (vissprite-gzt spr) (drawseg-tsilheight ds))
                                       (fxand silhouette (fxnot SIL_TOP))
                                       silhouette)])
                  (when (fxlogtest silhouette SIL_BOTTOM)
                    ;; bottom sil
                    (do ([x r1 (fx+ x 1)]) ((fx> x r2))
                      (when (fx= (fxvector-ref clipmem (fx+ clipbot x)) -2)
                        (fxvector-set! clipmem (fx+ clipbot x)
                                       (fxvector-ref clipmem (fx+ (drawseg-sprbottomclip ds) x))))))
                  (when (fxlogtest silhouette SIL_TOP)
                    ;; top sil
                    (do ([x r1 (fx+ x 1)]) ((fx> x r2))
                      (when (fx= (fxvector-ref clipmem (fx+ cliptop x)) -2)
                        (fxvector-set! clipmem (fx+ cliptop x)
                                       (fxvector-ref clipmem (fx+ (drawseg-sprtopclip ds) x))))))))))))
    ;; all clipping has been performed, so draw the sprite
    ;; check for unclipped columns
    (do ([x sx1 (fx+ x 1)]) ((fx> x sx2))
      (when (fx= (fxvector-ref clipmem (fx+ clipbot x)) -2)
        (fxvector-set! clipmem (fx+ clipbot x) viewheight))
      (when (fx= (fxvector-ref clipmem (fx+ cliptop x)) -2)
        (fxvector-set! clipmem (fx+ cliptop x) -1)))
    (set! mfloorclip clipbot)
    (set! mceilingclip cliptop)
    (set! clip-lo sx1)
    (set! clip-hi sx2)
    (r-draw-vis-sprite spr sx1 sx2)))

;; R_DrawMasked
(define (r-draw-masked)
  ;; draw all vissprites back to front
  (for-each r-draw-sprite (r-sort-vis-sprites))
  ;; render any remaining masked mid textures
  (do ([i (fx- ds-p 1) (fx- i 1)]) ((fx< i 0))
    (let ([ds (vector-ref drawsegs i)])
      (when (drawseg-maskedtexturecol ds)
        (r-render-masked-seg-range ds (drawseg-x1 ds) (drawseg-x2 ds)))))
  ;; draw the psprites on top of everything but does not draw on side views
  (when (fx= viewangleoffset 0)
    (r-draw-player-sprites)))
