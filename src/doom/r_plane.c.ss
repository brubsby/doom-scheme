;;; r_plane.c: visplanes, the floor and ceiling spans. Included by r_local.sls.

(define MAXVISPLANES 128)

;; visplane_t. top and bottom are arrays of rwidth+2 indexed from -1 (use
;; vp-ref / vp-set!): C pads both arrays with an entry on each side, and
;; R_DrawPlanes reads and writes top[minx-1] and top[maxx+1]. Only
;; top[0..rwidth-1] is ever reset, so the pads and bottom[] keep whatever
;; earlier frames left, as in C. C's entries are unsigned chars, and 0xff
;; marks an empty column; with -hires (rows past 255) they are 16 bits and
;; 0xffff marks it.
(define-struct visplane ()
  (height 0) (picnum 0) (lightlevel 0) (minx 0) (maxx 0)
  (top (make-fxvector (fx+ MAXWIDTH 2) 0))
  (bottom (make-fxvector (fx+ MAXWIDTH 2) 0)))
(define vp-empty #xff)
(define-syntax vp-ref
  (syntax-rules () [(_ v x) (fxvector-ref v (fx+ x 1))]))
(define-syntax vp-set!
  (syntax-rules () [(_ v x val) (fxvector-set! v (fx+ x 1) (fxand val vp-empty))]))
(define (vp-clear-top! pl)
  (let ([top (visplane-top pl)])
    (do ([i 1 (fx+ i 1)]) ((fx> i rwidth))
      (fxvector-set! top i vp-empty))))

(define visplanes
  (let ([v (make-vector MAXVISPLANES)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXVISPLANES) v)
      (vector-set! v i (make-visplane)))))
(define lastvisplane 0)             ; index into visplanes
(define floorplane #f)
(define ceilingplane #f)

;; The short arrays that drawsegs and sprites point into (negonearray,
;; screenheightarray, R_DrawSprite's clipbot/cliptop, openings) are
;; slices of one fxvector, clipmem; a C `short*` into them is an offset.
;; In C they are separate arrays. (Sized for -hires; C's openings[] holds
;; SCREENWIDTH*64, maxopenings.)
(define (maxopenings) (fx* rwidth 64))
(define negonearray 0)
(define screenheightarray MAXWIDTH)
(define clipbot (* 2 MAXWIDTH))
(define cliptop (* 3 MAXWIDTH))
(define openings (* 4 MAXWIDTH))
(define clipmem (make-fxvector (fx+ openings (* MAXWIDTH 64)) 0))
(define lastopening openings)
(define (s16 x) (fx- (fxand (fx+ x #x8000) #xFFFF) #x8000))

;; Clip values are the solid pixel bounding the range.
;; floorclip starts out SCREENHEIGHT, ceilingclip starts out -1.
(define floorclip (make-fxvector MAXWIDTH 0))
(define ceilingclip (make-fxvector MAXWIDTH 0))

;; spanstart holds the start of a plane span, initialized to 0 at start.
(define spanstart (make-fxvector MAXHEIGHT 0))

;; texture mapping
(define planezlight (vector-ref zlight 0))
(define planeheight 0)
(define yslope (make-fxvector MAXHEIGHT 0))
(define distscale (make-fxvector MAXWIDTH 0))
(define basexscale 0)
(define baseyscale 0)
(define cachedheight (make-fxvector MAXHEIGHT 0))
(define cacheddistance (make-fxvector MAXHEIGHT 0))
(define cachedxstep (make-fxvector MAXHEIGHT 0))
(define cachedystep (make-fxvector MAXHEIGHT 0))

;; R_InitPlanes: only at game startup.
(define (r-init-planes)
  ;; Doh!
  (void))

;; R_MapPlane: draws one span of the current plane.
(define (r-map-plane y x1 x2)
  (when (or (fx< x2 x1) (fx< x1 0) (fx>= x2 viewwidth) (not (fx<= 0 y viewheight)))
    (i-error "R_MapPlane: ~a, ~a at ~a" x1 x2 y))
  (let ([distance
         (if (fx= planeheight (fxvector-ref cachedheight y))
             (begin
               (set! ds-xstep (fxvector-ref cachedxstep y))
               (set! ds-ystep (fxvector-ref cachedystep y))
               (fxvector-ref cacheddistance y))
             (let ([distance (fixed-mul planeheight (fxvector-ref yslope y))])
               (fxvector-set! cachedheight y planeheight)
               (fxvector-set! cacheddistance y distance)
               (set! ds-xstep (fixed-mul distance basexscale))
               (fxvector-set! cachedxstep y ds-xstep)
               (set! ds-ystep (fixed-mul distance baseyscale))
               (fxvector-set! cachedystep y ds-ystep)
               distance))])
    (let ([length (fixed-mul distance (fxvector-ref distscale x1))]
          [angle (fxsrl (angle+ viewangle (fxvector-ref xtoviewangle x1)) ANGLETOFINESHIFT)])
      (set! ds-xfrac (i+ viewx (fixed-mul (finecosine angle) length)))
      (set! ds-yfrac (i- (ineg viewy) (fixed-mul (fxvector-ref finesine angle) length)))
      (set! ds-colormap
            (or fixedcolormap
                ;; index is unsigned in C
                (let ([index (fxsra distance LIGHTZSHIFT)])
                  (fxvector-ref planezlight
                                (if (fx< -1 index MAXLIGHTZ) index (fx- MAXLIGHTZ 1))))))
      (set! ds-y y)
      (set! ds-x1 x1)
      (set! ds-x2 x2)
      ;; high or low detail
      (spanfunc))))

;; R_ClearPlanes: at begining of frame.
(define (r-clear-planes)
  ;; opening / clipping determination
  (do ([i 0 (fx+ i 1)]) ((fx= i viewwidth))
    (fxvector-set! floorclip i viewheight)
    (fxvector-set! ceilingclip i -1))
  (set! lastvisplane 0)
  (set! lastopening openings)
  ;; texture calculation
  (fxvector-fill! cachedheight 0)
  ;; left to right mapping
  (let ([angle (fxsrl (angle- viewangle ANG90) ANGLETOFINESHIFT)])
    ;; scale will be unit scale at SCREENWIDTH/2 distance
    (set! basexscale (fixed-div (finecosine angle) centerxfrac))
    (set! baseyscale (ineg (fixed-div (fxvector-ref finesine angle) centerxfrac)))))

;; R_FindPlane
(define (r-find-plane height picnum lightlevel)
  (let-values ([(height lightlevel)
                (if (fx= picnum skyflatnum)
                    (values 0 0)        ; all skys map together
                    (values height lightlevel))])
    (let loop ([i 0])
      (cond
        [(fx< i lastvisplane)
         (let ([check (vector-ref visplanes i)])
           (if (and (fx= height (visplane-height check))
                    (fx= picnum (visplane-picnum check))
                    (fx= lightlevel (visplane-lightlevel check)))
               check
               (loop (fx+ i 1))))]
        [else
         (when (fx= lastvisplane MAXVISPLANES)
           (i-error "R_FindPlane: no more visplanes"))
         (let ([check (vector-ref visplanes lastvisplane)])
           (set! lastvisplane (fx+ lastvisplane 1))
           (visplane-height-set! check height)
           (visplane-picnum-set! check picnum)
           (visplane-lightlevel-set! check lightlevel)
           (visplane-minx-set! check rwidth)
           (visplane-maxx-set! check -1)
           (vp-clear-top! check)
           check)]))))

;; R_CheckPlane. (C doesn't check for running out of visplanes here; we
;; fail on the vector index.)
(define (r-check-plane pl start stop)
  (let-values ([(intrl unionl) (if (fx< start (visplane-minx pl))
                                   (values (visplane-minx pl) start)
                                   (values start (visplane-minx pl)))]
               [(intrh unionh) (if (fx> stop (visplane-maxx pl))
                                   (values (visplane-maxx pl) stop)
                                   (values stop (visplane-maxx pl)))])
    (let ([top (visplane-top pl)])
      (if (let loop ([x intrl])
            (cond [(fx> x intrh) #t]
                  [(not (fx= (vp-ref top x) vp-empty)) #f]
                  [else (loop (fx+ x 1))]))
          ;; use the same one
          (begin (visplane-minx-set! pl unionl)
                 (visplane-maxx-set! pl unionh)
                 pl)
          ;; make a new visplane
          (let ([new (vector-ref visplanes lastvisplane)])
            (visplane-height-set! new (visplane-height pl))
            (visplane-picnum-set! new (visplane-picnum pl))
            (visplane-lightlevel-set! new (visplane-lightlevel pl))
            (set! lastvisplane (fx+ lastvisplane 1))
            (visplane-minx-set! new start)
            (visplane-maxx-set! new stop)
            (vp-clear-top! new)
            new)))))

;; R_MakeSpans
(define (r-make-spans x t1 b1 t2 b2)
  (let loop ([t1 t1])
    (if (and (fx< t1 t2) (fx<= t1 b1))
        (begin (r-map-plane t1 (fxvector-ref spanstart t1) (fx- x 1))
               (loop (fx+ t1 1)))
        (let loop ([b1 b1])
          (if (and (fx> b1 b2) (fx>= b1 t1))
              (begin (r-map-plane b1 (fxvector-ref spanstart b1) (fx- x 1))
                     (loop (fx- b1 1)))
              (let loop ([t2 t2])
                (if (and (fx< t2 t1) (fx<= t2 b2))
                    (begin (fxvector-set! spanstart t2 x)
                           (loop (fx+ t2 1)))
                    (let loop ([b2 b2])
                      (when (and (fx> b2 b1) (fx>= b2 t2))
                        (fxvector-set! spanstart b2 x)
                        (loop (fx- b2 1)))))))))))

;; R_DrawPlanes: at the end of each frame.
(define (r-draw-planes)
  (when (fx> ds-p MAXDRAWSEGS)
    (i-error "R_DrawPlanes: drawsegs overflow (~a)" ds-p))
  (when (fx> lastvisplane MAXVISPLANES)
    (i-error "R_DrawPlanes: visplane overflow (~a)" lastvisplane))
  (when (fx> (fx- lastopening openings) (maxopenings))
    (i-error "R_DrawPlanes: opening overflow (~a)" (fx- lastopening openings)))
  (do ([i 0 (fx+ i 1)]) ((fx= i lastvisplane))
    (let* ([pl (vector-ref visplanes i)]
           [minx (visplane-minx pl)] [maxx (visplane-maxx pl)]
           [top (visplane-top pl)] [bottom (visplane-bottom pl)])
      (cond
        [(fx> minx maxx) (void)]
        [(fx= (visplane-picnum pl) skyflatnum)
         ;; sky flat
         (set! dc-iscale (fxsra pspriteiscale detailshift))
         ;; Sky is allways drawn full bright, i.e. colormaps[0] is used.
         ;; Because of this hack, sky is not affected by INVUL inverse
         ;; mapping.
         (set! dc-colormap 0)
         (set! dc-texturemid skytexturemid)
         (do ([x minx (fx+ x 1)]) ((fx> x maxx))
           (set! dc-yl (vp-ref top x))
           (set! dc-yh (vp-ref bottom x))
           (when (fx<= dc-yl dc-yh)
             (let ([angle (fxsrl (angle+ viewangle (fxvector-ref xtoviewangle x)) ANGLETOSKYSHIFT)])
               (set! dc-x x)
               (let-values ([(bv ofs) (r-get-column skytexture angle)])
                 (set! dc-source bv)
                 (set! dc-source-ofs ofs))
               (colfunc))))]
        [else
         ;; regular flat
         (set! ds-source (w-cache-lump-num (fx+ firstflat
                                                 (fxvector-ref flattranslation (visplane-picnum pl)))))
         (set! planeheight (c-abs (i- (visplane-height pl) viewz)))
         (let ([light (fx+ (fxsra (visplane-lightlevel pl) LIGHTSEGSHIFT) extralight)])
           (set! planezlight
                 (vector-ref zlight (cond [(fx>= light LIGHTLEVELS) (fx- LIGHTLEVELS 1)]
                                          [(fx< light 0) 0]
                                          [else light]))))
         (vp-set! top (fx+ maxx 1) vp-empty)
         (vp-set! top (fx- minx 1) vp-empty)
         (do ([x minx (fx+ x 1)]) ((fx> x (fx+ maxx 1)))
           (r-make-spans x (vp-ref top (fx- x 1)) (vp-ref bottom (fx- x 1))
                         (vp-ref top x) (vp-ref bottom x)))]))))
