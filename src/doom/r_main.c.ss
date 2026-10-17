;;; r_main.c: view setup, BSP utility math, and the frame driver.
;;; Included by r_local.sls.

;; Fineangles in the SCREENWIDTH wide window.
(define FIELDOFVIEW 2048)

;; Lighting constants (r_main.h). Now why not 32 levels here?
(define LIGHTLEVELS 16)
(define LIGHTSEGSHIFT 4)
(define MAXLIGHTSCALE 48)
(define LIGHTSCALESHIFT 12)
(define MAXLIGHTZ 128)
(define LIGHTZSHIFT 20)
(define NUMCOLORMAPS 32)
(define DISTMAP 2)

(define-global viewangleoffset 0)

;; increment every time a check is made
(define-global validcount 1)

;; Lighttable pointers are offsets into `colormaps`; #f is NULL.
(define fixedcolormap #f)

(define centerx 0)
(define centery 0)
(define centerxfrac 0)
(define centeryfrac 0)
(define projection 0)

;; just for profiling purposes
(define-global framecount 0)
(define sscount 0)

(define viewx 0)
(define viewy 0)
(define viewz 0)
(define viewangle 0)
(define viewcos 0)
(define viewsin 0)
(define viewplayer #f)

;; 0 = high, 1 = low
(define-global detailshift 0)

;; precalculated math tables
(define clipangle 0)

;; The viewangletox[viewangle + FINEANGLES/4] lookup maps the visible view
;; angles to screen X coordinates, flattening the arc to a flat projection
;; plane. There will be many angles mapped to the same X.
(define viewangletox (make-fxvector (fxquotient FINEANGLES 2) 0))

;; The xtoviewangle[] table maps a screen pixel to the lowest viewangle
;; that maps back to x ranges from clipangle to -clipangle.
(define xtoviewangle (make-fxvector (fx+ SCREENWIDTH 1) 0))

;; Light tables: each row is an fxvector of offsets into `colormaps`.
;; walllights, spritelights and planezlight point at one of these rows.
(define scalelight
  (let ([v (make-vector LIGHTLEVELS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i LIGHTLEVELS) v)
      (vector-set! v i (make-fxvector MAXLIGHTSCALE 0)))))
(define scalelightfixed (make-fxvector MAXLIGHTSCALE 0))
(define zlight
  (let ([v (make-vector LIGHTLEVELS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i LIGHTLEVELS) v)
      (vector-set! v i (make-fxvector MAXLIGHTZ 0)))))

;; bumped light from gun blasts
(define extralight 0)

(define colfunc #f)
(define basecolfunc #f)
(define fuzzcolfunc #f)
(define transcolfunc #f)
(define spanfunc #f)

;; int / fixed_t arithmetic that wraps like 32-bit C.
(define-syntax i+ (syntax-rules () [(_ a b) (int32 (fx+ a b))]))
(define-syntax i- (syntax-rules () [(_ a b) (int32 (fx- a b))]))
(define-syntax ineg (syntax-rules () [(_ a) (int32 (fx- 0 a))]))
;; angle_t arithmetic.
(define-syntax uneg (syntax-rules () [(_ a) (fxand (fx- 0 a) #xFFFFFFFF)]))

;; R_PointOnSide: traverse BSP (sub) tree, check point against partition
;; plane. Returns side 0 (front) or 1 (back).
(define (r-point-on-side x y node)
  (let ([nx (node-x node)] [ny (node-y node)]
        [ndx (node-dx node)] [ndy (node-dy node)])
    (cond
      [(fx= ndx 0)
       (if (fx<= x nx)
           (if (fx> ndy 0) 1 0)
           (if (fx< ndy 0) 1 0))]
      [(fx= ndy 0)
       (if (fx<= y ny)
           (if (fx< ndx 0) 1 0)
           (if (fx> ndx 0) 1 0))]
      [else
       (let ([dx (i- x nx)] [dy (i- y ny)])
         ;; Try to quickly decide by looking at sign bits.
         (if (fx< (fxxor ndy ndx dx dy) 0)
             (if (fx< (fxxor ndy dx) 0) 1 0)   ; (left is negative)
             (let ([left (fixed-mul (fxsra ndy FRACBITS) dx)]
                   [right (fixed-mul dy (fxsra ndx FRACBITS))])
               (if (fx< right left) 0 1))))])))

(define (r-point-on-seg-side x y line)
  (let* ([lx (vertex-x (seg-v1 line))] [ly (vertex-y (seg-v1 line))]
         [ldx (i- (vertex-x (seg-v2 line)) lx)]
         [ldy (i- (vertex-y (seg-v2 line)) ly)])
    (cond
      [(fx= ldx 0)
       (if (fx<= x lx)
           (if (fx> ldy 0) 1 0)
           (if (fx< ldy 0) 1 0))]
      [(fx= ldy 0)
       (if (fx<= y ly)
           (if (fx< ldx 0) 1 0)
           (if (fx> ldx 0) 1 0))]
      [else
       (let ([dx (i- x lx)] [dy (i- y ly)])
         (if (fx< (fxxor ldy ldx dx dy) 0)
             (if (fx< (fxxor ldy dx) 0) 1 0)
             (let ([left (fixed-mul (fxsra ldy FRACBITS) dx)]
                   [right (fixed-mul dy (fxsra ldx FRACBITS))])
               (if (fx< right left) 0 1))))])))

;; R_PointToAngle: to get a global angle from cartesian coordinates, the
;; coordinates are flipped until they are in the first octant of the
;; coordinate system, then the y (<=x) is scaled and divided by x to get a
;; tangent (slope) value which is looked up in the tantoangle[] table.
(define (tan->angle i) (fxvector-ref tantoangle i))
(define (r-point-to-angle x y)
  (let ([x (i- x viewx)] [y (i- y viewy)])
    (cond
      [(and (fx= x 0) (fx= y 0)) 0]
      [(fx>= x 0)
       (if (fx>= y 0)
           (if (fx> x y)
               (tan->angle (slope-div y x))                                  ; octant 0
               (fxand (fx- (fx- ANG90 1) (tan->angle (slope-div x y))) #xFFFFFFFF)) ; octant 1
           (let ([y (ineg y)])
             (if (fx> x y)
                 (uneg (tan->angle (slope-div y x)))                         ; octant 8
                 (angle+ ANG270 (tan->angle (slope-div x y))))))]            ; octant 7
      [else
       (let ([x (ineg x)])
         (if (fx>= y 0)
             (if (fx> x y)
                 (angle- (fx- ANG180 1) (tan->angle (slope-div y x)))       ; octant 3
                 (angle+ ANG90 (tan->angle (slope-div x y))))               ; octant 2
             (let ([y (ineg y)])
               (if (fx> x y)
                   (angle+ ANG180 (tan->angle (slope-div y x)))              ; octant 4
                   (angle- (fx- ANG270 1) (tan->angle (slope-div x y)))))))]))) ; octant 5

;; Note: sets viewx/viewy, as in C (callers in the playsim rely on nothing
;; else, and the renderer resets them every frame).
(define (r-point-to-angle2 x1 y1 x2 y2)
  (set! viewx x1)
  (set! viewy y1)
  (r-point-to-angle x2 y2))

(define (r-point-to-dist x y)
  (let* ([dx (c-abs (i- x viewx))]
         [dy (c-abs (i- y viewy))]
         [big (if (fx> dy dx) dy dx)]
         [small (if (fx> dy dx) dx dy)]
         [angle (fxsrl (angle+ (fxvector-ref tantoangle (fxsra (fixed-div small big) DBITS))
                               ANG90)
                       ANGLETOFINESHIFT)])
    ;; use as cosine
    (fixed-div big (fxvector-ref finesine angle))))

;; R_ScaleFromGlobalAngle: returns the texture mapping scale for the
;; current line (horizontal span) at the given angle. rw_distance must be
;; calculated first.
(define (r-scale-from-global-angle visangle)
  ;; anglea/angleb are ints in C: shifted arithmetically.
  (let* ([anglea (int32 (fx+ ANG90 (fx- visangle viewangle)))]
         [angleb (int32 (fx+ ANG90 (fx- visangle rw-normalangle)))]
         ;; both sines are allways positive
         [sinea (fxvector-ref finesine (fxsra anglea ANGLETOFINESHIFT))]
         [sineb (fxvector-ref finesine (fxsra angleb ANGLETOFINESHIFT))]
         [num (int32 (fxsll (fixed-mul projection sineb) detailshift))]
         [den (fixed-mul rw-distance sinea)])
    (if (fx> den (fxsra num 16))
        (let ([scale (fixed-div num den)])
          (cond [(fx> scale (* 64 FRACUNIT)) (* 64 FRACUNIT)]
                [(fx< scale 256) 256]
                [else scale]))
        (* 64 FRACUNIT))))

;; R_InitTextureMapping
(define (r-init-texture-mapping)
  ;; Use tangent table to generate viewangletox: viewangletox will give the
  ;; next greatest x after the view angle.
  ;; Calc focallength so FIELDOFVIEW angles covers SCREENWIDTH.
  (let ([focallength (fixed-div centerxfrac
                                (fxvector-ref finetangent
                                              (fx+ (fxquotient FINEANGLES 4)
                                                   (fxquotient FIELDOFVIEW 2))))])
    (do ([i 0 (fx+ i 1)]) ((fx= i (fxquotient FINEANGLES 2)))
      (let ([tan (fxvector-ref finetangent i)])
        (fxvector-set! viewangletox i
                       (cond
                         [(fx> tan (* FRACUNIT 2)) -1]
                         [(fx< tan (- (* FRACUNIT 2))) (fx+ viewwidth 1)]
                         [else
                          (let* ([t (fixed-mul tan focallength)]
                                 [t (fxsra (int32 (fx+ (fx- centerxfrac t) (fx- FRACUNIT 1)))
                                           FRACBITS)])
                            (cond [(fx< t -1) -1]
                                  [(fx> t (fx+ viewwidth 1)) (fx+ viewwidth 1)]
                                  [else t]))]))))
    ;; Scan viewangletox[] to generate xtoviewangle[]: xtoviewangle will
    ;; give the smallest view angle that maps to x.
    (do ([x 0 (fx+ x 1)]) ((fx> x viewwidth))
      (let loop ([i 0])
        (if (fx> (fxvector-ref viewangletox i) x)
            (loop (fx+ i 1))
            (fxvector-set! xtoviewangle x
                           (angle- (fxsll i ANGLETOFINESHIFT) ANG90)))))
    ;; Take out the fencepost cases from viewangletox.
    (do ([i 0 (fx+ i 1)]) ((fx= i (fxquotient FINEANGLES 2)))
      (cond
        [(fx= (fxvector-ref viewangletox i) -1)
         (fxvector-set! viewangletox i 0)]
        [(fx= (fxvector-ref viewangletox i) (fx+ viewwidth 1))
         (fxvector-set! viewangletox i viewwidth)]))
    (set! clipangle (fxvector-ref xtoviewangle 0))))

;; R_InitLightTables: only inits the zlight table, because the scalelight
;; table changes with view size.
(define (r-init-light-tables)
  ;; Calculate the light levels to use for each level / distance
  ;; combination.
  (do ([i 0 (fx+ i 1)]) ((fx= i LIGHTLEVELS))
    (let ([startmap (fxquotient (fx* (fx* (fx- (fx- LIGHTLEVELS 1) i) 2) NUMCOLORMAPS)
                                LIGHTLEVELS)]
          [row (vector-ref zlight i)])
      (do ([j 0 (fx+ j 1)]) ((fx= j MAXLIGHTZ))
        (let* ([scale (fixed-div (* (fxquotient SCREENWIDTH 2) FRACUNIT)
                                 (fxsll (fx+ j 1) LIGHTZSHIFT))]
               [scale (fxsra scale LIGHTSCALESHIFT)]
               [level (fx- startmap (fxquotient scale DISTMAP))]
               [level (cond [(fx< level 0) 0]
                            [(fx>= level NUMCOLORMAPS) (fx- NUMCOLORMAPS 1)]
                            [else level])])
          (fxvector-set! row j (fx* level 256)))))))

;; R_SetViewSize: do not really change anything here, because it might be
;; in the middle of a refresh. The change will take effect next refresh.
(define-global setsizeneeded #f)
(define setblocks 0)
(define setdetail 0)
(define (r-set-view-size blocks detail)
  (set! setsizeneeded #t)
  (set! setblocks blocks)
  (set! setdetail detail))

;; R_ExecuteSetViewSize
(define (r-execute-set-view-size)
  (set! setsizeneeded #f)
  (if (fx= setblocks 11)
      (begin (set! scaledviewwidth SCREENWIDTH)
             (set! viewheight SCREENHEIGHT))
      (begin (set! scaledviewwidth (fx* setblocks 32))
             (set! viewheight (fxand (fxquotient (fx* setblocks 168) 10) (fxnot 7)))))
  (set! detailshift setdetail)
  (set! viewwidth (fxsra scaledviewwidth detailshift))
  (set! centery (fxquotient viewheight 2))
  (set! centerx (fxquotient viewwidth 2))
  (set! centerxfrac (fxsll centerx FRACBITS))
  (set! centeryfrac (fxsll centery FRACBITS))
  (set! projection centerxfrac)
  (if (fx= detailshift 0)
      (begin
        (set! colfunc r-draw-column)
        (set! basecolfunc r-draw-column)
        (set! fuzzcolfunc r-draw-fuzz-column)
        (set! transcolfunc r-draw-translated-column)
        (set! spanfunc r-draw-span))
      (begin
        (set! colfunc r-draw-column-low)
        (set! basecolfunc r-draw-column-low)
        (set! fuzzcolfunc r-draw-fuzz-column)
        (set! transcolfunc r-draw-translated-column)
        (set! spanfunc r-draw-span-low)))
  (r-init-buffer scaledviewwidth viewheight)
  (r-init-texture-mapping)
  ;; psprite scales
  (set! pspritescale (fxquotient (fx* FRACUNIT viewwidth) SCREENWIDTH))
  (set! pspriteiscale (fxquotient (fx* FRACUNIT SCREENWIDTH) viewwidth))
  ;; thing clipping
  (do ([i 0 (fx+ i 1)]) ((fx= i viewwidth))
    (fxvector-set! clipmem (fx+ screenheightarray i) viewheight))
  ;; planes
  (do ([i 0 (fx+ i 1)]) ((fx= i viewheight))
    (let* ([dy (fx+ (fxsll (fx- i (fxquotient viewheight 2)) FRACBITS) (fxquotient FRACUNIT 2))]
           [dy (fxabs dy)])
      (fxvector-set! yslope i (fixed-div (fx* (fxquotient (fxsll viewwidth detailshift) 2)
                                              FRACUNIT)
                                         dy))))
  (do ([i 0 (fx+ i 1)]) ((fx= i viewwidth))
    (let ([cosadj (fxabs (finecosine (fxsrl (fxvector-ref xtoviewangle i) ANGLETOFINESHIFT)))])
      (fxvector-set! distscale i (fixed-div FRACUNIT cosadj))))
  ;; Calculate the light levels to use for each level / scale combination.
  (do ([i 0 (fx+ i 1)]) ((fx= i LIGHTLEVELS))
    (let ([startmap (fxquotient (fx* (fx* (fx- (fx- LIGHTLEVELS 1) i) 2) NUMCOLORMAPS)
                                LIGHTLEVELS)]
          [row (vector-ref scalelight i)])
      (do ([j 0 (fx+ j 1)]) ((fx= j MAXLIGHTSCALE))
        (let* ([level (fx- startmap
                           (fxquotient (fxquotient (fx* j SCREENWIDTH)
                                                   (fxsll viewwidth detailshift))
                                       DISTMAP))]
               [level (cond [(fx< level 0) 0]
                            [(fx>= level NUMCOLORMAPS) (fx- NUMCOLORMAPS 1)]
                            [else level])])
          (fxvector-set! row j (fx* level 256)))))))

;; R_Init. screenblocks and detail-level come from the config (C reads the
;; globals set by m_misc).
(define (r-init screenblocks detail-level)
  (r-init-data)
  (r-set-view-size screenblocks detail-level)
  (r-init-planes)
  (r-init-light-tables)
  (r-init-sky-map)
  (r-init-translation-tables)
  (set! framecount 0))

;; R_PointInSubsector
(define (r-point-in-subsector x y)
  ;; single subsector is a special case
  (if (fx= numnodes 0)
      (vector-ref subsectors 0)
      (let loop ([nodenum (fx- numnodes 1)])
        (if (fxlogtest nodenum NF_SUBSECTOR)
            (vector-ref subsectors (fxand nodenum (fxnot NF_SUBSECTOR)))
            (let ([node (vector-ref nodes nodenum)])
              (loop (fxvector-ref (node-children node) (r-point-on-side x y node))))))))

;; R_SetupFrame
(define (r-setup-frame player)
  (let ([mo (player-mo player)])
    (set! viewplayer player)
    (set! viewx (mobj-x mo))
    (set! viewy (mobj-y mo))
    (set! viewangle (angle+ (mobj-angle mo) viewangleoffset))
    (set! extralight (player-extralight player))
    (set! viewz (player-viewz player))
    (set! viewsin (fxvector-ref finesine (fxsrl viewangle ANGLETOFINESHIFT)))
    (set! viewcos (finecosine (fxsrl viewangle ANGLETOFINESHIFT)))
    (set! sscount 0)
    (if (fx= (player-fixedcolormap player) 0)
        (set! fixedcolormap #f)
        (begin
          (set! fixedcolormap (fx* (player-fixedcolormap player) 256))
          (set! walllights scalelightfixed)
          (fxvector-fill! scalelightfixed fixedcolormap)))
    (set! framecount (fx+ framecount 1))
    (set! validcount (fx+ validcount 1))))

;; R_RenderPlayerView. (C calls NetUpdate between the phases to poll for
;; network input; that has no effect on the picture.)
;; NetUpdate, which d_main (above the renderer) sets: the renderer checks
;; for new console commands between its phases.
(define-global r-net-update (lambda () (void)))

(define (r-render-player-view player)
  (r-setup-frame player)
  ;; Clear buffers.
  (r-clear-clip-segs)
  (r-clear-draw-segs)
  (r-clear-planes)
  (r-clear-sprites)
  ;; check for new console commands.
  (r-net-update)
  ;; The head node is the last node output.
  (r-render-bsp-node (fx- numnodes 1))
  ;; Check for new console commands.
  (r-net-update)
  (r-draw-planes)
  ;; Check for new console commands.
  (r-net-update)
  (r-draw-masked)
  ;; Check for new console commands.
  (r-net-update))
