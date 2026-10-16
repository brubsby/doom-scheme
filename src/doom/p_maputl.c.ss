;;; p_maputl.c: movement and collision utilities: distance and side tests,
;;; line openings, thing position links, blockmap iterators, and path
;;; traversal. Included by p_local.sls.

;; P_AproxDistance: gives an estimation of distance (not exact).
(define (p-aprox-distance dx dy)
  (let ([dx (c-abs dx)] [dy (c-abs dy)])
    (if (fx< dx dy)
        (i- (i+ dx dy) (fxsra dx 1))
        (i- (i+ dx dy) (fxsra dy 1)))))

;; P_PointOnLineSide: returns 0 or 1.
(define (p-point-on-line-side x y line)
  (let ([v1 (line-v1 line)] [ldx (line-dx line)] [ldy (line-dy line)])
    (cond
      [(fx= ldx 0)
       (if (fx<= x (vertex-x v1))
           (if (fx> ldy 0) 1 0)
           (if (fx< ldy 0) 1 0))]
      [(fx= ldy 0)
       (if (fx<= y (vertex-y v1))
           (if (fx< ldx 0) 1 0)
           (if (fx> ldx 0) 1 0))]
      [else
       (let* ([dx (i- x (vertex-x v1))]
              [dy (i- y (vertex-y v1))]
              [left (fixed-mul (fxsra ldy FRACBITS) dx)]
              [right (fixed-mul dy (fxsra ldx FRACBITS))])
         (if (fx< right left)
             0                          ; front side
             1))])))                    ; back side

;; P_BoxOnLineSide: considers the line to be infinite. Returns side 0 or 1,
;; -1 if box crosses the line.
(define (p-box-on-line-side tmbox ld)
  (let-values
      ([(p1 p2)
        (let ([v1 (line-v1 ld)])
          (cond
            [(fx= (line-slopetype ld) ST_HORIZONTAL)
             (let ([p1 (if (fx> (fxvector-ref tmbox BOXTOP) (vertex-y v1)) 1 0)]
                   [p2 (if (fx> (fxvector-ref tmbox BOXBOTTOM) (vertex-y v1)) 1 0)])
               (if (fx< (line-dx ld) 0)
                   (values (fxxor p1 1) (fxxor p2 1))
                   (values p1 p2)))]
            [(fx= (line-slopetype ld) ST_VERTICAL)
             (let ([p1 (if (fx< (fxvector-ref tmbox BOXRIGHT) (vertex-x v1)) 1 0)]
                   [p2 (if (fx< (fxvector-ref tmbox BOXLEFT) (vertex-x v1)) 1 0)])
               (if (fx< (line-dy ld) 0)
                   (values (fxxor p1 1) (fxxor p2 1))
                   (values p1 p2)))]
            [(fx= (line-slopetype ld) ST_POSITIVE)
             (values (p-point-on-line-side (fxvector-ref tmbox BOXLEFT) (fxvector-ref tmbox BOXTOP) ld)
                     (p-point-on-line-side (fxvector-ref tmbox BOXRIGHT) (fxvector-ref tmbox BOXBOTTOM) ld))]
            [else                       ; ST_NEGATIVE
             (values (p-point-on-line-side (fxvector-ref tmbox BOXRIGHT) (fxvector-ref tmbox BOXTOP) ld)
                     (p-point-on-line-side (fxvector-ref tmbox BOXLEFT) (fxvector-ref tmbox BOXBOTTOM) ld))]))])
    (if (fx= p1 p2) p1 -1)))

;; divline_t: a line given by a point and a direction.
(define-struct divline () (x 0) (y 0) (dx 0) (dy 0))

;; P_PointOnDivlineSide: returns 0 or 1.
(define (p-point-on-divline-side x y line)
  (let ([lx (divline-x line)] [ly (divline-y line)]
        [ldx (divline-dx line)] [ldy (divline-dy line)])
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
         ;; try to quickly decide by looking at sign bits
         (if (fx< (fxxor ldy ldx dx dy) 0)
             (if (fx< (fxxor ldy dx) 0) 1 0)   ; (left is negative)
             (let ([left (fixed-mul (fxsra ldy 8) (fxsra dx 8))]
                   [right (fixed-mul (fxsra dy 8) (fxsra ldx 8))])
               (if (fx< right left) 0 1))))])))

;; P_MakeDivline
(define (p-make-divline li dl)
  (divline-x-set! dl (vertex-x (line-v1 li)))
  (divline-y-set! dl (vertex-y (line-v1 li)))
  (divline-dx-set! dl (line-dx li))
  (divline-dy-set! dl (line-dy li)))

;; P_InterceptVector: returns the fractional intercept point along the
;; first divline. This is only called by the addthings and addlines
;; traversers.
(define (p-intercept-vector v2 v1)
  (let ([den (i- (fixed-mul (fxsra (divline-dy v1) 8) (divline-dx v2))
                 (fixed-mul (fxsra (divline-dx v1) 8) (divline-dy v2)))])
    (if (fx= den 0)
        0
        (let ([num (i+ (fixed-mul (fxsra (i- (divline-x v1) (divline-x v2)) 8) (divline-dy v1))
                       (fixed-mul (fxsra (i- (divline-y v2) (divline-y v1)) 8) (divline-dx v1)))])
          (fixed-div num den)))))

;; P_LineOpening: sets opentop and openbottom to the window through a two
;; sided line.
(define opentop 0)
(define openbottom 0)
(define openrange 0)
(define lowfloor 0)

(define (p-line-opening linedef)
  (if (fx= (fxvector-ref (line-sidenum linedef) 1) -1)
      ;; single sided line
      (set! openrange 0)
      (let ([front (line-frontsector linedef)] [back (line-backsector linedef)])
        (set! opentop (fxmin (sector-ceilingheight front) (sector-ceilingheight back)))
        (if (fx> (sector-floorheight front) (sector-floorheight back))
            (begin (set! openbottom (sector-floorheight front))
                   (set! lowfloor (sector-floorheight back)))
            (begin (set! openbottom (sector-floorheight back))
                   (set! lowfloor (sector-floorheight front))))
        (set! openrange (i- opentop openbottom)))))

;; P_UnsetThingPosition: unlinks a thing from block map and sectors. On
;; each position change, BLOCKMAP and other lookups maintaining lists of
;; things inside these structures need to be updated.
(define (p-unset-thing-position thing)
  (unless (fxlogtest (mobj-flags thing) MF_NOSECTOR)
    ;; inert things don't need to be in blockmap?
    ;; unlink from subsector
    (when (mobj-snext thing)
      (mobj-sprev-set! (mobj-snext thing) (mobj-sprev thing)))
    (if (mobj-sprev thing)
        (mobj-snext-set! (mobj-sprev thing) (mobj-snext thing))
        (sector-thinglist-set! (subsector-sector (mobj-subsector thing)) (mobj-snext thing))))
  (unless (fxlogtest (mobj-flags thing) MF_NOBLOCKMAP)
    ;; inert things don't need to be in blockmap
    ;; unlink from block map
    (when (mobj-bnext thing)
      (mobj-bprev-set! (mobj-bnext thing) (mobj-bprev thing)))
    (if (mobj-bprev thing)
        (mobj-bnext-set! (mobj-bprev thing) (mobj-bnext thing))
        (let ([blockx (fxsra (i- (mobj-x thing) bmaporgx) MAPBLOCKSHIFT)]
              [blocky (fxsra (i- (mobj-y thing) bmaporgy) MAPBLOCKSHIFT)])
          (when (and (fx< -1 blockx bmapwidth) (fx< -1 blocky bmapheight))
            (vector-set! blocklinks (fx+ (fx* blocky bmapwidth) blockx) (mobj-bnext thing)))))))

;; P_SetThingPosition: links a thing into both a block and a subsector
;; based on its x y. Sets thing->subsector properly.
(define (p-set-thing-position thing)
  ;; link into subsector
  (let ([ss (r-point-in-subsector (mobj-x thing) (mobj-y thing))])
    (mobj-subsector-set! thing ss)
    (unless (fxlogtest (mobj-flags thing) MF_NOSECTOR)
      ;; invisible things don't go into the sector links
      (let ([sec (subsector-sector ss)])
        (mobj-sprev-set! thing #f)
        (mobj-snext-set! thing (sector-thinglist sec))
        (when (sector-thinglist sec)
          (mobj-sprev-set! (sector-thinglist sec) thing))
        (sector-thinglist-set! sec thing))))
  ;; link into blockmap
  (unless (fxlogtest (mobj-flags thing) MF_NOBLOCKMAP)
    ;; inert things don't need to be in blockmap
    (let ([blockx (fxsra (i- (mobj-x thing) bmaporgx) MAPBLOCKSHIFT)]
          [blocky (fxsra (i- (mobj-y thing) bmaporgy) MAPBLOCKSHIFT)])
      (if (and (fx< -1 blockx bmapwidth) (fx< -1 blocky bmapheight))
          (let* ([i (fx+ (fx* blocky bmapwidth) blockx)]
                 [link (vector-ref blocklinks i)])
            (mobj-bprev-set! thing #f)
            (mobj-bnext-set! thing link)
            (when link (mobj-bprev-set! link thing))
            (vector-set! blocklinks i thing))
          ;; thing is off the map
          (begin (mobj-bnext-set! thing #f)
                 (mobj-bprev-set! thing #f))))))

;; BLOCK MAP ITERATORS
;; For each line/thing in the given mapblock, call the passed PIT_*
;; function. If the function returns false, exit with false without
;; checking anything else.

;; P_BlockLinesIterator: the validcount flags are used to avoid checking
;; lines that are marked in multiple mapblocks, so increment validcount
;; before the first call to P_BlockLinesIterator, then make one or more
;; calls to it. (Each block's list starts with a 0, so line 0 is checked
;; from every block.)
(define (p-block-lines-iterator x y func)
  (if (or (fx< x 0) (fx< y 0) (fx>= x bmapwidth) (fx>= y bmapheight))
      #t
      (let loop ([list (fxvector-ref blockmaplump (fx+ BLOCKMAP (fx* y bmapwidth) x))])
        (let ([n (fxvector-ref blockmaplump list)])
          (if (fx= n -1)
              #t                        ; everything was checked
              (let ([ld (vector-ref lines n)])
                (cond
                  ;; line has already been checked
                  [(fx= (line-validcount ld) validcount) (loop (fx+ list 1))]
                  [else
                   (line-validcount-set! ld validcount)
                   (and (func ld) (loop (fx+ list 1)))])))))))

;; P_BlockThingsIterator
(define (p-block-things-iterator x y func)
  (if (or (fx< x 0) (fx< y 0) (fx>= x bmapwidth) (fx>= y bmapheight))
      #t
      (let loop ([mobj (vector-ref blocklinks (fx+ (fx* y bmapwidth) x))])
        (or (not mobj)
            (and (func mobj) (loop (mobj-bnext mobj)))))))

;; INTERCEPT ROUTINES
;; intercept_t: d is a line (isaline) or an mobj.
(define-struct intercept () (frac 0) (isaline #f) (d #f))

;; C has no bounds check here: more than MAXINTERCEPTS intercepts overrun
;; the array. We stop with an error instead (none seen in the tests).
(define intercepts
  (let ([v (make-vector MAXINTERCEPTS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXINTERCEPTS) v)
      (vector-set! v i (make-intercept)))))
(define intercept-p 0)

(define (add-intercept! frac isaline d)
  (when (fx= intercept-p MAXINTERCEPTS)
    (i-error "intercepts overflow (more than ~a)" MAXINTERCEPTS))
  (let ([in (vector-ref intercepts intercept-p)])
    (intercept-frac-set! in frac)
    (intercept-isaline-set! in isaline)
    (intercept-d-set! in d)
    (set! intercept-p (fx+ intercept-p 1))))

;; C: trace
(define trace-line (make-divline))
(define earlyout #f)

;; PIT_AddLineIntercepts: looks for lines in the given block that
;; intercept the given trace to add to the intercepts list. A line is
;; crossed if its endpoints are on opposite sides of the trace. Returns
;; true if earlyout and a solid line hit.
(define dl (make-divline))
(define (pit-add-line-intercepts ld)
  (let-values
      ([(s1 s2)
        ;; avoid precision problems with two routines
        (if (or (fx> (divline-dx trace-line) (* FRACUNIT 16)) (fx> (divline-dy trace-line) (* FRACUNIT 16))
                (fx< (divline-dx trace-line) (* FRACUNIT -16)) (fx< (divline-dy trace-line) (* FRACUNIT -16)))
            (values (p-point-on-divline-side (vertex-x (line-v1 ld)) (vertex-y (line-v1 ld)) trace-line)
                    (p-point-on-divline-side (vertex-x (line-v2 ld)) (vertex-y (line-v2 ld)) trace-line))
            (values (p-point-on-line-side (divline-x trace-line) (divline-y trace-line) ld)
                    (p-point-on-line-side (i+ (divline-x trace-line) (divline-dx trace-line))
                                          (i+ (divline-y trace-line) (divline-dy trace-line)) ld)))])
    (if (fx= s1 s2)
        #t                              ; line isn't crossed
        ;; hit the line
        (begin
          (p-make-divline ld dl)
          (let ([frac (p-intercept-vector trace-line dl)])
            (cond
              [(fx< frac 0) #t]         ; behind source
              ;; try to early out the check
              [(and earlyout (fx< frac FRACUNIT) (not (line-backsector ld)))
               #f]                      ; stop checking
              [else (add-intercept! frac #t ld) #t]))))))

;; PIT_AddThingIntercepts
(define (pit-add-thing-intercepts thing)
  (let* ([tracepositive (fx> (fxxor (divline-dx trace-line) (divline-dy trace-line)) 0)]
         [r (mobj-radius thing)] [x (mobj-x thing)] [y (mobj-y thing)]
         ;; check a corner to corner crossection for hit
         [x1 (i- x r)]
         [y1 (if tracepositive (i+ y r) (i- y r))]
         [x2 (i+ x r)]
         [y2 (if tracepositive (i- y r) (i+ y r))]
         [s1 (p-point-on-divline-side x1 y1 trace-line)]
         [s2 (p-point-on-divline-side x2 y2 trace-line)])
    (if (fx= s1 s2)
        #t                              ; line isn't crossed
        (begin
          (divline-x-set! dl x1)
          (divline-y-set! dl y1)
          (divline-dx-set! dl (i- x2 x1))
          (divline-dy-set! dl (i- y2 y1))
          (let ([frac (p-intercept-vector trace-line dl)])
            (unless (fx< frac 0)        ; behind source
              (add-intercept! frac #f thing))
            #t)))))                     ; keep going

;; P_TraverseIntercepts: returns true if the traverser function returns
;; true for all lines.
(define (p-traverse-intercepts func maxfrac)
  (let loop ([count intercept-p])
    (if (fx= count 0)
        #t                              ; everything was traversed
        (let find ([i 0] [dist MAXINT] [in #f])
          (if (fx< i intercept-p)
              (let ([scan (vector-ref intercepts i)])
                (if (fx< (intercept-frac scan) dist)
                    (find (fx+ i 1) (intercept-frac scan) scan)
                    (find (fx+ i 1) dist in)))
              (cond
                [(fx> dist maxfrac) #t] ; checked everything in range
                [(not (func in)) #f]    ; don't bother going farther
                [else (intercept-frac-set! in MAXINT)
                      (loop (fx- count 1))]))))))

;; P_PathTraverse: traces a line from x1,y1 to x2,y2, calling the traverser
;; function for each. Returns true if the traverser function returns true
;; for all lines.
(define (p-path-traverse x1 y1 x2 y2 flags trav)
  (set! earlyout (fxlogtest flags PT_EARLYOUT))
  (set! validcount (fx+ validcount 1))
  (set! intercept-p 0)
  (let* ([x1 (if (fx= (fxand (i- x1 bmaporgx) (fx- MAPBLOCKSIZE 1)) 0)
                 (i+ x1 FRACUNIT)       ; don't side exactly on a line
                 x1)]
         [y1 (if (fx= (fxand (i- y1 bmaporgy) (fx- MAPBLOCKSIZE 1)) 0)
                 (i+ y1 FRACUNIT)       ; don't side exactly on a line
                 y1)])
    (divline-x-set! trace-line x1)
    (divline-y-set! trace-line y1)
    (divline-dx-set! trace-line (i- x2 x1))
    (divline-dy-set! trace-line (i- y2 y1))
    (let* ([x1 (i- x1 bmaporgx)] [y1 (i- y1 bmaporgy)]
           [xt1 (fxsra x1 MAPBLOCKSHIFT)] [yt1 (fxsra y1 MAPBLOCKSHIFT)]
           [x2 (i- x2 bmaporgx)] [y2 (i- y2 bmaporgy)]
           [xt2 (fxsra x2 MAPBLOCKSHIFT)] [yt2 (fxsra y2 MAPBLOCKSHIFT)])
      (let*-values
          ([(mapxstep partial ystep)
            (cond
              [(fx> xt2 xt1)
               (values 1 (fx- FRACUNIT (fxand (fxsra x1 MAPBTOFRAC) (fx- FRACUNIT 1)))
                       (fixed-div (i- y2 y1) (c-abs (i- x2 x1))))]
              [(fx< xt2 xt1)
               (values -1 (fxand (fxsra x1 MAPBTOFRAC) (fx- FRACUNIT 1))
                       (fixed-div (i- y2 y1) (c-abs (i- x2 x1))))]
              [else (values 0 FRACUNIT (* 256 FRACUNIT))])]
           [(yintercept) (i+ (fxsra y1 MAPBTOFRAC) (fixed-mul partial ystep))]
           [(mapystep partial xstep)
            (cond
              [(fx> yt2 yt1)
               (values 1 (fx- FRACUNIT (fxand (fxsra y1 MAPBTOFRAC) (fx- FRACUNIT 1)))
                       (fixed-div (i- x2 x1) (c-abs (i- y2 y1))))]
              [(fx< yt2 yt1)
               (values -1 (fxand (fxsra y1 MAPBTOFRAC) (fx- FRACUNIT 1))
                       (fixed-div (i- x2 x1) (c-abs (i- y2 y1))))]
              [else (values 0 FRACUNIT (* 256 FRACUNIT))])]
           [(xintercept) (i+ (fxsra x1 MAPBTOFRAC) (fixed-mul partial xstep))])
        ;; Step through map blocks. Count is present to prevent a round off
        ;; error from skipping the break.
        (let loop ([count 0] [mapx xt1] [mapy yt1] [xintercept xintercept] [yintercept yintercept])
          (cond
            [(fx= count 64) (p-traverse-intercepts trav FRACUNIT)]
            [(and (fxlogtest flags PT_ADDLINES)
                  (not (p-block-lines-iterator mapx mapy pit-add-line-intercepts)))
             #f]                        ; early out
            [(and (fxlogtest flags PT_ADDTHINGS)
                  (not (p-block-things-iterator mapx mapy pit-add-thing-intercepts)))
             #f]                        ; early out
            [(and (fx= mapx xt2) (fx= mapy yt2))
             ;; go through the sorted list
             (p-traverse-intercepts trav FRACUNIT)]
            [(fx= (fxsra yintercept FRACBITS) mapy)
             (loop (fx+ count 1) (fx+ mapx mapxstep) mapy xintercept (i+ yintercept ystep))]
            [(fx= (fxsra xintercept FRACBITS) mapx)
             (loop (fx+ count 1) mapx (fx+ mapy mapystep) (i+ xintercept xstep) yintercept)]
            [else (loop (fx+ count 1) mapx mapy xintercept yintercept)]))))))
