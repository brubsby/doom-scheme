;;; p_sight.c: line of sight checking. Included by p_local.sls.

(define sightzstart 0)          ; eye z of looker
(define topslope 0)
(define bottomslope 0)          ; slopes to top and bottom of target
(define strace (make-divline))  ; from t1 to t2
(define t2x 0)
(define t2y 0)
(define sightcounts (make-fxvector 2 0))

;; P_DivlineSide: returns side 0 (front), 1 (back), or 2 (on). Takes the
;; divline's fields, since C also passes nodes cast to divline_t. Note the
;; x==node->y test for horizontal lines, as in C.
(define (p-divline-side x y nx ny ndx ndy)
  (cond
    [(fx= ndx 0)
     (cond [(fx= x nx) 2]
           [(fx<= x nx) (if (fx> ndy 0) 1 0)]
           [else (if (fx< ndy 0) 1 0)])]
    [(fx= ndy 0)
     (cond [(fx= x ny) 2]
           [(fx<= y ny) (if (fx< ndx 0) 1 0)]
           [else (if (fx> ndx 0) 1 0)])]
    [else
     (let* ([dx (i- x nx)]
            [dy (i- y ny)]
            [left (int32 (fx* (fxsra ndy FRACBITS) (fxsra dx FRACBITS)))]
            [right (int32 (fx* (fxsra dy FRACBITS) (fxsra ndx FRACBITS)))])
       (cond [(fx< right left) 0]       ; front side
             [(fx= left right) 2]
             [else 1]))]))              ; back side

(define (divline-side x y dl)
  (p-divline-side x y (divline-x dl) (divline-y dl) (divline-dx dl) (divline-dy dl)))
(define (node-side x y node)
  (p-divline-side x y (node-x node) (node-y node) (node-dx node) (node-dy node)))

;; P_InterceptVector2: returns the fractional intercept point along the
;; first divline. (The same arithmetic as P_InterceptVector.)
(define (p-intercept-vector2 v2 v1)
  (p-intercept-vector v2 v1))

;; P_CrossSubsector: returns true if strace crosses the given subsector
;; successfully.
(define divl (make-divline))
(define (p-cross-subsector num)
  (when (fx>= num numsubsectors)
    (i-error "P_CrossSubsector: ss ~a with numss = ~a" num numsubsectors))
  (let* ([sub (vector-ref subsectors num)]
         [first (subsector-firstline sub)]
         [end (fx+ first (subsector-numlines sub))])
    ;; check lines
    (let loop ([i first])
      (if (fx= i end)
          #t                            ; passed the subsector ok
          (let* ([seg (vector-ref segs i)]
                 [line (seg-linedef seg)])
            (if (fx= (line-validcount line) validcount)
                (loop (fx+ i 1))        ; allready checked other side?
                (begin
                  (line-validcount-set! line validcount)
                  (let ([v1 (line-v1 line)] [v2 (line-v2 line)])
                    (if (fx= (divline-side (vertex-x v1) (vertex-y v1) strace)
                             (divline-side (vertex-x v2) (vertex-y v2) strace))
                        (loop (fx+ i 1))    ; line isn't crossed?
                        (begin
                          (divline-x-set! divl (vertex-x v1))
                          (divline-y-set! divl (vertex-y v1))
                          (divline-dx-set! divl (i- (vertex-x v2) (vertex-x v1)))
                          (divline-dy-set! divl (i- (vertex-y v2) (vertex-y v1)))
                          (cond
                            [(fx= (divline-side (divline-x strace) (divline-y strace) divl)
                                  (divline-side t2x t2y divl))
                             (loop (fx+ i 1))]  ; line isn't crossed?
                            ;; stop because it is not two sided anyway
                            [(not (fxlogtest (line-flags line) ML_TWOSIDED)) #f]
                            [else
                             ;; crosses a two sided line
                             (let ([front (seg-frontsector seg)] [back (seg-backsector seg)])
                               (if (and (fx= (sector-floorheight front) (sector-floorheight back))
                                        (fx= (sector-ceilingheight front) (sector-ceilingheight back)))
                                   (loop (fx+ i 1)) ; no wall to block sight with?
                                   ;; possible occluder because of ceiling /
                                   ;; floor height differences
                                   (let ([opentop (fxmin (sector-ceilingheight front)
                                                         (sector-ceilingheight back))]
                                         [openbottom (fxmax (sector-floorheight front)
                                                            (sector-floorheight back))])
                                     ;; quick test for totally closed doors
                                     (if (fx>= openbottom opentop)
                                         #f     ; stop
                                         (let ([frac (p-intercept-vector2 strace divl)])
                                           (unless (fx= (sector-floorheight front) (sector-floorheight back))
                                             (let ([slope (fixed-div (i- openbottom sightzstart) frac)])
                                               (when (fx> slope bottomslope) (set! bottomslope slope))))
                                           (unless (fx= (sector-ceilingheight front) (sector-ceilingheight back))
                                             (let ([slope (fixed-div (i- opentop sightzstart) frac)])
                                               (when (fx< slope topslope) (set! topslope slope))))
                                           (if (fx<= topslope bottomslope)
                                               #f   ; stop
                                               (loop (fx+ i 1))))))))])))))))))))

;; P_CrossBSPNode: returns true if strace crosses the given node
;; successfully.
(define (p-cross-bsp-node bspnum)
  (if (fxlogtest bspnum NF_SUBSECTOR)
      (if (fx= bspnum -1)
          (p-cross-subsector 0)
          (p-cross-subsector (fxand bspnum (fxnot NF_SUBSECTOR))))
      (let* ([bsp (vector-ref nodes bspnum)]
             ;; decide which side the start point is on
             [side (node-side (divline-x strace) (divline-y strace) bsp)]
             [side (if (fx= side 2) 0 side)]) ; an "on" should cross both sides
        ;; cross the starting side
        (and (p-cross-bsp-node (fxvector-ref (node-children bsp) side))
             ;; the partition plane is crossed here
             (or (fx= side (node-side t2x t2y bsp))
                 ;; (the line doesn't touch the other side)
                 ;; cross the ending side
                 (p-cross-bsp-node (fxvector-ref (node-children bsp) (fxxor side 1))))))))

;; P_CheckSight: returns true if a straight line between t1 and t2 is
;; unobstructed. Uses REJECT.
(define (p-check-sight t1 t2)
  ;; First check for trivial rejection. Determine subsector entries in
  ;; REJECT table.
  (let* ([s1 (sector-index (subsector-sector (mobj-subsector t1)))]
         [s2 (sector-index (subsector-sector (mobj-subsector t2)))]
         [pnum (fx+ (fx* s1 numsectors) s2)]
         [bytenum (fxsra pnum 3)]
         [bitnum (fxsll 1 (fxand pnum 7))])
    ;; Check in REJECT table. (Past the end of the lump C reads other
    ;; memory; we read 0, see DESIGN.md.)
    (if (fxlogtest (if (fx< bytenum (bytevector-length rejectmatrix))
                       (bytevector-u8-ref rejectmatrix bytenum)
                       0)
                   bitnum)
        (begin
          (fxvector-set! sightcounts 0 (fx+ (fxvector-ref sightcounts 0) 1))
          ;; can't possibly be connected
          #f)
        (begin
          ;; An unobstructed LOS is possible. Now look from eyes of t1 to
          ;; any part of t2.
          (fxvector-set! sightcounts 1 (fx+ (fxvector-ref sightcounts 1) 1))
          (set! validcount (fx+ validcount 1))
          (set! sightzstart (i- (i+ (mobj-z t1) (mobj-height t1)) (fxsra (mobj-height t1) 2)))
          (set! topslope (i- (i+ (mobj-z t2) (mobj-height t2)) sightzstart))
          (set! bottomslope (i- (mobj-z t2) sightzstart))
          (divline-x-set! strace (mobj-x t1))
          (divline-y-set! strace (mobj-y t1))
          (set! t2x (mobj-x t2))
          (set! t2y (mobj-y t2))
          (divline-dx-set! strace (i- (mobj-x t2) (mobj-x t1)))
          (divline-dy-set! strace (i- (mobj-y t2) (mobj-y t1)))
          ;; the head node is the last node output
          (p-cross-bsp-node (fx- numnodes 1))))))
