;;; p_ceilng.c: moving ceilings and crushers. Included by p_local.sls.

(define activeceilings (make-vector MAXCEILINGS #f))

(define (ceiling-sound ceiling sfx)
  (s-start-sound (sector-soundorg (ceilingmove-sector ceiling)) sfx))

;; T_MoveCeiling
(define (t-move-ceiling ceiling)
  (let ([type (ceilingmove-type ceiling)])
    (case (ceilingmove-direction ceiling)
      [(0) (void)]                      ; IN STASIS
      [(1)
       ;; UP
       (let ([res (t-move-plane (ceilingmove-sector ceiling) (ceilingmove-speed ceiling)
                                (ceilingmove-topheight ceiling) #f 1 (ceilingmove-direction ceiling))])
         (when (and (fx= (fxand leveltime 7) 0) (not (fx= type silentCrushAndRaise)))
           (ceiling-sound ceiling sfx_stnmov))
         (when (fx= res pastdest)
           (cond
             [(fx= type raiseToHighest) (p-remove-active-ceiling ceiling)]
             [(fx= type silentCrushAndRaise)
              (ceiling-sound ceiling sfx_pstop)
              (ceilingmove-direction-set! ceiling -1)]
             [(or (fx= type fastCrushAndRaise) (fx= type crushAndRaise))
              (ceilingmove-direction-set! ceiling -1)])))]
      [(-1)
       ;; DOWN
       (let ([res (t-move-plane (ceilingmove-sector ceiling) (ceilingmove-speed ceiling)
                                (ceilingmove-bottomheight ceiling) (ceilingmove-crush ceiling) 1
                                (ceilingmove-direction ceiling))])
         (when (and (fx= (fxand leveltime 7) 0) (not (fx= type silentCrushAndRaise)))
           (ceiling-sound ceiling sfx_stnmov))
         (cond
           [(fx= res pastdest)
            (cond
              [(fx= type silentCrushAndRaise)
               (ceiling-sound ceiling sfx_pstop)
               (ceilingmove-speed-set! ceiling CEILSPEED)
               (ceilingmove-direction-set! ceiling 1)]
              [(fx= type crushAndRaise)
               (ceilingmove-speed-set! ceiling CEILSPEED)
               (ceilingmove-direction-set! ceiling 1)]
              [(fx= type fastCrushAndRaise)
               (ceilingmove-direction-set! ceiling 1)]
              [(or (fx= type lowerAndCrush) (fx= type lowerToFloor))
               (p-remove-active-ceiling ceiling)])]
           [(fx= res crushed)
            (when (or (fx= type silentCrushAndRaise) (fx= type crushAndRaise) (fx= type lowerAndCrush))
              (ceilingmove-speed-set! ceiling (fxquotient CEILSPEED 8)))]))])))

;; EV_DoCeiling: move a ceiling up/down and all around!
(define (ev-do-ceiling line type)
  ;; Reactivate in-stasis ceilings...for certain types.
  (when (or (fx= type fastCrushAndRaise) (fx= type silentCrushAndRaise) (fx= type crushAndRaise))
    (p-activate-in-stasis-ceiling line))
  (let ([rtn 0])
    (for-each-tagged-sector
     line
     (lambda (sec secnum)
       (unless (sector-specialdata sec)
         ;; new door thinker
         (set! rtn 1)
         (let ([ceiling (make-ceilingmove)])
           (p-add-thinker ceiling)
           (sector-specialdata-set! sec ceiling)
           (thinker-func-set! ceiling t-move-ceiling)
           (ceilingmove-sector-set! ceiling sec)
           (ceilingmove-crush-set! ceiling #f)
           (cond
             [(fx= type fastCrushAndRaise)
              (ceilingmove-crush-set! ceiling #t)
              (ceilingmove-topheight-set! ceiling (sector-ceilingheight sec))
              (ceilingmove-bottomheight-set! ceiling (i+ (sector-floorheight sec) (* 8 FRACUNIT)))
              (ceilingmove-direction-set! ceiling -1)
              (ceilingmove-speed-set! ceiling (* CEILSPEED 2))]
             [(memv type (list silentCrushAndRaise crushAndRaise lowerAndCrush lowerToFloor))
              (when (or (fx= type silentCrushAndRaise) (fx= type crushAndRaise))
                (ceilingmove-crush-set! ceiling #t)
                (ceilingmove-topheight-set! ceiling (sector-ceilingheight sec)))
              (ceilingmove-bottomheight-set! ceiling (sector-floorheight sec))
              (unless (fx= type lowerToFloor)
                (ceilingmove-bottomheight-set! ceiling (i+ (ceilingmove-bottomheight ceiling) (* 8 FRACUNIT))))
              (ceilingmove-direction-set! ceiling -1)
              (ceilingmove-speed-set! ceiling CEILSPEED)]
             [(fx= type raiseToHighest)
              (ceilingmove-topheight-set! ceiling (p-find-highest-ceiling-surrounding sec))
              (ceilingmove-direction-set! ceiling 1)
              (ceilingmove-speed-set! ceiling CEILSPEED)])
           (ceilingmove-tag-set! ceiling (sector-tag sec))
           (ceilingmove-type-set! ceiling type)
           (p-add-active-ceiling ceiling)))))
    rtn))

;; Add an active ceiling (silently not, if all 30 slots are taken)
(define (p-add-active-ceiling c)
  (let loop ([i 0])
    (when (fx< i MAXCEILINGS)
      (if (vector-ref activeceilings i)
          (loop (fx+ i 1))
          (vector-set! activeceilings i c)))))

;; Remove a ceiling's thinker
(define (p-remove-active-ceiling c)
  (let loop ([i 0])
    (when (fx< i MAXCEILINGS)
      (if (eq? (vector-ref activeceilings i) c)
          (begin
            (sector-specialdata-set! (ceilingmove-sector c) #f)
            (p-remove-thinker c)
            (vector-set! activeceilings i #f))
          (loop (fx+ i 1))))))

;; Restart a ceiling that's in-stasis
(define (p-activate-in-stasis-ceiling line)
  (vector-for-each
   (lambda (c)
     (when (and c (fx= (ceilingmove-tag c) (line-tag line)) (fx= (ceilingmove-direction c) 0))
       (ceilingmove-direction-set! c (ceilingmove-olddirection c))
       (thinker-func-set! c t-move-ceiling)))
   activeceilings))

;; EV_CeilingCrushStop: stop a ceiling from crushing!
(define (ev-ceilingmove-crush-stop line)
  (let ([rtn 0])
    (vector-for-each
     (lambda (c)
       (when (and c (fx= (ceilingmove-tag c) (line-tag line)) (not (fx= (ceilingmove-direction c) 0)))
         (ceilingmove-olddirection-set! c (ceilingmove-direction c))
         (thinker-func-set! c #f)
         (ceilingmove-direction-set! c 0)     ; in-stasis
         (set! rtn 1)))
     activeceilings)
    rtn))
