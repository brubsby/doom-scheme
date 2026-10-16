;;; p_floor.c: moving floors and stairs. Included by p_local.sls.

;; T_MovePlane: move a plane (floor or ceiling) and check for crushing.
;; Returns ok, crushed or pastdest.
(define (t-move-plane sector speed dest crush floor-or-ceiling direction)
  (let-values ([(get set) (if (fx= floor-or-ceiling 0)
                              (values sector-floorheight sector-floorheight-set!)
                              (values sector-ceilingheight sector-ceilingheight-set!))])
    (define (move-to! height)
      ;; Returns #t (P_ChangeSector's nofit) if something doesn't fit.
      (set sector height)
      (p-change-sector sector crush))
    (let ([lastpos (get sector)])
      (cond
        ;; FLOOR DOWN, CEILING DOWN
        [(fx= direction -1)
         (if (fx< (i- lastpos speed) dest)
             (begin
               (when (move-to! dest)
                 (move-to! lastpos))
               ;; (return crushed is commented out in C)
               pastdest)
             (if (move-to! (i- lastpos speed))
                 ;; COULD GET CRUSHED (ceilings that crush stay down)
                 (if (and (fx= floor-or-ceiling 1) crush)
                     crushed
                     (begin (move-to! lastpos) crushed))
                 ok))]
        ;; FLOOR UP, CEILING UP
        [(fx= direction 1)
         (if (fx> (i+ lastpos speed) dest)
             (begin
               (when (move-to! dest)
                 (move-to! lastpos))
               pastdest)
             (let ([flag (move-to! (i+ lastpos speed))])
               (cond
                 ;; a rising ceiling ignores what it hits
                 [(fx= floor-or-ceiling 1) ok]
                 ;; COULD GET CRUSHED
                 [(not flag) ok]
                 [crush crushed]
                 [else (move-to! lastpos) crushed])))]
        [else ok]))))

;; T_MoveFloor: move a floor to its destination (up or down).
(define (t-move-floor floor)
  (let* ([sector (floormove-sector floor)]
         [res (t-move-plane sector (floormove-speed floor) (floormove-floordestheight floor)
                            (floormove-crush floor) 0 (floormove-direction floor))])
    (when (fx= (fxand leveltime 7) 0)
      (s-start-sound (sector-soundorg sector) sfx_stnmov))
    (when (fx= res pastdest)
      (sector-specialdata-set! sector #f)
      (when (or (and (fx= (floormove-direction floor) 1) (fx= (floormove-type floor) donutRaise))
                (and (fx= (floormove-direction floor) -1) (fx= (floormove-type floor) lowerAndChange)))
        (sector-special-set! sector (floormove-newspecial floor))
        (sector-floorpic-set! sector (floormove-texture floor)))
      (p-remove-thinker floor)
      (s-start-sound (sector-soundorg sector) sfx_pstop))))

;; EV_DoFloor: handle floor types.
(define (ev-do-floor line floortype)
  (let ([rtn 0])
    (for-each-tagged-sector
     line
     (lambda (sec secnum)
       ;; ALREADY MOVING? IF SO, KEEP GOING...
       (unless (sector-specialdata sec)
         ;; new floor thinker
         (set! rtn 1)
         (let ([floor (make-floormove)])
           (p-add-thinker floor)
           (sector-specialdata-set! sec floor)
           (thinker-func-set! floor t-move-floor)
           (floormove-type-set! floor floortype)
           (floormove-crush-set! floor #f)
           (let ([setup (lambda (direction speed dest)
                          (floormove-direction-set! floor direction)
                          (floormove-sector-set! floor sec)
                          (floormove-speed-set! floor speed)
                          (floormove-floordestheight-set! floor dest))])
             (cond
               [(fx= floortype lowerFloor)
                (setup -1 FLOORSPEED (p-find-highest-floor-surrounding sec))]
               [(fx= floortype lowerFloorToLowest)
                (setup -1 FLOORSPEED (p-find-lowest-floor-surrounding sec))]
               [(fx= floortype turboLower)
                (setup -1 (* FLOORSPEED 4) (p-find-highest-floor-surrounding sec))
                (unless (fx= (floormove-floordestheight floor) (sector-floorheight sec))
                  (floormove-floordestheight-set! floor (i+ (floormove-floordestheight floor) (* 8 FRACUNIT))))]
               [(or (fx= floortype raiseFloorCrush) (fx= floortype raiseFloor))
                (when (fx= floortype raiseFloorCrush)
                  (floormove-crush-set! floor #t))
                (setup 1 FLOORSPEED (p-find-lowest-ceiling-surrounding sec))
                (when (fx> (floormove-floordestheight floor) (sector-ceilingheight sec))
                  (floormove-floordestheight-set! floor (sector-ceilingheight sec)))
                (floormove-floordestheight-set!
                 floor (i- (floormove-floordestheight floor)
                           (if (fx= floortype raiseFloorCrush) (* 8 FRACUNIT) 0)))]
               [(fx= floortype raiseFloorTurbo)
                (setup 1 (* FLOORSPEED 4) (p-find-next-highest-floor sec (sector-floorheight sec)))]
               [(fx= floortype raiseFloorToNearest)
                (setup 1 FLOORSPEED (p-find-next-highest-floor sec (sector-floorheight sec)))]
               [(fx= floortype raiseFloor24)
                (setup 1 FLOORSPEED (i+ (sector-floorheight sec) (* 24 FRACUNIT)))]
               [(fx= floortype raiseFloor512)
                (setup 1 FLOORSPEED (i+ (sector-floorheight sec) (* 512 FRACUNIT)))]
               [(fx= floortype raiseFloor24AndChange)
                (setup 1 FLOORSPEED (i+ (sector-floorheight sec) (* 24 FRACUNIT)))
                (sector-floorpic-set! sec (sector-floorpic (line-frontsector line)))
                (sector-special-set! sec (sector-special (line-frontsector line)))]
               [(fx= floortype raiseToTexture)
                (let ([minsize
                       (let loop ([i 0] [minsize MAXINT])
                         (if (fx= i (sector-linecount sec))
                             minsize
                             (loop (fx+ i 1)
                                   (if (two-sided secnum i)
                                       (let* ([b0 (side-bottomtexture (get-side secnum i 0))]
                                              [minsize (if (and (fx>= b0 0) (fx< (fxvector-ref textureheight b0) minsize))
                                                           (fxvector-ref textureheight b0)
                                                           minsize)]
                                              [b1 (side-bottomtexture (get-side secnum i 1))])
                                         (if (and (fx>= b1 0) (fx< (fxvector-ref textureheight b1) minsize))
                                             (fxvector-ref textureheight b1)
                                             minsize))
                                       minsize))))])
                  (setup 1 FLOORSPEED (i+ (sector-floorheight sec) minsize)))]
               [(fx= floortype lowerAndChange)
                (setup -1 FLOORSPEED (p-find-lowest-floor-surrounding sec))
                (floormove-texture-set! floor (sector-floorpic sec))
                (let loop ([i 0])
                  (when (fx< i (sector-linecount sec))
                    (if (two-sided secnum i)
                        (let ([other (if (fx= (sector-index (side-sector (get-side secnum i 0))) secnum)
                                         (get-sector secnum i 1)
                                         (get-sector secnum i 0))])
                          (if (fx= (sector-floorheight other) (floormove-floordestheight floor))
                              (begin
                                (floormove-texture-set! floor (sector-floorpic other))
                                (floormove-newspecial-set! floor (sector-special other)))
                              (loop (fx+ i 1))))
                        (loop (fx+ i 1)))))]
               [else (void)]))))))
    rtn))

;; BUILD A STAIRCASE! Note that the tag search continues from the last
;; stair built, not from the sector it started at (C reuses secnum).
(define (ev-build-stairs line type)
  (let-values ([(speed stairsize)
                (if (fx= type build8)
                    (values (fxquotient FLOORSPEED 4) (* 8 FRACUNIT))
                    (values (* FLOORSPEED 4) (* 16 FRACUNIT)))])
    (define (start-floor! sec height)
      (let ([floor (make-floormove)])
        (p-add-thinker floor)
        (sector-specialdata-set! sec floor)
        (thinker-func-set! floor t-move-floor)
        (floormove-direction-set! floor 1)
        (floormove-sector-set! floor sec)
        (floormove-speed-set! floor speed)
        (floormove-floordestheight-set! floor height)))
    ;; Raise the stairs that follow sec (number secnum) at height; returns
    ;; the number of the last one.
    (define (build-from sec secnum height texture)
      ;; Find next sector to raise
      ;; 1. Find 2-sided line with same sector side[0]
      ;; 2. Other side is the next sector to raise
      (let loop ([i 0] [height height])
        (if (fx= i (sector-linecount sec))
            secnum
            (let ([li (vector-ref (sector-lines sec) i)])
              (cond
                [(not (fxlogtest (line-flags li) ML_TWOSIDED)) (loop (fx+ i 1) height)]
                [(not (fx= secnum (sector-index (line-frontsector li)))) (loop (fx+ i 1) height)]
                [(not (fx= (sector-floorpic (line-backsector li)) texture)) (loop (fx+ i 1) height)]
                [else
                 (let ([tsec (line-backsector li)] [height (i+ height stairsize)])
                   (if (sector-specialdata tsec)
                       (loop (fx+ i 1) height)
                       (begin
                         (start-floor! tsec height)
                         (build-from tsec (sector-index tsec) height texture))))])))))
    (let loop ([secnum (p-find-sector-from-line-tag line -1)] [rtn 0])
      (if (fx< secnum 0)
          rtn
          (let ([sec (vector-ref sectors secnum)])
            ;; ALREADY MOVING? IF SO, KEEP GOING...
            (if (sector-specialdata sec)
                (loop (p-find-sector-from-line-tag line secnum) rtn)
                (let ([height (i+ (sector-floorheight sec) stairsize)])
                  ;; new floor thinker
                  (start-floor! sec height)
                  (let ([last (build-from sec secnum height (sector-floorpic sec))])
                    (loop (p-find-sector-from-line-tag line last) 1)))))))))
