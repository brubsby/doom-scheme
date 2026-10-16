;;; p_map.c: movement, collision handling, shooting and aiming. Included by
;;; p_local.sls.

(define tmbbox (make-fxvector 4 0))
(define tmthing #f)
(define tmflags 0)
(define tmx 0)
(define tmy 0)

;; If "floatok" true, move would be ok if within "tmfloorz - tmceilingz".
(define floatok #f)
(define tmfloorz 0)
(define tmceilingz 0)
(define tmdropoffz 0)

;; keep track of the line that lowers the ceiling, so missiles don't
;; explode against sky hack walls
(define ceilingline #f)

;; keep track of special lines as they are hit, but don't process them
;; until the move is proven valid. (C has no bounds check: more than 8
;; overrun the array. We stop with an error instead.)
(define MAXSPECIALCROSS 8)
(define spechit (make-vector MAXSPECIALCROSS #f))
(define numspechit 0)

(define (set-tmbbox! x y radius)
  (fxvector-set! tmbbox BOXTOP (i+ y radius))
  (fxvector-set! tmbbox BOXBOTTOM (i- y radius))
  (fxvector-set! tmbbox BOXRIGHT (i+ x radius))
  (fxvector-set! tmbbox BOXLEFT (i- x radius)))

;; Mapblock range of tmbbox, extended by `pad` on each side.
(define (block-range pad)
  (values (fxsra (i- (i- (fxvector-ref tmbbox BOXLEFT) bmaporgx) pad) MAPBLOCKSHIFT)
          (fxsra (i+ (i- (fxvector-ref tmbbox BOXRIGHT) bmaporgx) pad) MAPBLOCKSHIFT)
          (fxsra (i- (i- (fxvector-ref tmbbox BOXBOTTOM) bmaporgy) pad) MAPBLOCKSHIFT)
          (fxsra (i+ (i- (fxvector-ref tmbbox BOXTOP) bmaporgy) pad) MAPBLOCKSHIFT)))

;; Iterate bx over [xl, xh] (outer) and by over [yl, yh]; stop at the
;; first false.
(define (blocks-every xl xh yl yh iterator func)
  (let outer ([bx xl])
    (or (fx> bx xh)
        (and (let inner ([by yl])
               (or (fx> by yh)
                   (and (iterator bx by func) (inner (fx+ by 1)))))
             (outer (fx+ bx 1))))))

;; TELEPORT MOVE

;; PIT_StompThing
(define (pit-stomp-thing thing)
  (cond
    [(not (fxlogtest (mobj-flags thing) MF_SHOOTABLE)) #t]
    [(let ([blockdist (i+ (mobj-radius thing) (mobj-radius tmthing))])
       (or (fx>= (c-abs (i- (mobj-x thing) tmx)) blockdist)
           (fx>= (c-abs (i- (mobj-y thing) tmy)) blockdist)))
     #t]                                ; didn't hit it
    ;; don't clip against self
    [(eq? thing tmthing) #t]
    ;; monsters don't stomp things except on boss level
    [(and (not (mobj-player tmthing)) (not (fx= gamemap 30))) #f]
    [else (p-damage-mobj thing tmthing tmthing 10000) #t]))

;; P_TeleportMove
(define (p-teleport-move thing x y)
  ;; kill anything occupying the position
  (set! tmthing thing)
  (set! tmflags (mobj-flags thing))
  (set! tmx x)
  (set! tmy y)
  (set-tmbbox! x y (mobj-radius tmthing))
  (let ([newsubsec (r-point-in-subsector x y)])
    (set! ceilingline #f)
    ;; The base floor/ceiling is from the subsector that contains the
    ;; point. Any contacted lines the step closer together will adjust
    ;; them.
    (set! tmfloorz (sector-floorheight (subsector-sector newsubsec)))
    (set! tmdropoffz tmfloorz)
    (set! tmceilingz (sector-ceilingheight (subsector-sector newsubsec)))
    (set! validcount (fx+ validcount 1))
    (set! numspechit 0)
    ;; stomp on any things contacted
    (let-values ([(xl xh yl yh) (block-range MAXRADIUS)])
      (and (blocks-every xl xh yl yh p-block-things-iterator pit-stomp-thing)
           ;; the move is ok, so link the thing into its new position
           (begin
             (p-unset-thing-position thing)
             (mobj-floorz-set! thing tmfloorz)
             (mobj-ceilingz-set! thing tmceilingz)
             (mobj-x-set! thing x)
             (mobj-y-set! thing y)
             (p-set-thing-position thing)
             #t)))))

;; MOVEMENT ITERATOR FUNCTIONS

;; PIT_CheckLine: adjusts tmfloorz and tmceilingz as lines are contacted.
(define (pit-check-line ld)
  (let ([bbox (line-bbox ld)])
    (cond
      [(or (fx<= (fxvector-ref tmbbox BOXRIGHT) (fxvector-ref bbox BOXLEFT))
           (fx>= (fxvector-ref tmbbox BOXLEFT) (fxvector-ref bbox BOXRIGHT))
           (fx<= (fxvector-ref tmbbox BOXTOP) (fxvector-ref bbox BOXBOTTOM))
           (fx>= (fxvector-ref tmbbox BOXBOTTOM) (fxvector-ref bbox BOXTOP)))
       #t]
      [(not (fx= (p-box-on-line-side tmbbox ld) -1)) #t]
      ;; A line has been hit. The moving thing's destination position will
      ;; cross the given line. If this should not be allowed, return false.
      ;; If the line is special, keep track of it to process later if the
      ;; move is proven ok. NOTE: specials are NOT sorted by order, so two
      ;; special lines that are only 8 pixels apart could be crossed in
      ;; either order.
      [(not (line-backsector ld)) #f]   ; one sided line
      [(and (not (fxlogtest (mobj-flags tmthing) MF_MISSILE))
            (or (fxlogtest (line-flags ld) ML_BLOCKING) ; explicitly blocking everything
                (and (not (mobj-player tmthing))         ; block monsters only
                     (fxlogtest (line-flags ld) ML_BLOCKMONSTERS))))
       #f]
      [else
       ;; set openrange, opentop, openbottom
       (p-line-opening ld)
       ;; adjust floor / ceiling heights
       (when (fx< opentop tmceilingz)
         (set! tmceilingz opentop)
         (set! ceilingline ld))
       (when (fx> openbottom tmfloorz)
         (set! tmfloorz openbottom))
       (when (fx< lowfloor tmdropoffz)
         (set! tmdropoffz lowfloor))
       ;; if contacted a special line, add it to the list
       (unless (fx= (line-special ld) 0)
         (when (fx= numspechit MAXSPECIALCROSS)
           (i-error "spechit overflow (more than ~a)" MAXSPECIALCROSS))
         (vector-set! spechit numspechit ld)
         (set! numspechit (fx+ numspechit 1)))
       #t])))

;; PIT_CheckThing
(define (pit-check-thing thing)
  (cond
    [(not (fxlogtest (mobj-flags thing) (fxior MF_SOLID MF_SPECIAL MF_SHOOTABLE))) #t]
    [(let ([blockdist (i+ (mobj-radius thing) (mobj-radius tmthing))])
       (or (fx>= (c-abs (i- (mobj-x thing) tmx)) blockdist)
           (fx>= (c-abs (i- (mobj-y thing) tmy)) blockdist)))
     #t]                                ; didn't hit it
    ;; don't clip against self
    [(eq? thing tmthing) #t]
    ;; check for skulls slamming into things
    [(fxlogtest (mobj-flags tmthing) MF_SKULLFLY)
     (let ([damage (fx* (fx+ (fxremainder (p-random) 8) 1)
                        (mobjinfo-damage (mobj-info tmthing)))])
       (p-damage-mobj thing tmthing tmthing damage))
     (mobj-flags-set! tmthing (fxand (mobj-flags tmthing) (fxnot MF_SKULLFLY)))
     (mobj-momx-set! tmthing 0)
     (mobj-momy-set! tmthing 0)
     (mobj-momz-set! tmthing 0)
     (p-set-mobj-state tmthing (mobjinfo-spawnstate (mobj-info tmthing)))
     #f]                                ; stop moving
    ;; missiles can hit other things
    [(fxlogtest (mobj-flags tmthing) MF_MISSILE)
     (let ([target (mobj-target tmthing)])
       (cond
         ;; see if it went over / under
         [(fx> (mobj-z tmthing) (i+ (mobj-z thing) (mobj-height thing))) #t] ; overhead
         [(fx< (i+ (mobj-z tmthing) (mobj-height tmthing)) (mobj-z thing)) #t] ; underneath
         [(and target
               (or (fx= (mobj-type target) (mobj-type thing))
                   (and (fx= (mobj-type target) MT_KNIGHT) (fx= (mobj-type thing) MT_BRUISER))
                   (and (fx= (mobj-type target) MT_BRUISER) (fx= (mobj-type thing) MT_KNIGHT)))
               ;; Don't hit same species as originator.
               (or (eq? thing target)
                   ;; Explode, but do no damage. Let players missile other
                   ;; players.
                   (not (fx= (mobj-type thing) MT_PLAYER))))
          (eq? thing target)]
         [(not (fxlogtest (mobj-flags thing) MF_SHOOTABLE))
          ;; didn't do any damage
          (not (fxlogtest (mobj-flags thing) MF_SOLID))]
         [else
          ;; damage / explode
          (let ([damage (fx* (fx+ (fxremainder (p-random) 8) 1)
                             (mobjinfo-damage (mobj-info tmthing)))])
            (p-damage-mobj thing tmthing target damage))
          ;; don't traverse any more
          #f]))]
    ;; check for special pickup
    [(fxlogtest (mobj-flags thing) MF_SPECIAL)
     (let ([solid (fxlogtest (mobj-flags thing) MF_SOLID)])
       (when (fxlogtest tmflags MF_PICKUP)
         ;; can remove thing
         (p-touch-special-thing thing tmthing))
       (not solid))]
    [else (not (fxlogtest (mobj-flags thing) MF_SOLID))]))

;; MOVEMENT CLIPPING

;; P_CheckPosition: this is purely informative, nothing is modified (except
;; things picked up).
;; in:  a mobj_t (can be valid or invalid), a position to be checked
;;      (doesn't need to be related to the mobj_t->x,y)
;; during: special things are touched if MF_PICKUP; early out on solid
;;      lines?
;; out: newsubsec, floorz, ceilingz, tmdropoffz (the lowest point
;;      contacted; monsters won't move to a dropoff), speciallines[],
;;      numspeciallines
(define (p-check-position thing x y)
  (set! tmthing thing)
  (set! tmflags (mobj-flags thing))
  (set! tmx x)
  (set! tmy y)
  (set-tmbbox! x y (mobj-radius tmthing))
  (let ([newsubsec (r-point-in-subsector x y)])
    (set! ceilingline #f)
    ;; The base floor / ceiling is from the subsector that contains the
    ;; point. Any contacted lines the step closer together will adjust
    ;; them.
    (set! tmfloorz (sector-floorheight (subsector-sector newsubsec)))
    (set! tmdropoffz tmfloorz)
    (set! tmceilingz (sector-ceilingheight (subsector-sector newsubsec)))
    (set! validcount (fx+ validcount 1))
    (set! numspechit 0)
    (or (fxlogtest tmflags MF_NOCLIP)
        ;; Check things first, possibly picking things up. The bounding box
        ;; is extended by MAXRADIUS because mobj_ts are grouped into
        ;; mapblocks based on their origin point, and can overlap into
        ;; adjacent blocks by up to MAXRADIUS units.
        (and (let-values ([(xl xh yl yh) (block-range MAXRADIUS)])
               (blocks-every xl xh yl yh p-block-things-iterator pit-check-thing))
             ;; check lines
             (let-values ([(xl xh yl yh) (block-range 0)])
               (blocks-every xl xh yl yh p-block-lines-iterator pit-check-line))))))

;; P_TryMove: attempt to move to a new position, crossing special lines
;; unless MF_TELEPORT is set.
(define (p-try-move thing x y)
  (set! floatok #f)
  (and
   (p-check-position thing x y)         ; solid wall or thing
   (or (fxlogtest (mobj-flags thing) MF_NOCLIP)
       (let ([flags (mobj-flags thing)])
         (and (not (fx< (i- tmceilingz tmfloorz) (mobj-height thing))) ; doesn't fit
              (begin (set! floatok #t) #t)
              ;; mobj must lower itself to fit
              (not (and (not (fxlogtest flags MF_TELEPORT))
                        (fx< (i- tmceilingz (mobj-z thing)) (mobj-height thing))))
              ;; too big a step up
              (not (and (not (fxlogtest flags MF_TELEPORT))
                        (fx> (i- tmfloorz (mobj-z thing)) (* 24 FRACUNIT))))
              ;; don't stand over a dropoff
              (not (and (not (fxlogtest flags (fxior MF_DROPOFF MF_FLOAT)))
                        (fx> (i- tmfloorz tmdropoffz) (* 24 FRACUNIT)))))))
   ;; the move is ok, so link the thing into its new position
   (let ([oldx (mobj-x thing)] [oldy (mobj-y thing)])
     (p-unset-thing-position thing)
     (mobj-floorz-set! thing tmfloorz)
     (mobj-ceilingz-set! thing tmceilingz)
     (mobj-x-set! thing x)
     (mobj-y-set! thing y)
     (p-set-thing-position thing)
     ;; if any special lines were hit, do the effect
     (unless (fxlogtest (mobj-flags thing) (fxior MF_TELEPORT MF_NOCLIP))
       (let loop ()
         (unless (fx= numspechit 0)
           (set! numspechit (fx- numspechit 1))
           ;; see if the line was crossed
           (let* ([ld (vector-ref spechit numspechit)]
                  [side (p-point-on-line-side (mobj-x thing) (mobj-y thing) ld)]
                  [oldside (p-point-on-line-side oldx oldy ld)])
             (when (and (not (fx= side oldside)) (not (fx= (line-special ld) 0)))
               (p-cross-special-line (line-index ld) oldside thing)))
           (loop))))
     #t)))

;; P_ThingHeightClip: takes a valid thing and adjusts the thing->floorz,
;; thing->ceilingz, and possibly thing->z. This is called for all nearby
;; monsters whenever a sector changes height. If the thing doesn't fit,
;; the z will be set to the lowest value and false will be returned.
(define (p-thing-height-clip thing)
  (let ([onfloor (fx= (mobj-z thing) (mobj-floorz thing))])
    (p-check-position thing (mobj-x thing) (mobj-y thing))
    ;; what about stranding a monster partially off an edge?
    (mobj-floorz-set! thing tmfloorz)
    (mobj-ceilingz-set! thing tmceilingz)
    (if onfloor
        ;; walking monsters rise and fall with the floor
        (mobj-z-set! thing (mobj-floorz thing))
        ;; don't adjust a floating monster unless forced to
        (when (fx> (i+ (mobj-z thing) (mobj-height thing)) (mobj-ceilingz thing))
          (mobj-z-set! thing (i- (mobj-ceilingz thing) (mobj-height thing)))))
    (not (fx< (i- (mobj-ceilingz thing) (mobj-floorz thing)) (mobj-height thing)))))

;; SLIDE MOVE: allows the player to slide along any angled walls.
(define bestslidefrac 0)
(define secondslidefrac 0)
(define bestslideline #f)
(define secondslideline #f)
(define slidemo #f)
(define tmxmove 0)
(define tmymove 0)

;; P_HitSlideLine: adjusts the xmove / ymove so that the next move will
;; slide along the wall.
(define (p-hit-slide-line ld)
  (cond
    [(fx= (line-slopetype ld) ST_HORIZONTAL) (set! tmymove 0)]
    [(fx= (line-slopetype ld) ST_VERTICAL) (set! tmxmove 0)]
    [else
     (let* ([side (p-point-on-line-side (mobj-x slidemo) (mobj-y slidemo) ld)]
            [lineangle (r-point-to-angle2 0 0 (line-dx ld) (line-dy ld))]
            [lineangle (if (fx= side 1) (angle+ lineangle ANG180) lineangle)]
            [moveangle (r-point-to-angle2 0 0 tmxmove tmymove)]
            [deltaangle (angle- moveangle lineangle)]
            [deltaangle (if (fx> deltaangle ANG180) (angle+ deltaangle ANG180) deltaangle)]
            [lineangle (fxsrl lineangle ANGLETOFINESHIFT)]
            [deltaangle (fxsrl deltaangle ANGLETOFINESHIFT)]
            [movelen (p-aprox-distance tmxmove tmymove)]
            [newlen (fixed-mul movelen (finecosine deltaangle))])
       (set! tmxmove (fixed-mul newlen (finecosine lineangle)))
       (set! tmymove (fixed-mul newlen (fxvector-ref finesine lineangle))))]))

;; PTR_SlideTraverse
(define (ptr-slide-traverse in)
  (unless (intercept-isaline in)
    (i-error "PTR_SlideTraverse: not a line?"))
  (let ([li (intercept-d in)])
    (define (isblocking)
      ;; the line does block movement, see if it is closer than best so far
      (when (fx< (intercept-frac in) bestslidefrac)
        (set! secondslidefrac bestslidefrac)
        (set! secondslideline bestslideline)
        (set! bestslidefrac (intercept-frac in))
        (set! bestslideline li))
      #f)                               ; stop
    (if (not (fxlogtest (line-flags li) ML_TWOSIDED))
        (if (fx= (p-point-on-line-side (mobj-x slidemo) (mobj-y slidemo) li) 1)
            #t                          ; don't hit the back side
            (isblocking))
        (begin
          ;; set openrange, opentop, openbottom
          (p-line-opening li)
          (cond
            [(fx< openrange (mobj-height slidemo)) (isblocking)]              ; doesn't fit
            [(fx< (i- opentop (mobj-z slidemo)) (mobj-height slidemo)) (isblocking)] ; mobj is too high
            [(fx> (i- openbottom (mobj-z slidemo)) (* 24 FRACUNIT)) (isblocking)] ; too big a step up
            ;; this line doesn't block movement
            [else #t])))))

;; P_SlideMove: the momx / momy move is bad, so try to slide along a wall.
;; Find the first line hit, move flush to it, and slide along it. This is
;; a kludgy mess.
(define (p-slide-move mo)
  (set! slidemo mo)
  (let retry ([hitcount 1])
    (define (stairstep)
      (unless (p-try-move mo (mobj-x mo) (i+ (mobj-y mo) (mobj-momy mo)))
        (p-try-move mo (i+ (mobj-x mo) (mobj-momx mo)) (mobj-y mo))))
    (if (fx= hitcount 3)
        (stairstep)                     ; don't loop forever
        ;; trace along the three leading corners
        (let*-values ([(leadx trailx) (if (fx> (mobj-momx mo) 0)
                                          (values (i+ (mobj-x mo) (mobj-radius mo))
                                                  (i- (mobj-x mo) (mobj-radius mo)))
                                          (values (i- (mobj-x mo) (mobj-radius mo))
                                                  (i+ (mobj-x mo) (mobj-radius mo))))]
                      [(leady traily) (if (fx> (mobj-momy mo) 0)
                                          (values (i+ (mobj-y mo) (mobj-radius mo))
                                                  (i- (mobj-y mo) (mobj-radius mo)))
                                          (values (i- (mobj-y mo) (mobj-radius mo))
                                                  (i+ (mobj-y mo) (mobj-radius mo))))])
          (set! bestslidefrac (fx+ FRACUNIT 1))
          (p-path-traverse leadx leady (i+ leadx (mobj-momx mo)) (i+ leady (mobj-momy mo))
                           PT_ADDLINES ptr-slide-traverse)
          (p-path-traverse trailx leady (i+ trailx (mobj-momx mo)) (i+ leady (mobj-momy mo))
                           PT_ADDLINES ptr-slide-traverse)
          (p-path-traverse leadx traily (i+ leadx (mobj-momx mo)) (i+ traily (mobj-momy mo))
                           PT_ADDLINES ptr-slide-traverse)
          ;; move up to the wall
          (if (fx= bestslidefrac (fx+ FRACUNIT 1))
              ;; the move most have hit the middle, so stairstep
              (stairstep)
              ;; fudge a bit to make sure it doesn't hit
              (begin
                (set! bestslidefrac (fx- bestslidefrac #x800))
                (if (and (fx> bestslidefrac 0)
                         (not (p-try-move mo
                                          (i+ (mobj-x mo) (fixed-mul (mobj-momx mo) bestslidefrac))
                                          (i+ (mobj-y mo) (fixed-mul (mobj-momy mo) bestslidefrac)))))
                    (stairstep)
                    ;; Now continue along the wall. First calculate
                    ;; remainder.
                    (begin
                      (set! bestslidefrac (fx- FRACUNIT (fx+ bestslidefrac #x800)))
                      (when (fx> bestslidefrac FRACUNIT)
                        (set! bestslidefrac FRACUNIT))
                      (unless (fx<= bestslidefrac 0)
                        (set! tmxmove (fixed-mul (mobj-momx mo) bestslidefrac))
                        (set! tmymove (fixed-mul (mobj-momy mo) bestslidefrac))
                        (p-hit-slide-line bestslideline) ; clip the moves
                        (mobj-momx-set! mo tmxmove)
                        (mobj-momy-set! mo tmymove)
                        (unless (p-try-move mo (i+ (mobj-x mo) tmxmove) (i+ (mobj-y mo) tmymove))
                          (retry (fx+ hitcount 1))))))))))))

;; P_LineAttack
(define-global linetarget #f)         ; who got hit (or NULL)
(define shootthing #f)
;; Height if not aiming up or down. ???: use slope for monsters?
(define shootz 0)
(define la-damage 0)
(define-global attackrange 0)
(define aimslope 0)
;; (topslope and bottomslope, the slopes to the top and bottom of the
;; target, are p_sight's)

;; PTR_AimTraverse: sets linetarget and aimslope when a target is aimed
;; at.
(define (ptr-aim-traverse in)
  (if (intercept-isaline in)
      (let ([li (intercept-d in)])
        (and
         (fxlogtest (line-flags li) ML_TWOSIDED) ; stop
         ;; Crosses a two sided line. A two sided line will restrict the
         ;; possible target ranges.
         (begin (p-line-opening li)
                (not (fx>= openbottom opentop))) ; stop
         (let ([dist (fixed-mul attackrange (intercept-frac in))])
           (unless (fx= (sector-floorheight (line-frontsector li))
                        (sector-floorheight (line-backsector li)))
             (let ([slope (fixed-div (i- openbottom shootz) dist)])
               (when (fx> slope bottomslope) (set! bottomslope slope))))
           (unless (fx= (sector-ceilingheight (line-frontsector li))
                        (sector-ceilingheight (line-backsector li)))
             (let ([slope (fixed-div (i- opentop shootz) dist)])
               (when (fx< slope topslope) (set! topslope slope))))
           (not (fx<= topslope bottomslope))))) ; stop / shot continues
      ;; shoot a thing
      (let ([th (intercept-d in)])
        (cond
          [(eq? th shootthing) #t]      ; can't shoot self
          [(not (fxlogtest (mobj-flags th) MF_SHOOTABLE)) #t] ; corpse or something
          [else
           ;; check angles to see if the thing can be aimed at
           (let* ([dist (fixed-mul attackrange (intercept-frac in))]
                  [thingtopslope (fixed-div (i- (i+ (mobj-z th) (mobj-height th)) shootz) dist)])
             (if (fx< thingtopslope bottomslope)
                 #t                     ; shot over the thing
                 (let ([thingbottomslope (fixed-div (i- (mobj-z th) shootz) dist)])
                   (if (fx> thingbottomslope topslope)
                       #t               ; shot under the thing
                       ;; this thing can be hit!
                       (let ([thingtopslope (fxmin thingtopslope topslope)]
                             [thingbottomslope (fxmax thingbottomslope bottomslope)])
                         (set! aimslope (fxquotient (i+ thingtopslope thingbottomslope) 2))
                         (set! linetarget th)
                         #f)))))]))))  ; don't go any farther

;; PTR_ShootTraverse
(define (ptr-shoot-traverse in)
  (if (intercept-isaline in)
      (let ([li (intercept-d in)])
        (define (hitline)
          ;; position a bit closer
          (let* ([frac (i- (intercept-frac in) (fixed-div (* 4 FRACUNIT) attackrange))]
                 [x (i+ (divline-x trace-line) (fixed-mul (divline-dx trace-line) frac))]
                 [y (i+ (divline-y trace-line) (fixed-mul (divline-dy trace-line) frac))]
                 [z (i+ shootz (fixed-mul aimslope (fixed-mul frac attackrange)))]
                 [front (line-frontsector li)])
            (unless (and (fx= (sector-ceilingpic front) skyflatnum)
                         ;; don't shoot the sky!
                         (or (fx> z (sector-ceilingheight front))
                             ;; it's a sky hack wall
                             (and (line-backsector li)
                                  (fx= (sector-ceilingpic (line-backsector li)) skyflatnum))))
              ;; Spawn bullet puffs.
              (p-spawn-puff x y z))
            ;; don't go any farther
            #f))
        (unless (fx= (line-special li) 0)
          (p-shoot-special-line shootthing li))
        (if (not (fxlogtest (line-flags li) ML_TWOSIDED))
            (hitline)
            ;; crosses a two sided line
            (begin
              (p-line-opening li)
              (let ([dist (fixed-mul attackrange (intercept-frac in))])
                (cond
                  [(and (not (fx= (sector-floorheight (line-frontsector li))
                                  (sector-floorheight (line-backsector li))))
                        (fx> (fixed-div (i- openbottom shootz) dist) aimslope))
                   (hitline)]
                  [(and (not (fx= (sector-ceilingheight (line-frontsector li))
                                  (sector-ceilingheight (line-backsector li))))
                        (fx< (fixed-div (i- opentop shootz) dist) aimslope))
                   (hitline)]
                  ;; shot continues
                  [else #t])))))
      ;; shoot a thing
      (let ([th (intercept-d in)])
        (cond
          [(eq? th shootthing) #t]      ; can't shoot self
          [(not (fxlogtest (mobj-flags th) MF_SHOOTABLE)) #t] ; corpse or something
          [else
           ;; check angles to see if the thing can be aimed at
           (let* ([dist (fixed-mul attackrange (intercept-frac in))]
                  [thingtopslope (fixed-div (i- (i+ (mobj-z th) (mobj-height th)) shootz) dist)])
             (cond
               [(fx< thingtopslope aimslope) #t] ; shot over the thing
               [(fx> (fixed-div (i- (mobj-z th) shootz) dist) aimslope) #t] ; shot under the thing
               [else
                ;; hit thing: position a bit closer
                (let* ([frac (i- (intercept-frac in) (fixed-div (* 10 FRACUNIT) attackrange))]
                       [x (i+ (divline-x trace-line) (fixed-mul (divline-dx trace-line) frac))]
                       [y (i+ (divline-y trace-line) (fixed-mul (divline-dy trace-line) frac))]
                       [z (i+ shootz (fixed-mul aimslope (fixed-mul frac attackrange)))])
                  ;; Spawn bullet puffs or blod spots, depending on target
                  ;; type.
                  (if (fxlogtest (mobj-flags th) MF_NOBLOOD)
                      (p-spawn-puff x y z)
                      (p-spawn-blood x y z la-damage))
                  (unless (fx= la-damage 0)
                    (p-damage-mobj th shootthing shootthing la-damage))
                  ;; don't go any farther
                  #f)]))]))))

;; The end of a line of `distance` from t1 along fine angle `angle`.
(define (attack-end t1 angle distance)
  (values (i+ (mobj-x t1) (int32 (fx* (fxsra distance FRACBITS) (finecosine angle))))
          (i+ (mobj-y t1) (int32 (fx* (fxsra distance FRACBITS) (fxvector-ref finesine angle))))))

;; P_AimLineAttack
(define (p-aim-line-attack t1 angle distance)
  (let-values ([(x2 y2) (attack-end t1 (fxsrl angle ANGLETOFINESHIFT) distance)])
    (set! shootthing t1)
    (set! shootz (i+ (i+ (mobj-z t1) (fxsra (mobj-height t1) 1)) (* 8 FRACUNIT)))
    ;; can't shoot outside view angles
    (set! topslope (fxquotient (* 100 FRACUNIT) 160))
    (set! bottomslope (fxquotient (* -100 FRACUNIT) 160))
    (set! attackrange distance)
    (set! linetarget #f)
    (p-path-traverse (mobj-x t1) (mobj-y t1) x2 y2 (fxior PT_ADDLINES PT_ADDTHINGS)
                     ptr-aim-traverse)
    (if linetarget aimslope 0)))

;; P_LineAttack: if damage == 0, it is just a test trace that will leave
;; linetarget set.
(define (p-line-attack t1 angle distance slope damage)
  (let-values ([(x2 y2) (attack-end t1 (fxsrl angle ANGLETOFINESHIFT) distance)])
    (set! shootthing t1)
    (set! la-damage damage)
    (set! shootz (i+ (i+ (mobj-z t1) (fxsra (mobj-height t1) 1)) (* 8 FRACUNIT)))
    (set! attackrange distance)
    (set! aimslope slope)
    (p-path-traverse (mobj-x t1) (mobj-y t1) x2 y2 (fxior PT_ADDLINES PT_ADDTHINGS)
                     ptr-shoot-traverse)))

;; USE LINES
(define usething #f)

(define (ptr-use-traverse in)
  (let ([li (intercept-d in)])
    (if (fx= (line-special li) 0)
        (begin
          (p-line-opening li)
          (if (fx<= openrange 0)
              (begin
                (s-start-sound usething sfx_noway)
                ;; can't use through a wall
                #f)
              ;; not a special line, but keep checking
              #t))
        (begin
          (p-use-special-line usething li
                              (if (fx= (p-point-on-line-side (mobj-x usething) (mobj-y usething) li) 1)
                                  1 0))
          ;; can't use for than one special line in a row
          #f))))

;; P_UseLines: looks for special lines in front of the player to activate.
(define (p-use-lines player)
  (set! usething (player-mo player))
  (let ([mo (player-mo player)])
    (let-values ([(x2 y2) (attack-end mo (fxsrl (mobj-angle mo) ANGLETOFINESHIFT) USERANGE)])
      (p-path-traverse (mobj-x mo) (mobj-y mo) x2 y2 PT_ADDLINES ptr-use-traverse))))

;; RADIUS ATTACK
(define bombsource #f)
(define bombspot #f)
(define bombdamage 0)

;; PIT_RadiusAttack: "bombsource" is the creature that caused the explosion
;; at "bombspot".
(define (pit-radius-attack thing)
  (cond
    [(not (fxlogtest (mobj-flags thing) MF_SHOOTABLE)) #t]
    ;; Boss spider and cyborg take no damage from concussion.
    [(or (fx= (mobj-type thing) MT_CYBORG) (fx= (mobj-type thing) MT_SPIDER)) #t]
    [else
     (let* ([dx (c-abs (i- (mobj-x thing) (mobj-x bombspot)))]
            [dy (c-abs (i- (mobj-y thing) (mobj-y bombspot)))]
            [dist (fxsra (i- (fxmax dx dy) (mobj-radius thing)) FRACBITS)]
            [dist (if (fx< dist 0) 0 dist)])
       (when (and (fx< dist bombdamage)
                  ;; must be in direct path
                  (p-check-sight thing bombspot))
         (p-damage-mobj thing bombspot bombsource (fx- bombdamage dist)))
       #t)]))

;; P_RadiusAttack: source is the creature that caused the explosion at
;; spot.
(define (p-radius-attack spot source damage)
  ;; MAXRADIUS is already fixed point, so this overflows: C's 32-bit
  ;; result is just damage<<FRACBITS.
  (let* ([dist (int32 (fxsll (fx+ damage MAXRADIUS) FRACBITS))]
         [yh (fxsra (i- (i+ (mobj-y spot) dist) bmaporgy) MAPBLOCKSHIFT)]
         [yl (fxsra (i- (i- (mobj-y spot) dist) bmaporgy) MAPBLOCKSHIFT)]
         [xh (fxsra (i- (i+ (mobj-x spot) dist) bmaporgx) MAPBLOCKSHIFT)]
         [xl (fxsra (i- (i- (mobj-x spot) dist) bmaporgx) MAPBLOCKSHIFT)])
    (set! bombspot spot)
    (set! bombsource source)
    (set! bombdamage damage)
    (do ([y yl (fx+ y 1)]) ((fx> y yh))
      (do ([x xl (fx+ x 1)]) ((fx> x xh))
        (p-block-things-iterator x y pit-radius-attack)))))

;; SECTOR HEIGHT CHANGING
;; After modifying a sectors floor or ceiling height, call this routine to
;; adjust the positions of all things that touch the sector. If anything
;; doesn't fit anymore, true will be returned. If crunch is true, they will
;; take damage as they are being crushed. If Crunch is false, you should
;; set the sector height back the way it was and call P_ChangeSector again
;; to undo the changes.
(define crushchange #f)
(define nofit #f)

;; PIT_ChangeSector
(define (pit-change-sector thing)
  (cond
    ;; keep checking
    [(p-thing-height-clip thing) #t]
    ;; crunch bodies to giblets
    [(fx<= (mobj-health thing) 0)
     (p-set-mobj-state thing S_GIBS)
     (mobj-flags-set! thing (fxand (mobj-flags thing) (fxnot MF_SOLID)))
     (mobj-height-set! thing 0)
     (mobj-radius-set! thing 0)
     #t]
    ;; crunch dropped items
    [(fxlogtest (mobj-flags thing) MF_DROPPED)
     (p-remove-mobj thing)
     #t]
    ;; assume it is bloody gibs or something
    [(not (fxlogtest (mobj-flags thing) MF_SHOOTABLE)) #t]
    [else
     (set! nofit #t)
     (when (and crushchange (fx= (fxand leveltime 3) 0))
       (p-damage-mobj thing #f #f 10)
       ;; spray blood in a random direction
       (let ([mo (p-spawn-mobj (mobj-x thing) (mobj-y thing)
                               (i+ (mobj-z thing) (fxquotient (mobj-height thing) 2)) MT_BLOOD)])
         (mobj-momx-set! mo (fxsll (p-random-diff) 12))
         (mobj-momy-set! mo (fxsll (p-random-diff) 12))))
     ;; keep checking (crush other things)
     #t]))

;; P_ChangeSector
(define (p-change-sector sector crunch)
  (set! nofit #f)
  (set! crushchange crunch)
  ;; re-check heights for all things near the moving sector
  (let ([bb (sector-blockbox sector)])
    (do ([x (fxvector-ref bb BOXLEFT) (fx+ x 1)]) ((fx> x (fxvector-ref bb BOXRIGHT)))
      (do ([y (fxvector-ref bb BOXBOTTOM) (fx+ y 1)]) ((fx> y (fxvector-ref bb BOXTOP)))
        (p-block-things-iterator x y pit-change-sector))))
  nofit)
