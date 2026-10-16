;;; p_maputl.c: movement and collision utilities. Included by p_local.sls.
;;; So far only the thing position links.
;;; TODO(milestone 4): the rest.

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
        (let ([blockx (fxsra (int32 (fx- (mobj-x thing) bmaporgx)) MAPBLOCKSHIFT)]
              [blocky (fxsra (int32 (fx- (mobj-y thing) bmaporgy)) MAPBLOCKSHIFT)])
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
    (let ([blockx (fxsra (int32 (fx- (mobj-x thing) bmaporgx)) MAPBLOCKSHIFT)]
          [blocky (fxsra (int32 (fx- (mobj-y thing) bmaporgy)) MAPBLOCKSHIFT)])
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
