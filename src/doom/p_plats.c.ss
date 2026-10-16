;;; p_plats.c: platforms (lifts). Included by p_local.sls.

(define activeplats (make-vector MAXPLATS #f))

(define (plat-sound plat sfx)
  (s-start-sound (sector-soundorg (plat-sector plat)) sfx))

;; Move a plat up and down
(define (t-plat-raise plat)
  (let ([type (plat-type plat)])
    (case (plat-status plat)
      [(0)
       ;; up
       (let ([res (t-move-plane (plat-sector plat) (plat-speed plat) (plat-high plat)
                                (plat-crush plat) 0 1)])
         (when (and (or (fx= type raiseAndChange) (fx= type raiseToNearestAndChange))
                    (fx= (fxand leveltime 7) 0))
           (plat-sound plat sfx_stnmov))
         (cond
           [(and (fx= res crushed) (not (plat-crush plat)))
            (plat-count-set! plat (plat-wait plat))
            (plat-status-set! plat down)
            (plat-sound plat sfx_pstart)]
           [(fx= res pastdest)
            (plat-count-set! plat (plat-wait plat))
            (plat-status-set! plat waiting)
            (plat-sound plat sfx_pstop)
            (when (memv type (list blazeDWUS downWaitUpStay raiseAndChange raiseToNearestAndChange))
              (p-remove-active-plat plat))]))]
      [(1)
       ;; down
       (let ([res (t-move-plane (plat-sector plat) (plat-speed plat) (plat-low plat) #f 0 -1)])
         (when (fx= res pastdest)
           (plat-count-set! plat (plat-wait plat))
           (plat-status-set! plat waiting)
           (plat-sound plat sfx_pstop)))]
      [(2)
       ;; waiting
       (plat-count-set! plat (fx- (plat-count plat) 1))
       (when (fx= (plat-count plat) 0)
         (plat-status-set! plat (if (fx= (sector-floorheight (plat-sector plat)) (plat-low plat)) up down))
         (plat-sound plat sfx_pstart))]
      [else (void)])))                  ; in_stasis

;; Do Platforms. "amount" is only used for SOME platforms.
(define (ev-do-plat line type amount)
  ;; Activate all <type> plats that are in_stasis
  (when (fx= type perpetualRaise)
    (p-activate-in-stasis (line-tag line)))
  (let ([rtn 0])
    (for-each-tagged-sector
     line
     (lambda (sec secnum)
       (unless (sector-specialdata sec)
         ;; Find lowest & highest floors around sector
         (set! rtn 1)
         (let ([plat (make-plat)])
           (p-add-thinker plat)
           (plat-type-set! plat type)
           (plat-sector-set! plat sec)
           (sector-specialdata-set! sec plat)
           (thinker-func-set! plat t-plat-raise)
           (plat-crush-set! plat #f)
           (plat-tag-set! plat (line-tag line))
           (cond
             [(fx= type raiseToNearestAndChange)
              (plat-speed-set! plat (fxquotient PLATSPEED 2))
              (sector-floorpic-set! sec (sector-floorpic (side-sector (vector-ref sides (fxvector-ref (line-sidenum line) 0)))))
              (plat-high-set! plat (p-find-next-highest-floor sec (sector-floorheight sec)))
              (plat-wait-set! plat 0)
              (plat-status-set! plat up)
              ;; NO MORE DAMAGE, IF APPLICABLE
              (sector-special-set! sec 0)
              (s-start-sound (sector-soundorg sec) sfx_stnmov)]
             [(fx= type raiseAndChange)
              (plat-speed-set! plat (fxquotient PLATSPEED 2))
              (sector-floorpic-set! sec (sector-floorpic (side-sector (vector-ref sides (fxvector-ref (line-sidenum line) 0)))))
              (plat-high-set! plat (i+ (sector-floorheight sec) (fx* amount FRACUNIT)))
              (plat-wait-set! plat 0)
              (plat-status-set! plat up)
              (s-start-sound (sector-soundorg sec) sfx_stnmov)]
             [(or (fx= type downWaitUpStay) (fx= type blazeDWUS))
              (plat-speed-set! plat (* PLATSPEED (if (fx= type blazeDWUS) 8 4)))
              (plat-low-set! plat (fxmin (p-find-lowest-floor-surrounding sec) (sector-floorheight sec)))
              (plat-high-set! plat (sector-floorheight sec))
              (plat-wait-set! plat (* 35 PLATWAIT))
              (plat-status-set! plat down)
              (s-start-sound (sector-soundorg sec) sfx_pstart)]
             [(fx= type perpetualRaise)
              (plat-speed-set! plat PLATSPEED)
              (plat-low-set! plat (fxmin (p-find-lowest-floor-surrounding sec) (sector-floorheight sec)))
              (plat-high-set! plat (fxmax (p-find-highest-floor-surrounding sec) (sector-floorheight sec)))
              (plat-wait-set! plat (* 35 PLATWAIT))
              (plat-status-set! plat (fxand (p-random) 1))
              (s-start-sound (sector-soundorg sec) sfx_pstart)])
           (p-add-active-plat plat)))))
    rtn))

(define (p-activate-in-stasis tag)
  (vector-for-each
   (lambda (p)
     (when (and p (fx= (plat-tag p) tag) (fx= (plat-status p) in_stasis))
       (plat-status-set! p (plat-oldstatus p))
       (thinker-func-set! p t-plat-raise)))
   activeplats))

(define (ev-stop-plat line)
  (vector-for-each
   (lambda (p)
     (when (and p (not (fx= (plat-status p) in_stasis)) (fx= (plat-tag p) (line-tag line)))
       (plat-oldstatus-set! p (plat-status p))
       (plat-status-set! p in_stasis)
       (thinker-func-set! p #f)))
   activeplats))

(define (p-add-active-plat plat)
  (let loop ([i 0])
    (cond
      [(fx= i MAXPLATS) (i-error "P_AddActivePlat: no more plats!")]
      [(vector-ref activeplats i) (loop (fx+ i 1))]
      [else (vector-set! activeplats i plat)])))

(define (p-remove-active-plat plat)
  (let loop ([i 0])
    (cond
      [(fx= i MAXPLATS) (i-error "P_RemoveActivePlat: can't find plat!")]
      [(eq? (vector-ref activeplats i) plat)
       (sector-specialdata-set! (plat-sector plat) #f)
       (p-remove-thinker plat)
       (vector-set! activeplats i #f)]
      [else (loop (fx+ i 1))])))
