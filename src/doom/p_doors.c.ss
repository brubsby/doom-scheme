;;; p_doors.c: vertical doors. Included by p_local.sls.

(define (door-sound door sfx)
  (s-start-sound (sector-soundorg (vldoor-sector door)) sfx))

(define (door-done door)
  (sector-specialdata-set! (vldoor-sector door) #f)
  (p-remove-thinker door))              ; unlink and free

;; T_VerticalDoor
(define (t-vertical-door door)
  (let ([type (vldoor-type door)])
    (case (vldoor-direction door)
      [(0)
       ;; WAITING
       (vldoor-topcountdown-set! door (fx- (vldoor-topcountdown door) 1))
       (when (fx= (vldoor-topcountdown door) 0)
         (cond
           [(fx= type blazeRaise)
            (vldoor-direction-set! door -1)   ; time to go back down
            (door-sound door sfx_bdcls)]
           [(fx= type normal)
            (vldoor-direction-set! door -1)   ; time to go back down
            (door-sound door sfx_dorcls)]
           [(fx= type close30ThenOpen)
            (vldoor-direction-set! door 1)
            (door-sound door sfx_doropn)]))]
      [(2)
       ;; INITIAL WAIT
       (vldoor-topcountdown-set! door (fx- (vldoor-topcountdown door) 1))
       (when (and (fx= (vldoor-topcountdown door) 0) (fx= type raiseIn5Mins))
         (vldoor-direction-set! door 1)
         (vldoor-type-set! door normal)
         (door-sound door sfx_doropn))]
      [(-1)
       ;; DOWN
       (let ([res (t-move-plane (vldoor-sector door) (vldoor-speed door)
                                (sector-floorheight (vldoor-sector door)) #f 1 (vldoor-direction door))])
         (cond
           [(fx= res pastdest)
            (cond
              [(or (fx= type blazeRaise) (fx= type blazeClose))
               (door-done door)
               (door-sound door sfx_bdcls)]
              [(or (fx= type normal) (fx= type close))
               (door-done door)]
              [(fx= type close30ThenOpen)
               (vldoor-direction-set! door 0)
               (vldoor-topcountdown-set! door (* 35 30))])]
           [(fx= res crushed)
            (unless (or (fx= type blazeClose) (fx= type close)) ; DO NOT GO BACK UP!
              (vldoor-direction-set! door 1)
              (door-sound door sfx_doropn))]))]
      [(1)
       ;; UP
       (let ([res (t-move-plane (vldoor-sector door) (vldoor-speed door)
                                (vldoor-topheight door) #f 1 (vldoor-direction door))])
         (when (fx= res pastdest)
           (cond
             [(or (fx= type blazeRaise) (fx= type normal))
              (vldoor-direction-set! door 0)    ; wait at top
              (vldoor-topcountdown-set! door (vldoor-topwait door))]
             [(or (fx= type close30ThenOpen) (fx= type blazeOpen) (fx= type open))
              (door-done door)])))])))

;; A key check for locked doors: #t if the player has either key, else
;; shows the message and returns #f.
(define (has-key? p card skull message)
  (or (vector-ref (player-cards p) card)
      (vector-ref (player-cards p) skull)
      (begin
        (player-message-set! p message)
        (s-start-sound #f sfx_oof)
        #f)))

;; EV_DoLockedDoor: move a locked door up/down.
(define (ev-do-locked-door line type thing)
  (let ([p (mobj-player thing)])
    (if (not p)
        0
        (if (case (line-special line)
              [(99 133) (has-key? p it_bluecard it_blueskull PD_BLUEO)]    ; Blue Lock
              [(134 135) (has-key? p it_redcard it_redskull PD_REDO)]      ; Red Lock
              [(136 137) (has-key? p it_yellowcard it_yellowskull PD_YELLOWO)] ; Yellow Lock
              [else #t])
            (ev-do-door line type)
            0))))

(define (ev-do-door line type)
  (let ([rtn 0])
    (for-each-tagged-sector
     line
     (lambda (sec secnum)
       (unless (sector-specialdata sec)
         ;; new door thinker
         (set! rtn 1)
         (let ([door (make-vldoor)])
           (p-add-thinker door)
           (sector-specialdata-set! sec door)
           (thinker-func-set! door t-vertical-door)
           (vldoor-sector-set! door sec)
           (vldoor-type-set! door type)
           (vldoor-topwait-set! door VDOORWAIT)
           (vldoor-speed-set! door VDOORSPEED)
           (cond
             [(fx= type blazeClose)
              (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT)))
              (vldoor-direction-set! door -1)
              (vldoor-speed-set! door (* VDOORSPEED 4))
              (door-sound door sfx_bdcls)]
             [(fx= type close)
              (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT)))
              (vldoor-direction-set! door -1)
              (door-sound door sfx_dorcls)]
             [(fx= type close30ThenOpen)
              (vldoor-topheight-set! door (sector-ceilingheight sec))
              (vldoor-direction-set! door -1)
              (door-sound door sfx_dorcls)]
             [(or (fx= type blazeRaise) (fx= type blazeOpen))
              (vldoor-direction-set! door 1)
              (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT)))
              (vldoor-speed-set! door (* VDOORSPEED 4))
              (unless (fx= (vldoor-topheight door) (sector-ceilingheight sec))
                (door-sound door sfx_bdopn))]
             [(or (fx= type normal) (fx= type open))
              (vldoor-direction-set! door 1)
              (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT)))
              (unless (fx= (vldoor-topheight door) (sector-ceilingheight sec))
                (door-sound door sfx_doropn))])))))
    rtn))

;; EV_VerticalDoor: open a door manually, no tag value.
(define (ev-vertical-door line thing)
  (let ([player (mobj-player thing)]
        [special (line-special line)])
    ;; Check for locks
    (when (case special
            [(26 32) (and player (has-key? player it_bluecard it_blueskull PD_BLUEK))]       ; Blue Lock
            [(27 34) (and player (has-key? player it_yellowcard it_yellowskull PD_YELLOWK))] ; Yellow Lock
            [(28 33) (and player (has-key? player it_redcard it_redskull PD_REDK))]          ; Red Lock
            [else #t])
      ;; if the sector has an active thinker, use it. (only front sides can
      ;; be used, side = 0)
      (let ([sec (side-sector (vector-ref sides (fxvector-ref (line-sidenum line) 1)))])
        (if (and (sector-specialdata sec)
                 ;; ONLY FOR "RAISE" DOORS, NOT "OPEN"s
                 (memv special '(1 26 27 28 117)))
            (let ([door (sector-specialdata sec)])
              ;; C writes door->direction into whatever thinker this is;
              ;; we stop rather than guess at the other struct's layout.
              (unless (vldoor? door)
                (i-error "EV_VerticalDoor: sector's active thinker is not a door"))
              (if (fx= (vldoor-direction door) -1)
                  (vldoor-direction-set! door 1)      ; go back up
                  (when (mobj-player thing)          ; JDC: bad guys never close doors
                    (vldoor-direction-set! door -1)))) ; start going down immediately
            (begin
              ;; for proper sound
              (case special
                [(117 118) (s-start-sound (sector-soundorg sec) sfx_bdopn)] ; BLAZING DOOR RAISE / OPEN
                [else (s-start-sound (sector-soundorg sec) sfx_doropn)])     ; NORMAL / LOCKED DOOR SOUND
              ;; new door thinker
              (let ([door (make-vldoor)])
                (p-add-thinker door)
                (sector-specialdata-set! sec door)
                (thinker-func-set! door t-vertical-door)
                (vldoor-sector-set! door sec)
                (vldoor-direction-set! door 1)
                (vldoor-speed-set! door VDOORSPEED)
                (vldoor-topwait-set! door VDOORWAIT)
                (case special
                  [(1 26 27 28) (vldoor-type-set! door normal)]
                  [(31 32 33 34)
                   (vldoor-type-set! door open)
                   (line-special-set! line 0)]
                  [(117)                            ; blazing door raise
                   (vldoor-type-set! door blazeRaise)
                   (vldoor-speed-set! door (* VDOORSPEED 4))]
                  [(118)                            ; blazing door open
                   (vldoor-type-set! door blazeOpen)
                   (line-special-set! line 0)
                   (vldoor-speed-set! door (* VDOORSPEED 4))])
                ;; find the top and bottom of the movement range
                (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT))))))))))

;; Spawn a door that closes after 30 seconds
(define (p-spawn-door-close-in30 sec)
  (let ([door (make-vldoor)])
    (p-add-thinker door)
    (sector-specialdata-set! sec door)
    (sector-special-set! sec 0)
    (thinker-func-set! door t-vertical-door)
    (vldoor-sector-set! door sec)
    (vldoor-direction-set! door 0)
    (vldoor-type-set! door normal)
    (vldoor-speed-set! door VDOORSPEED)
    (vldoor-topcountdown-set! door (* 30 35))))

;; Spawn a door that opens after 5 minutes
(define (p-spawn-door-raise-in5-mins sec secnum)
  (let ([door (make-vldoor)])
    (p-add-thinker door)
    (sector-specialdata-set! sec door)
    (sector-special-set! sec 0)
    (thinker-func-set! door t-vertical-door)
    (vldoor-sector-set! door sec)
    (vldoor-direction-set! door 2)
    (vldoor-type-set! door raiseIn5Mins)
    (vldoor-speed-set! door VDOORSPEED)
    (vldoor-topheight-set! door (i- (p-find-lowest-ceiling-surrounding sec) (* 4 FRACUNIT)))
    (vldoor-topwait-set! door VDOORWAIT)
    (vldoor-topcountdown-set! door (* 5 60 35))))
