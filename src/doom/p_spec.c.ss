;;; p_spec.c (and p_spec.h): sector and line specials: the thinker types of
;;; the world effects, sector utilities, line triggers, special sectors,
;;; animations, and spawning. Included by p_local.sls.

;; Define values for map objects
(define MO_TELEPORTMAN 14)

;; P_LIGHTS
(define-struct fireflicker (thinker) (sector #f) (count 0) (maxlight 0) (minlight 0))
(define-struct lightflash (thinker)
  (sector #f) (count 0) (maxlight 0) (minlight 0) (maxtime 0) (mintime 0))
(define-struct strobe (thinker)
  (sector #f) (count 0) (minlight 0) (maxlight 0) (darktime 0) (brighttime 0))
(define-struct glow (thinker) (sector #f) (minlight 0) (maxlight 0) (direction 0))

(define GLOWSPEED 8)
(define STROBEBRIGHT 5)
(define FASTDARK 15)
(define SLOWDARK 35)

;; P_SWITCH
;; bwhere_e
(define top 0)
(define middle 1)
(define bottom 2)

;; button_t. soundorg is a degenmobj (the line's front sector's).
(define-struct button () (line #f) (where 0) (btexture 0) (btimer 0) (soundorg #f))

;; max # of wall switches in a level
(define MAXSWITCHES 50)
;; 4 players, 4 buttons each at once, max.
(define MAXBUTTONS 16)
;; 1 second, in ticks.
(define BUTTONTIME 35)

;; P_PLATS
;; plat_e
(define up 0)
(define down 1)
(define waiting 2)
(define in_stasis 3)
;; plattype_e
(define perpetualRaise 0)
(define downWaitUpStay 1)
(define raiseAndChange 2)
(define raiseToNearestAndChange 3)
(define blazeDWUS 4)

(define-struct plat (thinker)
  (sector #f) (speed 0) (low 0) (high 0) (wait 0) (count 0)
  (status 0) (oldstatus 0) (crush #f) (tag 0) (type 0))

(define PLATWAIT 3)
(define PLATSPEED FRACUNIT)
(define MAXPLATS 30)

;; P_DOORS
;; vldoor_e
(define normal 0)
(define close30ThenOpen 1)
(define close 2)
(define open 3)
(define raiseIn5Mins 4)
(define blazeRaise 5)
(define blazeOpen 6)
(define blazeClose 7)

(define-struct vldoor (thinker)
  (type 0) (sector #f) (topheight 0) (speed 0)
  (direction 0)       ; 1 = up, 0 = waiting at top, -1 = down
  (topwait 0)         ; tics to wait at the top
  ;; (keep in case a door going down is reset)
  ;; when it reaches 0, start going down
  (topcountdown 0))

(define VDOORSPEED (* FRACUNIT 2))
(define VDOORWAIT 150)

;; P_CEILNG
;; ceiling_e
(define lowerToFloor 0)
(define raiseToHighest 1)
(define lowerAndCrush 2)
(define crushAndRaise 3)
(define fastCrushAndRaise 4)
(define silentCrushAndRaise 5)

(define-struct ceilingmove (thinker)  ; ceiling_t
  (type 0) (sector #f) (bottomheight 0) (topheight 0) (speed 0) (crush #f)
  (direction 0)       ; 1 = up, 0 = waiting, -1 = down
  (tag 0)             ; ID
  (olddirection 0))

(define CEILSPEED FRACUNIT)
(define CEILWAIT 150)
(define MAXCEILINGS 30)

;; P_FLOOR
;; floor_e
(define lowerFloor 0)           ; lower floor to highest surrounding floor
(define lowerFloorToLowest 1)   ; lower floor to lowest surrounding floor
(define turboLower 2)           ; lower floor to highest surrounding floor VERY FAST
(define raiseFloor 3)           ; raise floor to lowest surrounding CEILING
(define raiseFloorToNearest 4)  ; raise floor to next highest surrounding floor
(define raiseToTexture 5)       ; raise floor to shortest height texture around it
(define lowerAndChange 6)       ; lower floor to lowest surrounding floor and change floorpic
(define raiseFloor24 7)
(define raiseFloor24AndChange 8)
(define raiseFloorCrush 9)
(define raiseFloorTurbo 10)     ; raise to next highest floor, turbo-speed
(define donutRaise 11)
(define raiseFloor512 12)
;; stair_e
(define build8 0)               ; slowly build by 8
(define turbo16 1)              ; quickly build by 16

;; floormove_t. C leaves newspecial and texture uninitialized for most
;; types; they are only read for donutRaise and lowerAndChange.
(define-struct floormove (thinker)
  (type 0) (crush #f) (sector #f) (direction 0) (newspecial 0) (texture 0)
  (floordestheight 0) (speed 0))

(define FLOORSPEED FRACUNIT)

;; result_e
(define ok 0)
(define crushed 1)
(define pastdest 2)

;; Animating textures and planes. (There is another anim_t used in
;; wi_stuff, unrelated.)
(define-struct anim () (istexture #f) (picnum 0) (basepic 0) (numpics 0) (speed 0))

;; Source animation definitions: istexture, endname, startname, speed.
(define animdefs
  '((#f "NUKAGE3" "NUKAGE1" 8)
    (#f "FWATER4" "FWATER1" 8)
    (#f "SWATER4" "SWATER1" 8)
    (#f "LAVA4" "LAVA1" 8)
    (#f "BLOOD3" "BLOOD1" 8)
    ;; DOOM II flat animations.
    (#f "RROCK08" "RROCK05" 8)
    (#f "SLIME04" "SLIME01" 8)
    (#f "SLIME08" "SLIME05" 8)
    (#f "SLIME12" "SLIME09" 8)
    (#t "BLODGR4" "BLODGR1" 8)
    (#t "SLADRIP3" "SLADRIP1" 8)
    (#t "BLODRIP4" "BLODRIP1" 8)
    (#t "FIREWALL" "FIREWALA" 8)
    (#t "GSTFONT3" "GSTFONT1" 8)
    (#t "FIRELAVA" "FIRELAV3" 8)
    (#t "FIREMAG3" "FIREMAG1" 8)
    (#t "FIREBLU2" "FIREBLU1" 8)
    (#t "ROCKRED3" "ROCKRED1" 8)
    (#t "BFALL4" "BFALL1" 8)
    (#t "SFALL4" "SFALL1" 8)
    (#t "WFALL4" "WFALL1" 8)
    (#t "DBRAIN4" "DBRAIN1" 8)))

(define MAXANIMS 32)
(define anims '())                  ; list of anim, in definition order

;; P_InitPicAnims: floor/ceiling animation sequences, defined by first and
;; last frame, i.e. the flat (64x64 tile) name to be used. The full
;; animation sequence is given using all the flats between the start and
;; end entry, in the order found in the WAD file.
(define (p-init-pic-anims)
  (set! anims
        (let loop ([defs animdefs] [acc '()])
          (if (null? defs)
              (reverse acc)
              (apply
               (lambda (istexture endname startname speed)
                 (if (if istexture
                         ;; different episode ?
                         (fx= (r-check-texture-num-for-name startname) -1)
                         (fx= (w-check-num-for-name startname) -1))
                     (loop (cdr defs) acc)
                     (let* ([picnum (if istexture (r-texture-num-for-name endname) (r-flat-num-for-name endname))]
                            [basepic (if istexture (r-texture-num-for-name startname) (r-flat-num-for-name startname))]
                            [numpics (fx+ (fx- picnum basepic) 1)])
                       (when (fx< numpics 2)
                         (i-error "P_InitPicAnims: bad cycle from ~a to ~a" startname endname))
                       (loop (cdr defs) (cons (make-anim istexture picnum basepic numpics speed) acc)))))
               (car defs))))))

;; UTILITIES

;; getSide(): the side_t of the line-th line of a sector.
(define (get-side current-sector line side)
  (vector-ref sides (fxvector-ref (line-sidenum (vector-ref (sector-lines (vector-ref sectors current-sector)) line))
                                  side)))

;; getSector(): the sector on the given side of the line-th line of a
;; sector.
(define (get-sector current-sector line side)
  (side-sector (get-side current-sector line side)))

;; twoSided(): whether the line-th line of a sector is two-sided.
(define (two-sided sector line)
  (fxlogtest (line-flags (vector-ref (sector-lines (vector-ref sectors sector)) line)) ML_TWOSIDED))

;; getNextSector(): the sector on the other side of the line, #f if not
;; two-sided.
(define (get-next-sector line sec)
  (cond
    [(not (fxlogtest (line-flags line) ML_TWOSIDED)) #f]
    [(eq? (line-frontsector line) sec) (line-backsector line)]
    [else (line-frontsector line)]))

;; Fold f over the sectors adjacent to sec (in line order), from init.
(define (fold-neighbors sec init f)
  (let ([ls (sector-lines sec)])
    (let loop ([i 0] [acc init])
      (if (fx= i (sector-linecount sec))
          acc
          (let ([other (get-next-sector (vector-ref ls i) sec)])
            (loop (fx+ i 1) (if other (f other acc) acc)))))))

;; P_FindLowestFloorSurrounding()
(define (p-find-lowest-floor-surrounding sec)
  (fold-neighbors sec (sector-floorheight sec)
                  (lambda (other floor) (fxmin (sector-floorheight other) floor))))

;; P_FindHighestFloorSurrounding()
(define (p-find-highest-floor-surrounding sec)
  (fold-neighbors sec (* -500 FRACUNIT)
                  (lambda (other floor) (fxmax (sector-floorheight other) floor))))

;; P_FindNextHighestFloor: the lowest of the surrounding floors above
;; currentheight. Note: this should be doable w/o a fixed array. 20
;; adjoining sectors max!
(define MAX_ADJOINING_SECTORS 20)
(define (p-find-next-highest-floor sec currentheight)
  (let ([ls (sector-lines sec)])
    (let loop ([i 0] [heights '()] [h 0])
      (if (fx= i (sector-linecount sec))
          (if (null? heights) currentheight (apply fxmin heights))
          (let* ([other (get-next-sector (vector-ref ls i) sec)]
                 [higher (and other (fx> (sector-floorheight other) currentheight))]
                 [heights (if higher (cons (sector-floorheight other) heights) heights)]
                 [h (if higher (fx+ h 1) h)])
            ;; Check for overflow. Exit.
            (if (fx>= h MAX_ADJOINING_SECTORS)
                (begin
                  (display "Sector with more than 20 adjoining sectors\n" (current-error-port))
                  (apply fxmin heights))
                (loop (fx+ i 1) heights h)))))))

;; FIND LOWEST CEILING IN THE SURROUNDING SECTORS
(define (p-find-lowest-ceiling-surrounding sec)
  (fold-neighbors sec MAXINT
                  (lambda (other height) (fxmin (sector-ceilingheight other) height))))

;; FIND HIGHEST CEILING IN THE SURROUNDING SECTORS
(define (p-find-highest-ceiling-surrounding sec)
  (fold-neighbors sec 0
                  (lambda (other height) (fxmax (sector-ceilingheight other) height))))

;; RETURN NEXT SECTOR # THAT LINE TAG REFERS TO
(define (p-find-sector-from-line-tag line start)
  (let loop ([i (fx+ start 1)])
    (cond [(fx>= i numsectors) -1]
          [(fx= (sector-tag (vector-ref sectors i)) (line-tag line)) i]
          [else (loop (fx+ i 1))])))

;; Call (f sec) for each sector tagged like the line, in order.
(define (for-each-tagged-sector line f)
  (let loop ([secnum (p-find-sector-from-line-tag line -1)])
    (when (fx>= secnum 0)
      (f (vector-ref sectors secnum) secnum)
      (loop (p-find-sector-from-line-tag line secnum)))))

;; Find minimum light from an adjacent sector
(define (p-find-min-surrounding-light sector max)
  (fold-neighbors sector max
                  (lambda (check min) (fxmin (sector-lightlevel check) min))))

;; G_ExitLevel and G_SecretExitLevel (g_game.c). Here because the playsim
;; calls them and g_game is above the playsim.
(define (g-exit-level)
  (set! secretexit #f)
  (set! gameaction ga_completed))

;; Here's for the german edition.
(define (g-secret-exit-level)
  ;; IF NO WOLF3D LEVELS, NO SECRET EXIT!
  (set! secretexit (not (and (fx= gamemode commercial) (fx< (w-check-num-for-name "map31") 0))))
  (set! gameaction ga_completed))

;; EVENTS
;; Events are operations triggered by using, crossing, or shooting special
;; lines, or by timed thinkers.

;; P_CrossSpecialLine - TRIGGER: called every time a thing origin is about
;; to cross a line with a non 0 special.
(define (p-cross-special-line linenum side thing)
  (let ([line (vector-ref lines linenum)])
    (define (once) (line-special-set! line 0))
    (when (or (mobj-player thing)
              ;; Triggers that other things can activate. Things that
              ;; should NOT trigger specials...
              (and (not (memv (mobj-type thing)
                              (list MT_ROCKET MT_PLASMA MT_BFG MT_TROOPSHOT MT_HEADSHOT
                                    MT_BRUISERSHOT)))
                   (memv (line-special line)
                         ;; TELEPORT TRIGGER / RETRIGGER / MONSTERONLY TRIGGER /
                         ;; RETRIGGER, RAISE DOOR, PLAT DOWN-WAIT-UP-STAY TRIGGER
                         ;; / RETRIGGER
                         '(39 97 125 126 4 10 88))))
      ;; Note: could use some const's here.
      (case (line-special line)
        ;; TRIGGERS. All from here to RETRIGGERS.
        [(2) (ev-do-door line open) (once)]                  ; Open Door
        [(3) (ev-do-door line close) (once)]                 ; Close Door
        [(4) (ev-do-door line normal) (once)]                ; Raise Door
        [(5) (ev-do-floor line raiseFloor) (once)]           ; Raise Floor
        [(6) (ev-do-ceiling line fastCrushAndRaise) (once)]  ; Fast Ceiling Crush & Raise
        [(8) (ev-build-stairs line build8) (once)]           ; Build Stairs
        [(10) (ev-do-plat line downWaitUpStay 0) (once)]     ; PlatDownWaitUp
        [(12) (ev-light-turn-on line 0) (once)]              ; Light Turn On - brightest near
        [(13) (ev-light-turn-on line 255) (once)]            ; Light Turn On 255
        [(16) (ev-do-door line close30ThenOpen) (once)]      ; Close Door 30
        [(17) (ev-start-light-strobing line) (once)]         ; Start Light Strobing
        [(19) (ev-do-floor line lowerFloor) (once)]          ; Lower Floor
        [(22) (ev-do-plat line raiseToNearestAndChange 0) (once)] ; Raise floor to nearest height and change texture
        [(25) (ev-do-ceiling line crushAndRaise) (once)]     ; Ceiling Crush and Raise
        [(30) (ev-do-floor line raiseToTexture) (once)]      ; Raise floor to shortest texture height
        [(35) (ev-light-turn-on line 35) (once)]             ; Lights Very Dark
        [(36) (ev-do-floor line turboLower) (once)]          ; Lower Floor (TURBO)
        [(37) (ev-do-floor line lowerAndChange) (once)]      ; LowerAndChange
        [(38) (ev-do-floor line lowerFloorToLowest) (once)]  ; Lower Floor To Lowest
        [(39) (ev-teleport line side thing) (once)]          ; TELEPORT!
        [(40) (ev-do-ceiling line raiseToHighest)            ; RaiseCeilingLowerFloor
              (ev-do-floor line lowerFloorToLowest)
              (once)]
        [(44) (ev-do-ceiling line lowerAndCrush) (once)]     ; Ceiling Crush
        [(52) (g-exit-level)]                                ; EXIT!
        [(53) (ev-do-plat line perpetualRaise 0) (once)]     ; Perpetual Platform Raise
        [(54) (ev-stop-plat line) (once)]                    ; Platform Stop
        [(56) (ev-do-floor line raiseFloorCrush) (once)]     ; Raise Floor Crush
        [(57) (ev-ceilingmove-crush-stop line) (once)]           ; Ceiling Crush Stop
        [(58) (ev-do-floor line raiseFloor24) (once)]        ; Raise Floor 24
        [(59) (ev-do-floor line raiseFloor24AndChange) (once)] ; Raise Floor 24 And Change
        [(104) (ev-turn-tag-lights-off line) (once)]         ; Turn lights off in sector(tag)
        [(108) (ev-do-door line blazeRaise) (once)]          ; Blazing Door Raise (faster than TURBO!)
        [(109) (ev-do-door line blazeOpen) (once)]           ; Blazing Door Open (faster than TURBO!)
        [(100) (ev-build-stairs line turbo16) (once)]        ; Build Stairs Turbo 16
        [(110) (ev-do-door line blazeClose) (once)]          ; Blazing Door Close (faster than TURBO!)
        [(119) (ev-do-floor line raiseFloorToNearest) (once)] ; Raise floor to nearest surr. floor
        [(121) (ev-do-plat line blazeDWUS 0) (once)]         ; Blazing PlatDownWaitUpStay
        [(124) (g-secret-exit-level)]                        ; Secret EXIT
        [(125)                                               ; TELEPORT MonsterONLY
         (unless (mobj-player thing)
           (ev-teleport line side thing)
           (once))]
        [(130) (ev-do-floor line raiseFloorTurbo) (once)]    ; Raise Floor Turbo
        [(141) (ev-do-ceiling line silentCrushAndRaise) (once)] ; Silent Ceiling Crush & Raise
        ;; RETRIGGERS. All from here till end.
        [(72) (ev-do-ceiling line lowerAndCrush)]            ; Ceiling Crush
        [(73) (ev-do-ceiling line crushAndRaise)]            ; Ceiling Crush and Raise
        [(74) (ev-ceilingmove-crush-stop line)]                  ; Ceiling Crush Stop
        [(75) (ev-do-door line close)]                       ; Close Door
        [(76) (ev-do-door line close30ThenOpen)]             ; Close Door 30
        [(77) (ev-do-ceiling line fastCrushAndRaise)]        ; Fast Ceiling Crush & Raise
        [(79) (ev-light-turn-on line 35)]                    ; Lights Very Dark
        [(80) (ev-light-turn-on line 0)]                     ; Light Turn On - brightest near
        [(81) (ev-light-turn-on line 255)]                   ; Light Turn On 255
        [(82) (ev-do-floor line lowerFloorToLowest)]         ; Lower Floor To Lowest
        [(83) (ev-do-floor line lowerFloor)]                 ; Lower Floor
        [(84) (ev-do-floor line lowerAndChange)]             ; LowerAndChange
        [(86) (ev-do-door line open)]                        ; Open Door
        [(87) (ev-do-plat line perpetualRaise 0)]            ; Perpetual Platform Raise
        [(88) (ev-do-plat line downWaitUpStay 0)]            ; PlatDownWaitUp
        [(89) (ev-stop-plat line)]                           ; Platform Stop
        [(90) (ev-do-door line normal)]                      ; Raise Door
        [(91) (ev-do-floor line raiseFloor)]                 ; Raise Floor
        [(92) (ev-do-floor line raiseFloor24)]               ; Raise Floor 24
        [(93) (ev-do-floor line raiseFloor24AndChange)]      ; Raise Floor 24 And Change
        [(94) (ev-do-floor line raiseFloorCrush)]            ; Raise Floor Crush
        [(95) (ev-do-plat line raiseToNearestAndChange 0)]   ; Raise floor to nearest height and change texture.
        [(96) (ev-do-floor line raiseToTexture)]             ; Raise floor to shortest texture height
        [(97) (ev-teleport line side thing)]                 ; TELEPORT!
        [(98) (ev-do-floor line turboLower)]                 ; Lower Floor (TURBO)
        [(105) (ev-do-door line blazeRaise)]                 ; Blazing Door Raise (faster than TURBO!)
        [(106) (ev-do-door line blazeOpen)]                  ; Blazing Door Open (faster than TURBO!)
        [(107) (ev-do-door line blazeClose)]                 ; Blazing Door Close (faster than TURBO!)
        [(120) (ev-do-plat line blazeDWUS 0)]                ; Blazing PlatDownWaitUpStay.
        [(126)                                               ; TELEPORT MonsterONLY.
         (unless (mobj-player thing)
           (ev-teleport line side thing))]
        [(128) (ev-do-floor line raiseFloorToNearest)]       ; Raise To Nearest Floor
        [(129) (ev-do-floor line raiseFloorTurbo)]           ; Raise Floor Turbo
        [else (void)]))))

;; P_ShootSpecialLine - IMPACT SPECIALS: called when a thing shoots a
;; special line.
(define (p-shoot-special-line thing line)
  ;; Impacts that other things can activate.
  (when (or (mobj-player thing) (fx= (line-special line) 46)) ; OPEN DOOR IMPACT
    (case (line-special line)
      [(24) (ev-do-floor line raiseFloor)                ; RAISE FLOOR
            (p-change-switch-texture line 0)]
      [(46) (ev-do-door line open)                       ; OPEN DOOR
            (p-change-switch-texture line 1)]
      [(47) (ev-do-plat line raiseToNearestAndChange 0)  ; RAISE FLOOR NEAR AND CHANGE
            (p-change-switch-texture line 0)]
      [else (void)])))

;; P_PlayerInSpecialSector: called every tic frame that the player origin
;; is in a special sector.
(define (p-player-in-special-sector player)
  (let* ([mo (player-mo player)]
         [sector (subsector-sector (mobj-subsector mo))])
    ;; Falling, not all the way down yet?
    (when (fx= (mobj-z mo) (sector-floorheight sector))
      ;; Has hitten ground.
      (let ([ironfeet (not (fx= (fxvector-ref (player-powers player) pw_ironfeet) 0))]
            [hurt-tic (fx= (fxand leveltime #x1f) 0)])
        (case (sector-special sector)
          [(5)
           ;; HELLSLIME DAMAGE
           (when (and (not ironfeet) hurt-tic)
             (p-damage-mobj mo #f #f 10))]
          [(7)
           ;; NUKAGE DAMAGE
           (when (and (not ironfeet) hurt-tic)
             (p-damage-mobj mo #f #f 5))]
          [(16 4)
           ;; SUPER HELLSLIME DAMAGE, STROBE HURT
           (when (and (or (not ironfeet) (fx< (p-random) 5)) hurt-tic)
             (p-damage-mobj mo #f #f 20))]
          [(9)
           ;; SECRET SECTOR
           (player-secretcount-set! player (fx+ (player-secretcount player) 1))
           (sector-special-set! sector 0)]
          [(11)
           ;; EXIT SUPER DAMAGE! (for E1M8 finale)
           (player-cheats-set! player (fxand (player-cheats player) (fxnot CF_GODMODE)))
           (when hurt-tic
             (p-damage-mobj mo #f #f 20))
           (when (fx<= (player-health player) 10)
             (g-exit-level))]
          [else
           (i-error "P_PlayerInSpecialSector: unknown special ~a" (sector-special sector))])))))

;; P_UpdateSpecials: animate planes, scroll walls, etc.
(define levelTimer #f)
(define levelTimeCount 0)

(define (p-update-specials)
  ;; LEVEL TIMER
  (when levelTimer
    (set! levelTimeCount (fx- levelTimeCount 1))
    (when (fx= levelTimeCount 0)
      (g-exit-level)))
  ;; ANIMATE FLATS AND TEXTURES GLOBALLY
  (for-each
   (lambda (anim)
     (let ([basepic (anim-basepic anim)] [numpics (anim-numpics anim)])
       (do ([i basepic (fx+ i 1)]) ((fx= i (fx+ basepic numpics)))
         (let ([pic (fx+ basepic (fxremainder (fx+ (fxquotient leveltime (anim-speed anim)) i) numpics))])
           (if (anim-istexture anim)
               (fxvector-set! texturetranslation i pic)
               (fxvector-set! flattranslation i pic))))))
   anims)
  ;; ANIMATE LINE SPECIALS
  (do ([i 0 (fx+ i 1)]) ((fx= i numlinespecials))
    (let ([line (vector-ref linespeciallist i)])
      (when (fx= (line-special line) 48)
        ;; EFFECT FIRSTCOL SCROLL +
        (let ([side (vector-ref sides (fxvector-ref (line-sidenum line) 0))])
          (side-textureoffset-set! side (i+ (side-textureoffset side) FRACUNIT))))))
  ;; DO BUTTONS
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXBUTTONS))
    (let ([b (vector-ref buttonlist i)])
      (unless (fx= (button-btimer b) 0)
        (button-btimer-set! b (fx- (button-btimer b) 1))
        (when (fx= (button-btimer b) 0)
          (let ([side (vector-ref sides (fxvector-ref (line-sidenum (button-line b)) 0))])
            (case (button-where b)
              [(0) (side-toptexture-set! side (button-btexture b))]    ; top
              [(1) (side-midtexture-set! side (button-btexture b))]    ; middle
              [(2) (side-bottomtexture-set! side (button-btexture b))])) ; bottom
          ;; C: S_StartSound((mobj_t *)&buttonlist[i].soundorg, ...): the
          ;; address of the soundorg field, taken as an mobj, whose x and y
          ;; (12 and 16 bytes on) are the next button's btexture and btimer.
          ;; (Past the last button C reads whatever follows; 0 here.)
          (let ([org (vector-ref button-sound-origins i)]
                [next (and (fx< (fx+ i 1) MAXBUTTONS) (vector-ref buttonlist (fx+ i 1)))])
            (degenmobj-x-set! org (if next (button-btexture next) 0))
            (degenmobj-y-set! org (if next (button-btimer next) 0))
            (s-start-sound org sfx_swtchn))
          (struct-copy! b (make-button)))))))

;; The "mobjs" P_UpdateSpecials plays a button's sound from: one per button
;; slot, as C's &buttonlist[i].soundorg is.
(define button-sound-origins
  (let ([v (make-vector MAXBUTTONS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXBUTTONS) v)
      (vector-set! v i (make-degenmobj)))))

;; Special Stuff that can not be categorized

;; EV_DoDonut. (C's `!s2->lines[i]->flags & ML_TWOSIDED` is
;; `(!flags) & ML_TWOSIDED`, always 0, so only the backsector test counts.)
(define (ev-do-donut line)
  (let ([rtn 0])
    (for-each-tagged-sector
     line
     (lambda (s1 secnum)
       ;; ALREADY MOVING? IF SO, KEEP GOING...
       (unless (sector-specialdata s1)
         (set! rtn 1)
         (let ([s2 (get-next-sector (vector-ref (sector-lines s1) 0) s1)])
           (let loop ([i 0])
             (when (fx< i (sector-linecount s2))
               (let ([li (vector-ref (sector-lines s2) i)])
                 (if (eq? (line-backsector li) s1)
                     (loop (fx+ i 1))
                     (let ([s3 (line-backsector li)])
                       ;; Spawn rising slime
                       (let ([floor (make-floormove)])
                         (p-add-thinker floor)
                         (sector-specialdata-set! s2 floor)
                         (thinker-func-set! floor t-move-floor)
                         (floormove-type-set! floor donutRaise)
                         (floormove-crush-set! floor #f)
                         (floormove-direction-set! floor 1)
                         (floormove-sector-set! floor s2)
                         (floormove-speed-set! floor (fxquotient FLOORSPEED 2))
                         (floormove-texture-set! floor (sector-floorpic s3))
                         (floormove-newspecial-set! floor 0)
                         (floormove-floordestheight-set! floor (sector-floorheight s3)))
                       ;; Spawn lowering donut-hole
                       (let ([floor (make-floormove)])
                         (p-add-thinker floor)
                         (sector-specialdata-set! s1 floor)
                         (thinker-func-set! floor t-move-floor)
                         (floormove-type-set! floor lowerFloor)
                         (floormove-crush-set! floor #f)
                         (floormove-direction-set! floor -1)
                         (floormove-sector-set! floor s1)
                         (floormove-speed-set! floor (fxquotient FLOORSPEED 2))
                         (floormove-floordestheight-set! floor (sector-floorheight s3))))))))))))
    rtn))

;; SPECIAL SPAWNING

;; P_SpawnSpecials: after the map has been loaded, scan for specials that
;; spawn thinkers.
(define MAXLINEANIMS 64)
(define numlinespecials 0)
(define linespeciallist (make-vector MAXLINEANIMS #f))

(define (p-spawn-specials)
  ;; See if -TIMER needs to be used.
  (set! levelTimer #f)
  (when (and (fx> (m-check-parm "-avg") 0) (not (fx= deathmatch 0)))
    (set! levelTimer #t)
    (set! levelTimeCount (* 20 60 35)))
  (let ([i (m-check-parm "-timer")])
    (when (and (fx> i 0) (not (fx= deathmatch 0)))
      ;; (C reads myargv[i+1] even when it is past the end)
      (set! levelTimer #t)
      (set! levelTimeCount (fx* (if (fx< i (fx- myargc 1)) (m-atoi (vector-ref myargv (fx+ i 1))) 0)
                                (* 60 35)))))
  ;; Init special SECTORs.
  (do ([i 0 (fx+ i 1)]) ((fx= i numsectors))
    (let ([sector (vector-ref sectors i)])
      (case (sector-special sector)
        [(0) (void)]
        [(1) (p-spawn-light-flash sector)]                   ; FLICKERING LIGHTS
        [(2) (p-spawn-strobe-flash sector FASTDARK 0)]       ; STROBE FAST
        [(3) (p-spawn-strobe-flash sector SLOWDARK 0)]       ; STROBE SLOW
        [(4)                                                 ; STROBE FAST/DEATH SLIME
         (p-spawn-strobe-flash sector FASTDARK 0)
         (sector-special-set! sector 4)]
        [(8) (p-spawn-glowing-light sector)]                 ; GLOWING LIGHT
        [(9) (set! totalsecret (fx+ totalsecret 1))]         ; SECRET SECTOR
        [(10) (p-spawn-door-close-in30 sector)]              ; DOOR CLOSE IN 30 SECONDS
        [(12) (p-spawn-strobe-flash sector SLOWDARK 1)]      ; SYNC STROBE SLOW
        [(13) (p-spawn-strobe-flash sector FASTDARK 1)]      ; SYNC STROBE FAST
        [(14) (p-spawn-door-raise-in5-mins sector i)]        ; DOOR RAISE IN 5 MINUTES
        [(17) (p-spawn-fire-flicker sector)]
        [else (void)])))
  ;; Init line EFFECTs
  (set! numlinespecials 0)
  (vector-for-each
   (lambda (line)
     (when (fx= (line-special line) 48)
       ;; EFFECT FIRSTCOL SCROLL+ (C overruns the array past 64)
       (vector-set! linespeciallist numlinespecials line)
       (set! numlinespecials (fx+ numlinespecials 1))))
   lines)
  ;; Init other misc stuff
  (vector-fill! activeceilings #f)
  (vector-fill! activeplats #f)
  (vector-for-each (lambda (b) (struct-copy! b (make-button))) buttonlist))
