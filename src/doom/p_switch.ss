;;; p_switch.c: wall switches and buttons, and the use-line dispatch.
;;; Included by p_local.sls.

;; CHANGE THE TEXTURE OF A WALL SWITCH TO ITS OPPOSITE
;; (name1 name2 episode)
(define alphSwitchList
  '(;; Doom shareware episode 1 switches
    ("SW1BRCOM" "SW2BRCOM" 1) ("SW1BRN1" "SW2BRN1" 1) ("SW1BRN2" "SW2BRN2" 1)
    ("SW1BRNGN" "SW2BRNGN" 1) ("SW1BROWN" "SW2BROWN" 1) ("SW1COMM" "SW2COMM" 1)
    ("SW1COMP" "SW2COMP" 1) ("SW1DIRT" "SW2DIRT" 1) ("SW1EXIT" "SW2EXIT" 1)
    ("SW1GRAY" "SW2GRAY" 1) ("SW1GRAY1" "SW2GRAY1" 1) ("SW1METAL" "SW2METAL" 1)
    ("SW1PIPE" "SW2PIPE" 1) ("SW1SLAD" "SW2SLAD" 1) ("SW1STARG" "SW2STARG" 1)
    ("SW1STON1" "SW2STON1" 1) ("SW1STON2" "SW2STON2" 1) ("SW1STONE" "SW2STONE" 1)
    ("SW1STRTN" "SW2STRTN" 1)
    ;; Doom registered episodes 2&3 switches
    ("SW1BLUE" "SW2BLUE" 2) ("SW1CMT" "SW2CMT" 2) ("SW1GARG" "SW2GARG" 2)
    ("SW1GSTON" "SW2GSTON" 2) ("SW1HOT" "SW2HOT" 2) ("SW1LION" "SW2LION" 2)
    ("SW1SATYR" "SW2SATYR" 2) ("SW1SKIN" "SW2SKIN" 2) ("SW1VINE" "SW2VINE" 2)
    ("SW1WOOD" "SW2WOOD" 2)
    ;; Doom II switches
    ("SW1PANEL" "SW2PANEL" 3) ("SW1ROCK" "SW2ROCK" 3) ("SW1MET2" "SW2MET2" 3)
    ("SW1WDMET" "SW2WDMET" 3) ("SW1BRIK" "SW2BRIK" 3) ("SW1MOD1" "SW2MOD1" 3)
    ("SW1ZIM" "SW2ZIM" 3) ("SW1STON6" "SW2STON6" 3) ("SW1TEK" "SW2TEK" 3)
    ("SW1MARB" "SW2MARB" 3) ("SW1SKULL" "SW2SKULL" 3)))

;; Texture numbers in pairs: switchlist[i^1] is switchlist[i]'s opposite.
(define switchlist (make-fxvector (* MAXSWITCHES 2) 0))
(define numswitches 0)
(define buttonlist
  (let ([v (make-vector MAXBUTTONS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXBUTTONS) v)
      (vector-set! v i (make-button)))))

;; P_InitSwitchList: only called at game initialization.
(define (p-init-switch-list)
  (let ([episode (cond [(fx= gamemode registered) 2] [(fx= gamemode commercial) 3] [else 1])])
    (let loop ([l alphSwitchList] [index 0])
      (if (null? l)
          (begin
            (set! numswitches (fxquotient index 2))
            (fxvector-set! switchlist index -1))
          (apply
           (lambda (name1 name2 ep)
             (if (fx<= ep episode)
                 (begin
                   (fxvector-set! switchlist index (r-texture-num-for-name name1))
                   (fxvector-set! switchlist (fx+ index 1) (r-texture-num-for-name name2))
                   (loop (cdr l) (fx+ index 2)))
                 (loop (cdr l) index)))
           (car l))))))

;; Start a button counting down till it turns off.
(define (p-start-button line w texture time)
  ;; See if button is already pressed
  (unless (let loop ([i 0])
            (and (fx< i MAXBUTTONS)
                 (let ([b (vector-ref buttonlist i)])
                   (or (and (not (fx= (button-btimer b) 0)) (eq? (button-line b) line))
                       (loop (fx+ i 1))))))
    (let loop ([i 0])
      (if (fx= i MAXBUTTONS)
          (i-error "P_StartButton: no button slots left!")
          (let ([b (vector-ref buttonlist i)])
            (if (fx= (button-btimer b) 0)
                (begin
                  (button-line-set! b line)
                  (button-where-set! b w)
                  (button-btexture-set! b texture)
                  (button-btimer-set! b time)
                  (button-soundorg-set! b (sector-soundorg (line-frontsector line))))
                (loop (fx+ i 1))))))))

;; Function that changes wall texture. Tell it if switch is ok to use again
;; (1=yes, it's a button).
(define (p-change-switch-texture line use-again)
  (when (fx= use-again 0)
    (line-special-set! line 0))
  (let* ([side (vector-ref sides (fxvector-ref (line-sidenum line) 0))]
         [texTop (side-toptexture side)]
         [texMid (side-midtexture side)]
         [texBot (side-bottomtexture side)]
         ;; EXIT SWITCH?
         [sound (if (fx= (line-special line) 11) sfx_swtchx sfx_swtchn)]
         ;; (C plays it at buttonlist[0]'s origin, whatever line that was)
         [origin (button-soundorg (vector-ref buttonlist 0))])
    (let loop ([i 0])
      (when (fx< i (fx* numswitches 2))
        (let ([sw (fxvector-ref switchlist i)]
              [opposite (fxvector-ref switchlist (fxxor i 1))])
          (cond
            [(fx= sw texTop)
             (s-start-sound origin sound)
             (side-toptexture-set! side opposite)
             (unless (fx= use-again 0) (p-start-button line top sw BUTTONTIME))]
            [(fx= sw texMid)
             (s-start-sound origin sound)
             (side-midtexture-set! side opposite)
             (unless (fx= use-again 0) (p-start-button line middle sw BUTTONTIME))]
            [(fx= sw texBot)
             (s-start-sound origin sound)
             (side-bottomtexture-set! side opposite)
             (unless (fx= use-again 0) (p-start-button line bottom sw BUTTONTIME))]
            [else (loop (fx+ i 1))]))))))

;; P_UseSpecialLine: called when a thing uses a special line. Only the
;; front sides of lines are usable.
(define (p-use-special-line thing line side)
  (cond
    ;; Err... Use the back sides of VERY SPECIAL lines... (124: sliding door
    ;; open&close, unused)
    [(and (not (fx= side 0)) (not (fx= (line-special line) 124))) #f]
    ;; Switches that other things can activate: never open secret doors;
    ;; only MANUAL DOOR RAISE and the MANUAL BLUE / RED / YELLOW.
    [(and (not (mobj-player thing))
          (or (fxlogtest (line-flags line) ML_SECRET)
              (not (memv (line-special line) '(1 32 33 34)))))
     #f]
    [else
     ;; do something
     (let-syntax ([switch (syntax-rules ()
                            [(_ use-again e) (unless (fx= e 0) (p-change-switch-texture line use-again))])])
       (case (line-special line)
         ;; MANUALS: vertical / locked / blazing doors
         [(1 26 27 28 31 32 33 34 117 118) (ev-vertical-door line thing)]
         ;; SWITCHES
         [(7) (switch 0 (ev-build-stairs line build8))]             ; Build Stairs
         [(9) (switch 0 (ev-do-donut line))]                        ; Change Donut
         [(11)                                                      ; Exit level
          (p-change-switch-texture line 0)
          (g-exit-level)]
         [(14) (switch 0 (ev-do-plat line raiseAndChange 32))]      ; Raise Floor 32 and change texture
         [(15) (switch 0 (ev-do-plat line raiseAndChange 24))]      ; Raise Floor 24 and change texture
         [(18) (switch 0 (ev-do-floor line raiseFloorToNearest))]   ; Raise Floor to next highest floor
         [(20) (switch 0 (ev-do-plat line raiseToNearestAndChange 0))] ; Raise Plat next highest floor and change texture
         [(21) (switch 0 (ev-do-plat line downWaitUpStay 0))]       ; PlatDownWaitUpStay
         [(23) (switch 0 (ev-do-floor line lowerFloorToLowest))]    ; Lower Floor to Lowest
         [(29) (switch 0 (ev-do-door line normal))]                 ; Raise Door
         [(41) (switch 0 (ev-do-ceiling line lowerToFloor))]        ; Lower Ceiling to Floor
         [(71) (switch 0 (ev-do-floor line turboLower))]            ; Turbo Lower Floor
         [(49) (switch 0 (ev-do-ceiling line crushAndRaise))]       ; Ceiling Crush And Raise
         [(50) (switch 0 (ev-do-door line close))]                  ; Close Door
         [(51)                                                      ; Secret EXIT
          (p-change-switch-texture line 0)
          (g-secret-exit-level)]
         [(55) (switch 0 (ev-do-floor line raiseFloorCrush))]       ; Raise Floor Crush
         [(101) (switch 0 (ev-do-floor line raiseFloor))]           ; Raise Floor
         [(102) (switch 0 (ev-do-floor line lowerFloor))]           ; Lower Floor to Surrounding floor height
         [(103) (switch 0 (ev-do-door line open))]                  ; Open Door
         [(111) (switch 0 (ev-do-door line blazeRaise))]            ; Blazing Door Raise (faster than TURBO!)
         [(112) (switch 0 (ev-do-door line blazeOpen))]             ; Blazing Door Open (faster than TURBO!)
         [(113) (switch 0 (ev-do-door line blazeClose))]            ; Blazing Door Close (faster than TURBO!)
         [(122) (switch 0 (ev-do-plat line blazeDWUS 0))]           ; Blazing PlatDownWaitUpStay
         [(127) (switch 0 (ev-build-stairs line turbo16))]          ; Build Stairs Turbo 16
         [(131) (switch 0 (ev-do-floor line raiseFloorTurbo))]      ; Raise Floor Turbo
         [(133 135 137) (switch 0 (ev-do-locked-door line blazeOpen thing))] ; BlzOpenDoor BLUE / RED / YELLOW
         [(140) (switch 0 (ev-do-floor line raiseFloor512))]        ; Raise Floor 512
         ;; BUTTONS
         [(42) (switch 1 (ev-do-door line close))]                  ; Close Door
         [(43) (switch 1 (ev-do-ceiling line lowerToFloor))]        ; Lower Ceiling to Floor
         [(45) (switch 1 (ev-do-floor line lowerFloor))]            ; Lower Floor to Surrounding floor height
         [(60) (switch 1 (ev-do-floor line lowerFloorToLowest))]    ; Lower Floor to Lowest
         [(61) (switch 1 (ev-do-door line open))]                   ; Open Door
         [(62) (switch 1 (ev-do-plat line downWaitUpStay 1))]       ; PlatDownWaitUpStay
         [(63) (switch 1 (ev-do-door line normal))]                 ; Raise Door
         [(64) (switch 1 (ev-do-floor line raiseFloor))]            ; Raise Floor to ceiling
         [(66) (switch 1 (ev-do-plat line raiseAndChange 24))]      ; Raise Floor 24 and change texture
         [(67) (switch 1 (ev-do-plat line raiseAndChange 32))]      ; Raise Floor 32 and change texture
         [(65) (switch 1 (ev-do-floor line raiseFloorCrush))]       ; Raise Floor Crush
         [(68) (switch 1 (ev-do-plat line raiseToNearestAndChange 0))] ; Raise Plat to next highest floor and change texture
         [(69) (switch 1 (ev-do-floor line raiseFloorToNearest))]   ; Raise Floor to next highest floor
         [(70) (switch 1 (ev-do-floor line turboLower))]            ; Turbo Lower Floor
         [(114) (switch 1 (ev-do-door line blazeRaise))]            ; Blazing Door Raise (faster than TURBO!)
         [(115) (switch 1 (ev-do-door line blazeOpen))]             ; Blazing Door Open (faster than TURBO!)
         [(116) (switch 1 (ev-do-door line blazeClose))]            ; Blazing Door Close (faster than TURBO!)
         [(123) (switch 1 (ev-do-plat line blazeDWUS 0))]           ; Blazing PlatDownWaitUpStay
         [(132) (switch 1 (ev-do-floor line raiseFloorTurbo))]      ; Raise Floor Turbo
         [(99 134 136) (switch 1 (ev-do-locked-door line blazeOpen thing))] ; BlzOpenDoor BLUE / RED / YELLOW
         [(138)                                                     ; Light Turn On
          (ev-light-turn-on line 255)
          (p-change-switch-texture line 1)]
         [(139)                                                     ; Light Turn Off
          (ev-light-turn-on line 35)
          (p-change-switch-texture line 1)]
         [else (void)]))
     #t]))
