;;; st_stuff.c: the status bar: its widgets, the face, the palette
;;; flashes, and the cheat codes. Included by d_main.sls.

;; Palette indices. For damage/bonus red-/gold-shifts
(define STARTREDPALS 1)
(define STARTBONUSPALS 9)
(define NUMREDPALS 8)
(define NUMBONUSPALS 4)
;; Radiation suit, green shift.
(define RADIATIONPAL 13)

;; Location of status bar
(define ST_X 0)
(define ST_FX 143)

;; Number of status faces.
(define ST_NUMPAINFACES 5)
(define ST_NUMSTRAIGHTFACES 3)
(define ST_NUMTURNFACES 2)
(define ST_NUMSPECIALFACES 3)
(define ST_FACESTRIDE (+ ST_NUMSTRAIGHTFACES ST_NUMTURNFACES ST_NUMSPECIALFACES))
(define ST_NUMEXTRAFACES 2)
(define ST_NUMFACES (+ (* ST_FACESTRIDE ST_NUMPAINFACES) ST_NUMEXTRAFACES))
(define ST_TURNOFFSET ST_NUMSTRAIGHTFACES)
(define ST_OUCHOFFSET (+ ST_TURNOFFSET ST_NUMTURNFACES))
(define ST_EVILGRINOFFSET (+ ST_OUCHOFFSET 1))
(define ST_RAMPAGEOFFSET (+ ST_EVILGRINOFFSET 1))
(define ST_GODFACE (* ST_NUMPAINFACES ST_FACESTRIDE))
(define ST_DEADFACE (+ ST_GODFACE 1))
(define ST_FACESX 143)
(define ST_FACESY 168)
(define ST_EVILGRINCOUNT (* 2 TICRATE))
(define ST_STRAIGHTFACECOUNT (fxquotient TICRATE 2))
(define ST_TURNCOUNT (* 1 TICRATE))
(define ST_RAMPAGEDELAY (* 2 TICRATE))
(define ST_MUCHPAIN 20)

;; Location and size of statistics, justified according to widget type.
(define ST_AMMOWIDTH 3)
(define ST_AMMOX 44)
(define ST_AMMOY 171)
(define ST_HEALTHX 90)
(define ST_HEALTHY 171)
(define ST_ARMSX 111)
(define ST_ARMSY 172)
(define ST_ARMSBGX 104)
(define ST_ARMSBGY 168)
(define ST_ARMSXSPACE 12)
(define ST_ARMSYSPACE 10)
(define ST_FRAGSX 138)
(define ST_FRAGSY 171)
(define ST_FRAGSWIDTH 2)
(define ST_ARMORX 221)
(define ST_ARMORY 171)
;; key icon positions
(define ST_KEYX 239)
(define ST_KEYY (fxvector 171 181 191))
;; ammunition counters, and maximum ammunition (only needed because
;; backpack exists)
(define ST_AMMOCOUNTWIDTH 3)
(define ST_AMMOCOUNTX 288)
(define ST_MAXAMMOX 314)
(define ST_AMMOCOUNTY (fxvector 173 179 191 185))
(define ST_MSGWIDTH 52)

(define plyr #f)                        ; main player in game
(define st_firsttime #f)                ; ST_Start() has just been called
(define lu_palette 0)                   ; lump number for PLAYPAL
(define st_clock 0)                     ; used for timing
(define st_msgcounter 0)                ; used for making messages go away
(define st_gamestate 'first-person)     ; whether in automap or first-person
(define st_statusbaron #f)              ; whether left-side main status bar is active
(define st_chat #f)                     ; whether status bar chat is active
(define st_oldchat #f)                  ; value of st_chat before message popped up
(define st_cursoron #f)                 ; whether chat window has the cursor on
(define st_notdeathmatch #f)            ; !deathmatch
(define st_armson #f)                   ; !deathmatch && st_statusbaron
(define st_fragson #f)                  ; !deathmatch

(define sbar #f)                        ; main bar left
(define tallnum (make-vector 10 #f))    ; 0-9, tall numbers
(define tallpercent #f)                 ; tall % sign
(define shortnum (make-vector 10 #f))   ; 0-9, short, yellow (,different!) numbers
(define keys (make-vector NUMCARDS #f)) ; 3 key-cards, 3 skulls
(define faces (make-vector ST_NUMFACES #f)) ; face status patches
(define faceback #f)                    ; face background
(define armsbg #f)                      ; main bar right
(define arms (make-vector 6 #f))        ; weapon ownership patches (gray, yellow)

(define w_ready (make-st-number))       ; ready-weapon widget
(define w_frags (make-st-number))       ; in deathmatch only, summary of frags stats
(define w_health (make-st-percent))     ; health widget
(define w_armsbg (make-st-binicon))     ; arms background
(define w_arms                          ; weapon ownership widgets
  (let ([v (make-vector 6)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 6) v) (vector-set! v i (make-st-multicon)))))
(define w_faces (make-st-multicon))     ; face status widget
(define w_keyboxes                      ; keycard widgets
  (let ([v (make-vector 3)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 3) v) (vector-set! v i (make-st-multicon)))))
(define w_armor (make-st-percent))      ; armor widget
(define w_ammo                          ; ammo widgets
  (let ([v (make-vector 4)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 4) v) (vector-set! v i (make-st-number)))))
(define w_maxammo                       ; max ammo widgets
  (let ([v (make-vector 4)])
    (do ([i 0 (fx+ i 1)]) ((fx= i 4) v) (vector-set! v i (make-st-number)))))

(define st_fragscount 0)                ; number of frags so far in deathmatch
(define st_oldhealth -1)                ; used to use appopriately pained face
(define oldweaponsowned (make-vector NUMWEAPONS #f)) ; used for evil grin
(define st_facecount 0)                 ; count until face changes
(define st_faceindex 0)                 ; current face index, used by w_faces
(define keyboxes (make-fxvector 3 -1))  ; holds key-type for each key box on bar
(define st_randomnumber 0)              ; a random number per tick

;; plyr->ammo[i], where C's ammo[NUMAMMO..] runs on into maxammo[].
(define (player-ammo-ref p i)
  (if (fx< i NUMAMMO)
      (fxvector-ref (player-ammo p) i)
      (fxvector-ref (player-maxammo p) (fx- i NUMAMMO))))

;; Massive bunches of cheat shit to keep it from being easy to figure them
;; out. Yeah, right...
(define cheat_mus (make-cheatseq '(#xb2 #x26 #xb6 #xae #xea 1 0 0 #xff)))
(define cheat_choppers (make-cheatseq '(#xb2 #x26 #xe2 #x32 #xf6 #x2a #x2a #xa6 #x6a #xea #xff))) ; id...
(define cheat_god (make-cheatseq '(#xb2 #x26 #x26 #xaa #x26 #xff))) ; iddqd
(define cheat_ammo (make-cheatseq '(#xb2 #x26 #xf2 #x66 #xa2 #xff))) ; idkfa
(define cheat_ammonokey (make-cheatseq '(#xb2 #x26 #x66 #xa2 #xff))) ; idfa
;; Smashing Pumpkins Into Samml Piles Of Putried Debris.
(define cheat_noclip (make-cheatseq '(#xb2 #x26 #xea #x2a #xb2 #xea #x2a #xf6 #x2a #x26 #xff))) ; idspispopd
(define cheat_commercial_noclip (make-cheatseq '(#xb2 #x26 #xe2 #x36 #xb2 #x2a #xff))) ; idclip
(define cheat_powerup
  (vector (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #x6e #xff)) ; beholdv
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #xea #xff)) ; beholds
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #xb2 #xff)) ; beholdi
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #x6a #xff)) ; beholdr
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #xa2 #xff)) ; beholda
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #x36 #xff)) ; beholdl
          (make-cheatseq '(#xb2 #x26 #x62 #xa6 #x32 #xf6 #x36 #x26 #xff))))     ; behold
(define cheat_clev (make-cheatseq '(#xb2 #x26 #xe2 #x36 #xa6 #x6e 1 0 0 #xff))) ; idclev
;; my position cheat
(define cheat_mypos (make-cheatseq '(#xb2 #x26 #xb6 #xba #x2a #xf6 #xea #xff))) ; idmypos

;; STATUS BAR CODE

(define (st-refresh-background)
  (when st_statusbaron
    (v-draw-patch ST_X 0 BG sbar)
    (when netgame
      (v-draw-patch ST_FX 0 BG faceback))
    (v-copy-rect ST_X 0 BG ST_WIDTH ST_HEIGHT ST_X ST_Y FG)))

(define (hex32 n) (number->string (fxand n #xFFFFFFFF) 16))

;; Respond to keyboard input events, intercept cheats.
(define (st-responder ev)
  (let ([type (event-type ev)] [data1 (event-data1 ev)])
    (cond
      ;; Filter automap on/off.
      [(and (fx= type ev_keyup) (= (logand data1 #xffff0000) AM_MSGHEADER))
       (cond
         [(= data1 AM_MSGENTERED)
          (set! st_gamestate 'automap)
          (set! st_firsttime #t)]
         [(= data1 AM_MSGEXITED)
          (set! st_gamestate 'first-person)])]
      ;; if a user keypress...
      [(fx= type ev_keydown)
       (unless netgame
         ;; b. - enabled for more debug fun.
         (cond
           ;; 'dqd' cheat for toggleable god mode
           [(cht-check-cheat cheat_god data1)
            (player-cheats-set! plyr (fxxor (player-cheats plyr) CF_GODMODE))
            (if (fxlogtest (player-cheats plyr) CF_GODMODE)
                (begin
                  (when (player-mo plyr)
                    (mobj-health-set! (player-mo plyr) 100))
                  (player-health-set! plyr 100)
                  (player-message-set! plyr STSTR_DQDON))
                (player-message-set! plyr STSTR_DQDOFF))]
           ;; 'fa' cheat for killer fucking arsenal
           [(cht-check-cheat cheat_ammonokey data1)
            (player-armorpoints-set! plyr 200)
            (player-armortype-set! plyr 2)
            (vector-fill! (player-weaponowned plyr) #t)
            (do ([i 0 (fx+ i 1)]) ((fx= i NUMAMMO))
              (fxvector-set! (player-ammo plyr) i (fxvector-ref (player-maxammo plyr) i)))
            (player-message-set! plyr STSTR_FAADDED)]
           ;; 'kfa' cheat for key full ammo
           [(cht-check-cheat cheat_ammo data1)
            (player-armorpoints-set! plyr 200)
            (player-armortype-set! plyr 2)
            (vector-fill! (player-weaponowned plyr) #t)
            (do ([i 0 (fx+ i 1)]) ((fx= i NUMAMMO))
              (fxvector-set! (player-ammo plyr) i (fxvector-ref (player-maxammo plyr) i)))
            (vector-fill! (player-cards plyr) #t)
            (player-message-set! plyr STSTR_KFAADDED)]
           ;; 'mus' cheat for changing music
           [(cht-check-cheat cheat_mus data1)
            (player-message-set! plyr STSTR_MUS)
            (let* ([buf (cht-get-param cheat_mus)]
                   [b0 (car buf)]
                   [b1 (if (pair? (cdr buf)) (cadr buf) 0)])
              (if (fx= gamemode commercial)
                  (let ([n (fx+ (fx* (fx- b0 48) 10) (fx- b1 48))])
                    (if (fx> n 35)
                        (player-message-set! plyr STSTR_NOMUS)
                        (s-change-music (fx+ mus_runnin n -1) #t)))
                  (let ([n (fx+ (fx* (fx- b0 49) 9) (fx- b1 49))])
                    (if (fx> n 31)
                        (player-message-set! plyr STSTR_NOMUS)
                        (s-change-music (fx+ mus_e1m1 n) #t)))))]
           ;; Simplified, accepting both "noclip" and "idspispopd".
           ;; no clipping mode cheat
           [(or (cht-check-cheat cheat_noclip data1)
                (cht-check-cheat cheat_commercial_noclip data1))
            (player-cheats-set! plyr (fxxor (player-cheats plyr) CF_NOCLIP))
            (player-message-set! plyr (if (fxlogtest (player-cheats plyr) CF_NOCLIP)
                                          STSTR_NCON
                                          STSTR_NCOFF))])
         ;; 'behold?' power-up cheats
         (do ([i 0 (fx+ i 1)]) ((fx= i 6))
           (when (cht-check-cheat (vector-ref cheat_powerup i) data1)
             (cond
               [(fx= (fxvector-ref (player-powers plyr) i) 0) (p-give-power plyr i)]
               [(not (fx= i pw_strength)) (fxvector-set! (player-powers plyr) i 1)]
               [else (fxvector-set! (player-powers plyr) i 0)])
             (player-message-set! plyr STSTR_BEHOLDX)))
         (cond
           ;; 'behold' power-up menu
           [(cht-check-cheat (vector-ref cheat_powerup 6) data1)
            (player-message-set! plyr STSTR_BEHOLD)]
           ;; 'choppers' invulnerability & chainsaw
           [(cht-check-cheat cheat_choppers data1)
            (vector-set! (player-weaponowned plyr) wp_chainsaw #t)
            (fxvector-set! (player-powers plyr) pw_invulnerability 1)
            (player-message-set! plyr STSTR_CHOPPERS)]
           ;; 'mypos' for player position
           [(cht-check-cheat cheat_mypos data1)
            (let ([mo (player-mo (vector-ref players consoleplayer))])
              (player-message-set! plyr (format "ang=0x~a;x,y=(0x~a,0x~a)"
                                                (hex32 (mobj-angle mo)) (hex32 (mobj-x mo))
                                                (hex32 (mobj-y mo)))))]))
       ;; 'clev' change-level cheat
       (when (cht-check-cheat cheat_clev data1)
         (let* ([buf (cht-get-param cheat_clev)]
                [b0 (car buf)]
                [b1 (if (pair? (cdr buf)) (cadr buf) 0)]
                [epsd (if (fx= gamemode commercial) 0 (fx- b0 48))]
                [map (if (fx= gamemode commercial) (fx+ (fx* (fx- b0 48) 10) (fx- b1 48)) (fx- b1 48))])
           ;; Catch invalid maps.
           (unless (or (fx< epsd 1)
                       (fx< map 1)
                       ;; Ohmygod - this is not going to work.
                       (and (fx= gamemode retail) (or (fx> epsd 4) (fx> map 9)))
                       (and (fx= gamemode registered) (or (fx> epsd 3) (fx> map 9)))
                       (and (fx= gamemode shareware) (or (fx> epsd 1) (fx> map 9)))
                       (and (fx= gamemode commercial) (or (fx> epsd 1) (fx> map 34))))
             ;; So be it.
             (player-message-set! plyr STSTR_CLEV)
             (g-defered-init-new gameskill epsd map))))])
    #f))

(define st-lastcalc 0)
(define st-oldhealth-calc -1)
(define (st-calc-pain-offset)
  (let ([health (fxmin (player-health plyr) 100)])
    (unless (fx= health st-oldhealth-calc)
      (set! st-lastcalc (fx* ST_FACESTRIDE (fxquotient (fx* (fx- 100 health) ST_NUMPAINFACES) 101)))
      (set! st-oldhealth-calc health))
    st-lastcalc))

;; This is a not-very-pretty routine which handles the face states and
;; their timing. the precedence of expressions is:
;;  dead > evil grin > turned head > straight ahead
(define lastattackdown -1)
(define priority 0)

(define (st-update-face-widget)
  (when (fx< priority 10)
    ;; dead
    (when (fx= (player-health plyr) 0)
      (set! priority 9)
      (set! st_faceindex ST_DEADFACE)
      (set! st_facecount 1)))
  (when (fx< priority 9)
    (unless (fx= (player-bonuscount plyr) 0)
      ;; picking up bonus
      (let ([doevilgrin #f])
        (do ([i 0 (fx+ i 1)]) ((fx= i NUMWEAPONS))
          (unless (eq? (vector-ref oldweaponsowned i) (vector-ref (player-weaponowned plyr) i))
            (set! doevilgrin #t)
            (vector-set! oldweaponsowned i (vector-ref (player-weaponowned plyr) i))))
        (when doevilgrin
          ;; evil grin if just picked up weapon
          (set! priority 8)
          (set! st_facecount ST_EVILGRINCOUNT)
          (set! st_faceindex (fx+ (st-calc-pain-offset) ST_EVILGRINOFFSET))))))
  (when (fx< priority 8)
    (when (and (not (fx= (player-damagecount plyr) 0))
               (player-attacker plyr)
               (not (eq? (player-attacker plyr) (player-mo plyr))))
      ;; being attacked
      (set! priority 7)
      (if (fx> (fx- (player-health plyr) st_oldhealth) ST_MUCHPAIN)
          (begin
            (set! st_facecount ST_TURNCOUNT)
            (set! st_faceindex (fx+ (st-calc-pain-offset) ST_OUCHOFFSET)))
          (let* ([mo (player-mo plyr)]
                 [badguyangle (r-point-to-angle2 (mobj-x mo) (mobj-y mo)
                                                 (mobj-x (player-attacker plyr))
                                                 (mobj-y (player-attacker plyr)))]
                 [angle (mobj-angle mo)])
            (let-values ([(diffang i)
                          (if (> badguyangle angle)
                              ;; whether right or left
                              (let ([d (- badguyangle angle)]) (values d (> d ANG180)))
                              ;; whether left or right
                              (let ([d (- angle badguyangle)]) (values d (<= d ANG180))))])
              ;; confusing, aint it?
              (set! st_facecount ST_TURNCOUNT)
              (set! st_faceindex (st-calc-pain-offset))
              (cond
                ;; head-on
                [(< diffang ANG45) (set! st_faceindex (fx+ st_faceindex ST_RAMPAGEOFFSET))]
                ;; turn face right
                [i (set! st_faceindex (fx+ st_faceindex ST_TURNOFFSET))]
                ;; turn face left
                [else (set! st_faceindex (fx+ st_faceindex ST_TURNOFFSET 1))]))))))
  (when (fx< priority 7)
    ;; getting hurt because of your own damn stupidity
    (unless (fx= (player-damagecount plyr) 0)
      (if (fx> (fx- (player-health plyr) st_oldhealth) ST_MUCHPAIN)
          (begin
            (set! priority 7)
            (set! st_facecount ST_TURNCOUNT)
            (set! st_faceindex (fx+ (st-calc-pain-offset) ST_OUCHOFFSET)))
          (begin
            (set! priority 6)
            (set! st_facecount ST_TURNCOUNT)
            (set! st_faceindex (fx+ (st-calc-pain-offset) ST_RAMPAGEOFFSET))))))
  (when (fx< priority 6)
    ;; rapid firing
    (if (player-attackdown plyr)
        (cond
          [(fx= lastattackdown -1)
           (set! lastattackdown ST_RAMPAGEDELAY)]
          [else
           (set! lastattackdown (fx- lastattackdown 1))
           (when (fx= lastattackdown 0)
             (set! priority 5)
             (set! st_faceindex (fx+ (st-calc-pain-offset) ST_RAMPAGEOFFSET))
             (set! st_facecount 1)
             (set! lastattackdown 1))])
        (set! lastattackdown -1)))
  (when (fx< priority 5)
    ;; invulnerability
    (when (or (fxlogtest (player-cheats plyr) CF_GODMODE)
              (not (fx= (fxvector-ref (player-powers plyr) pw_invulnerability) 0)))
      (set! priority 4)
      (set! st_faceindex ST_GODFACE)
      (set! st_facecount 1)))
  ;; look left or look right if the facecount has timed out
  (when (fx= st_facecount 0)
    (set! st_faceindex (fx+ (st-calc-pain-offset) (fxremainder st_randomnumber 3)))
    (set! st_facecount ST_STRAIGHTFACECOUNT)
    (set! priority 0))
  (set! st_facecount (fx- st_facecount 1)))

(define largeammo 1994)                 ; means "n/a"

(define (st-update-widgets)
  ;; must redirect the pointer if the ready weapon has changed.
  (let ([ammo (weaponinfo-ammo (vector-ref weaponinfo-table (player-readyweapon plyr)))])
    (st-number-num-set! w_ready
                        (if (fx= ammo am_noammo)
                            (lambda () largeammo)
                            (let ([p plyr]) (lambda () (player-ammo-ref p ammo))))))
  (st-number-data-set! w_ready (player-readyweapon plyr))
  ;; update keycard multiple widgets
  (do ([i 0 (fx+ i 1)]) ((fx= i 3))
    (fxvector-set! keyboxes i (if (vector-ref (player-cards plyr) i) i -1))
    (when (vector-ref (player-cards plyr) (fx+ i 3))
      (fxvector-set! keyboxes i (fx+ i 3))))
  ;; refresh everything if this is him coming back to life
  (st-update-face-widget)
  ;; used by the w_armsbg widget
  (set! st_notdeathmatch (fx= deathmatch 0))
  ;; used by w_arms[] widgets
  (set! st_armson (and st_statusbaron (fx= deathmatch 0)))
  ;; used by w_frags widget
  (set! st_fragson (and (not (fx= deathmatch 0)) st_statusbaron))
  (set! st_fragscount
        (let loop ([i 0] [n 0])
          (if (fx= i MAXPLAYERS)
              n
              (loop (fx+ i 1)
                    (if (fx= i consoleplayer)
                        (fx- n (fxvector-ref (player-frags plyr) i))
                        (fx+ n (fxvector-ref (player-frags plyr) i)))))))
  ;; get rid of chat window if up because of message
  (set! st_msgcounter (fx- st_msgcounter 1))
  (when (fx= st_msgcounter 0)
    (set! st_chat st_oldchat)))

(define (st-ticker)
  (set! st_clock (fx+ st_clock 1))
  (set! st_randomnumber (m-random))
  (st-update-widgets)
  (set! st_oldhealth (player-health plyr)))

(define st_palette 0)

(define (st-do-palette-stuff)
  (let* ([cnt (player-damagecount plyr)]
         [strength (fxvector-ref (player-powers plyr) pw_strength)]
         [cnt (if (fx= strength 0)
                  cnt
                  ;; slowly fade the berzerk out
                  (fxmax cnt (fx- 12 (fxsra strength 6))))]
         [ironfeet (fxvector-ref (player-powers plyr) pw_ironfeet)]
         [palette (cond
                    [(not (fx= cnt 0))
                     (fx+ (fxmin (fxsra (fx+ cnt 7) 3) (fx- NUMREDPALS 1)) STARTREDPALS)]
                    [(not (fx= (player-bonuscount plyr) 0))
                     (fx+ (fxmin (fxsra (fx+ (player-bonuscount plyr) 7) 3) (fx- NUMBONUSPALS 1))
                          STARTBONUSPALS)]
                    [(or (fx> ironfeet (* 4 32)) (fxlogtest ironfeet 8)) RADIATIONPAL]
                    [else 0])])
    (unless (fx= palette st_palette)
      (set! st_palette palette)
      (i-set-palette (w-cache-lump-num lu_palette) (fx* palette 768)))))

(define (st-draw-widgets refresh)
  ;; used by w_arms[] widgets
  (set! st_armson (and st_statusbaron (fx= deathmatch 0)))
  ;; used by w_frags widget
  (set! st_fragson (and (not (fx= deathmatch 0)) st_statusbaron))
  (stlib-update-num w_ready refresh)
  (do ([i 0 (fx+ i 1)]) ((fx= i 4))
    (stlib-update-num (vector-ref w_ammo i) refresh)
    (stlib-update-num (vector-ref w_maxammo i) refresh))
  (stlib-update-percent w_health refresh)
  (stlib-update-percent w_armor refresh)
  (stlib-update-bin-icon w_armsbg refresh)
  (do ([i 0 (fx+ i 1)]) ((fx= i 6))
    (stlib-update-mult-icon (vector-ref w_arms i) refresh))
  (stlib-update-mult-icon w_faces refresh)
  (do ([i 0 (fx+ i 1)]) ((fx= i 3))
    (stlib-update-mult-icon (vector-ref w_keyboxes i) refresh))
  (stlib-update-num w_frags refresh))

(define (st-do-refresh)
  (set! st_firsttime #f)
  ;; draw status bar background to off-screen buff
  (st-refresh-background)
  ;; and refresh all widgets
  (st-draw-widgets #t))

(define (st-diff-draw)
  ;; update all widgets
  (st-draw-widgets #f))

(define (st-drawer fullscreen refresh)
  (set! st_statusbaron (or (not fullscreen) automapactive))
  (set! st_firsttime (or st_firsttime refresh))
  ;; Do red-/gold-shifts from damage/items
  (st-do-palette-stuff)
  ;; If just after ST_Start(), refresh all
  (if st_firsttime
      (st-do-refresh)
      ;; Otherwise, update as little as possible
      (st-diff-draw)))

(define (st-load-graphics)
  ;; Load the numbers, tall and short
  (do ([i 0 (fx+ i 1)]) ((fx= i 10))
    (vector-set! tallnum i (w-cache-lump-name (format "STTNUM~a" i)))
    (vector-set! shortnum i (w-cache-lump-name (format "STYSNUM~a" i))))
  ;; Load percent key.
  (set! tallpercent (w-cache-lump-name "STTPRCNT"))
  ;; key cards
  (do ([i 0 (fx+ i 1)]) ((fx= i NUMCARDS))
    (vector-set! keys i (w-cache-lump-name (format "STKEYS~a" i))))
  ;; arms background
  (set! armsbg (w-cache-lump-name "STARMS"))
  ;; arms ownership widgets: gray #, yellow #
  (do ([i 0 (fx+ i 1)]) ((fx= i 6))
    (vector-set! arms i (vector (w-cache-lump-name (format "STGNUM~a" (fx+ i 2)))
                                (vector-ref shortnum (fx+ i 2)))))
  ;; face backgrounds for different color players
  (set! faceback (w-cache-lump-name (format "STFB~a" consoleplayer)))
  ;; status bar background bits
  (set! sbar (w-cache-lump-name "STBAR"))
  ;; face states
  (let ([facenum 0])
    (define (face! name)
      (vector-set! faces facenum (w-cache-lump-name name))
      (set! facenum (fx+ facenum 1)))
    (do ([i 0 (fx+ i 1)]) ((fx= i ST_NUMPAINFACES))
      (do ([j 0 (fx+ j 1)]) ((fx= j ST_NUMSTRAIGHTFACES))
        (face! (format "STFST~a~a" i j)))
      (face! (format "STFTR~a0" i))     ; turn right
      (face! (format "STFTL~a0" i))     ; turn left
      (face! (format "STFOUCH~a" i))    ; ouch!
      (face! (format "STFEVL~a" i))     ; evil grin ;)
      (face! (format "STFKILL~a" i)))   ; pissed off
    (face! "STFGOD0")
    (face! "STFDEAD0")))

(define (st-load-data)
  (set! lu_palette (w-get-num-for-name "PLAYPAL"))
  (st-load-graphics))

(define (st-init-data)
  (set! st_firsttime #t)
  (set! plyr (vector-ref players consoleplayer))
  (set! st_clock 0)
  (set! st_gamestate 'first-person)
  (set! st_statusbaron #t)
  (set! st_chat #f)
  (set! st_oldchat #f)
  (set! st_cursoron #f)
  (set! st_faceindex 0)
  (set! st_palette -1)
  (set! st_oldhealth -1)
  (do ([i 0 (fx+ i 1)]) ((fx= i NUMWEAPONS))
    (vector-set! oldweaponsowned i (vector-ref (player-weaponowned plyr) i)))
  (fxvector-fill! keyboxes -1)
  (stlib-init))

(define (st-create-widgets)
  (let ([p plyr])
    (define (on) st_statusbaron)
    ;; ready weapon ammo
    (let ([ammo (weaponinfo-ammo (vector-ref weaponinfo-table (player-readyweapon p)))])
      (stlib-init-num w_ready ST_AMMOX ST_AMMOY tallnum (lambda () (player-ammo-ref p ammo)) on
                      ST_AMMOWIDTH))
    ;; the last weapon type
    (st-number-data-set! w_ready (player-readyweapon p))
    ;; health percentage
    (stlib-init-percent w_health ST_HEALTHX ST_HEALTHY tallnum (lambda () (player-health p)) on
                        tallpercent)
    ;; arms background
    (stlib-init-bin-icon w_armsbg ST_ARMSBGX ST_ARMSBGY armsbg (lambda () st_notdeathmatch) on)
    ;; weapons owned
    (do ([i 0 (fx+ i 1)]) ((fx= i 6))
      (let ([i i])
        (stlib-init-mult-icon (vector-ref w_arms i)
                              (fx+ ST_ARMSX (fx* (fxremainder i 3) ST_ARMSXSPACE))
                              (fx+ ST_ARMSY (fx* (fxquotient i 3) ST_ARMSYSPACE))
                              (vector-ref arms i)
                              (lambda () (if (vector-ref (player-weaponowned p) (fx+ i 1)) 1 0))
                              (lambda () st_armson))))
    ;; frags sum
    (stlib-init-num w_frags ST_FRAGSX ST_FRAGSY tallnum (lambda () st_fragscount)
                    (lambda () st_fragson) ST_FRAGSWIDTH)
    ;; faces
    (stlib-init-mult-icon w_faces ST_FACESX ST_FACESY faces (lambda () st_faceindex) on)
    ;; armor percentage - should be colored later
    (stlib-init-percent w_armor ST_ARMORX ST_ARMORY tallnum (lambda () (player-armorpoints p)) on
                        tallpercent)
    ;; keyboxes 0-2
    (do ([i 0 (fx+ i 1)]) ((fx= i 3))
      (let ([i i])
        (stlib-init-mult-icon (vector-ref w_keyboxes i) ST_KEYX (fxvector-ref ST_KEYY i) keys
                              (lambda () (fxvector-ref keyboxes i)) on)))
    ;; ammo count (all four kinds), max ammo count (all four kinds)
    (do ([i 0 (fx+ i 1)]) ((fx= i 4))
      (let ([i i])
        (stlib-init-num (vector-ref w_ammo i) ST_AMMOCOUNTX (fxvector-ref ST_AMMOCOUNTY i) shortnum
                        (lambda () (fxvector-ref (player-ammo p) i)) on ST_AMMOCOUNTWIDTH)))
    (do ([i 0 (fx+ i 1)]) ((fx= i 4))
      (let ([i i])
        (stlib-init-num (vector-ref w_maxammo i) ST_MAXAMMOX (fxvector-ref ST_AMMOCOUNTY i) shortnum
                        (lambda () (fxvector-ref (player-maxammo p) i)) on ST_AMMOCOUNTWIDTH)))))

(define st_stopped #t)

(define (st-start)
  (unless st_stopped
    (st-stop))
  (st-init-data)
  (st-create-widgets)
  (set! st_stopped #f))

(define (st-stop)
  (unless st_stopped
    (i-set-palette (w-cache-lump-num lu_palette))
    (set! st_stopped #t)))

(define (st-init)
  (st-load-data))
