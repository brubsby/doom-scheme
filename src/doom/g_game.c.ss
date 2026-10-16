;;; g_game.c: game flow: input to ticcmds, new games, level loading and
;;; completion, demo recording and playback, and the per-tic game driver.
;;; Included by d_main.sls.
;;;
;;; Saving and loading (p_saveg, milestone 9) are logged as unported.

(define SAVEGAMESIZE #x2c000)
(define SAVESTRINGSIZE 24)

;; (gameaction, gamestate, gameskill, ... live in doomstat)
(define starttime 0)                    ; for comparative timing purposes

(define demoend 0)

(define consistancy
  (let ([v (make-vector MAXPLAYERS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS) v)
      (vector-set! v i (make-fxvector BACKUPTICS 0)))))

;; controls (have defaults; see m_misc)

(define TURBOTHRESHOLD #x32)
(define forwardmove (fxvector #x19 #x32))
(define sidemove (fxvector #x18 #x28))
(define angleturn (fxvector 640 1280 320)) ; + slow turn
(define (MAXPLMOVE) (fxvector-ref forwardmove 1))
(define SLOWTURNTICS 6)

(define NUMKEYS 256)
(define gamekeydown (make-vector NUMKEYS #f))
(define turnheld 0)                     ; for accelerative turning

;; mousebuttons[i] is mousearray[i+1] (C allows [-1]); same for joybuttons.
(define mousearray (make-vector 4 #f))
(define-syntax mousebuttons (syntax-rules () [(_ i) (vector-ref mousearray (fx+ i 1))]))
(define (mousebuttons-set! i v) (vector-set! mousearray (fx+ i 1) v))
(define joyarray (make-vector 5 #f))
(define-syntax joybuttons (syntax-rules () [(_ i) (vector-ref joyarray (fx+ i 1))]))
(define (joybuttons-set! i v) (vector-set! joyarray (fx+ i 1) v))

;; mouse values are used once
(define dclicktime 0)
(define dclickstate #f)
(define dclicks 0)
(define dclicktime2 0)
(define dclickstate2 #f)
(define dclicks2 0)

;; joystick values are repeated


(define BODYQUESIZE 32)
(define bodyque (make-vector BODYQUESIZE #f))

;; ticcmd fields as C stores them: signed char moves, short angleturn,
;; byte buttons.
(define (s8 x) (fx- (fxand (fx+ x #x80) #xFF) #x80))
(define (s16 x) (fx- (fxand (fx+ x #x8000) #xFFFF) #x8000))

;; G_BuildTiccmd: builds a ticcmd from all of the available inputs.
(define (g-build-ticcmd cmd)
  ;; base: I_BaseTiccmd, empty (no external driver)
  (struct-copy! cmd (make-ticcmd))
  (ticcmd-consistancy-set! cmd (fxvector-ref (vector-ref consistancy consoleplayer)
                                             (fxremainder maketic BACKUPTICS)))
  (let* ([key (lambda (k) (vector-ref gamekeydown k))]
         [strafe (or (key key_strafe) (mousebuttons mousebstrafe) (joybuttons joybstrafe))]
         [speed (if (or (key key_speed) (joybuttons joybspeed)) 1 0)]
         [forward 0]
         [side 0]
         [turn 0]
         [buttons 0])
    ;; use two stage accelerative turning on the keyboard and joystick
    (if (or (not (fx= joyxmove 0)) (key key_right) (key key_left))
        (set! turnheld (fx+ turnheld ticdup))
        (set! turnheld 0))
    (let ([tspeed (if (fx< turnheld SLOWTURNTICS) 2 speed)]) ; slow turn
      ;; let movement keys cancel each other out
      (if strafe
          (begin
            (when (key key_right) (set! side (fx+ side (fxvector-ref sidemove speed))))
            (when (key key_left) (set! side (fx- side (fxvector-ref sidemove speed))))
            (when (fx> joyxmove 0) (set! side (fx+ side (fxvector-ref sidemove speed))))
            (when (fx< joyxmove 0) (set! side (fx- side (fxvector-ref sidemove speed)))))
          (begin
            (when (key key_right) (set! turn (fx- turn (fxvector-ref angleturn tspeed))))
            (when (key key_left) (set! turn (fx+ turn (fxvector-ref angleturn tspeed))))
            (when (fx> joyxmove 0) (set! turn (fx- turn (fxvector-ref angleturn tspeed))))
            (when (fx< joyxmove 0) (set! turn (fx+ turn (fxvector-ref angleturn tspeed)))))))
    (when (key key_up) (set! forward (fx+ forward (fxvector-ref forwardmove speed))))
    (when (key key_down) (set! forward (fx- forward (fxvector-ref forwardmove speed))))
    (when (fx< joyymove 0) (set! forward (fx+ forward (fxvector-ref forwardmove speed))))
    (when (fx> joyymove 0) (set! forward (fx- forward (fxvector-ref forwardmove speed))))
    (when (key key_straferight) (set! side (fx+ side (fxvector-ref sidemove speed))))
    (when (key key_strafeleft) (set! side (fx- side (fxvector-ref sidemove speed))))
    ;; buttons
    (ticcmd-chatchar-set! cmd (hu-dequeue-chat-char))
    (when (or (key key_fire) (mousebuttons mousebfire) (joybuttons joybfire))
      (set! buttons (fxior buttons BT_ATTACK)))
    (when (or (key key_use) (joybuttons joybuse))
      (set! buttons (fxior buttons BT_USE))
      ;; clear double clicks if hit use button
      (set! dclicks 0))
    ;; chainsaw overrides
    (let loop ([i 0])
      (when (fx< i (fx- NUMWEAPONS 1))
        (if (key (fx+ (char->integer #\1) i))
            (set! buttons (fxior buttons BT_CHANGE (fxsll i BT_WEAPONSHIFT)))
            (loop (fx+ i 1)))))
    ;; mouse
    (when (mousebuttons mousebforward)
      (set! forward (fx+ forward (fxvector-ref forwardmove speed))))
    ;; forward double click
    (if (and (not (eq? (mousebuttons mousebforward) dclickstate)) (fx> dclicktime 1))
        (begin
          (set! dclickstate (mousebuttons mousebforward))
          (when dclickstate (set! dclicks (fx+ dclicks 1)))
          (if (fx= dclicks 2)
              (begin (set! buttons (fxior buttons BT_USE))
                     (set! dclicks 0))
              (set! dclicktime 0)))
        (begin
          (set! dclicktime (fx+ dclicktime ticdup))
          (when (fx> dclicktime 20)
            (set! dclicks 0)
            (set! dclickstate #f))))
    ;; strafe double click
    (let ([bstrafe (or (mousebuttons mousebstrafe) (joybuttons joybstrafe))])
      (if (and (not (eq? bstrafe dclickstate2)) (fx> dclicktime2 1))
          (begin
            (set! dclickstate2 bstrafe)
            (when dclickstate2 (set! dclicks2 (fx+ dclicks2 1)))
            (if (fx= dclicks2 2)
                (begin (set! buttons (fxior buttons BT_USE))
                       (set! dclicks2 0))
                (set! dclicktime2 0)))
          (begin
            (set! dclicktime2 (fx+ dclicktime2 ticdup))
            (when (fx> dclicktime2 20)
              (set! dclicks2 0)
              (set! dclickstate2 #f)))))
    (set! forward (fx+ forward mousey))
    (if strafe
        (set! side (fx+ side (fx* mousex 2)))
        (set! turn (fx- turn (fx* mousex 8))))
    (set! mousex 0)
    (set! mousey 0)
    (set! forward (fxmax (fxmin forward (MAXPLMOVE)) (fx- 0 (MAXPLMOVE))))
    (set! side (fxmax (fxmin side (MAXPLMOVE)) (fx- 0 (MAXPLMOVE))))
    (ticcmd-forwardmove-set! cmd (s8 (fx+ (ticcmd-forwardmove cmd) forward)))
    (ticcmd-sidemove-set! cmd (s8 (fx+ (ticcmd-sidemove cmd) side)))
    (ticcmd-angleturn-set! cmd (s16 (fx+ (ticcmd-angleturn cmd) turn)))
    (ticcmd-buttons-set! cmd (fxand buttons #xFF))
    ;; special buttons
    (when sendpause
      (set! sendpause #f)
      (ticcmd-buttons-set! cmd (fxior BT_SPECIAL BTS_PAUSE)))
    (when sendsave
      (set! sendsave #f)
      (ticcmd-buttons-set! cmd (fxand (fxior BT_SPECIAL BTS_SAVEGAME (fxsll savegameslot BTS_SAVESHIFT))
                                      #xFF)))))

;; G_DoLoadLevel
(define (g-do-load-level)
  ;; Set the sky map. First thing, we have a dummy sky texture name, a
  ;; flat. The data is in the WAD only because we look for an actual index,
  ;; instead of simply setting one.
  (set! skyflatnum (r-flat-num-for-name SKYFLATNAME))
  ;; DOOM determines the sky texture to be used depending on the current
  ;; episode, and the game version.
  (when (fx= gamemode commercial)
    (set! skytexture (r-texture-num-for-name
                      (cond [(fx< gamemap 12) "SKY1"] [(fx< gamemap 21) "SKY2"] [else "SKY3"]))))
  (set! levelstarttic gametic)          ; for time calculation
  (when (fx= wipegamestate GS_LEVEL)
    (set! wipegamestate -1))            ; force a wipe
  (set! gamestate GS_LEVEL)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (let ([p (vector-ref players i)])
      (when (and (vector-ref playeringame i) (fx= (player-playerstate p) PST_DEAD))
        (player-playerstate-set! p PST_REBORN))
      (fxvector-fill! (player-frags p) 0)))
  (p-setup-level gameepisode gamemap 0 gameskill)
  (set! displayplayer consoleplayer)    ; view the guy you are playing
  (set! starttime (i-get-time))
  (set! gameaction ga_nothing)
  ;; clear cmd building stuff
  (vector-fill! gamekeydown #f)
  (set! joyxmove 0)
  (set! joyymove 0)
  (set! mousex 0)
  (set! mousey 0)
  (set! sendpause #f)
  (set! sendsave #f)
  (set! paused #f)
  (vector-fill! mousearray #f)
  (vector-fill! joyarray #f))

;; G_Responder: get info needed to make ticcmd_ts for the players.
(define (g-responder ev)
  (let ([type (event-type ev)] [data1 (event-data1 ev)])
    (cond
      ;; allow spy mode changes even during the demo
      [(and (fx= gamestate GS_LEVEL) (fx= type ev_keydown) (fx= data1 KEY_F12)
            (or singledemo (fx= deathmatch 0)))
       ;; spy mode
       (let loop ()
         (set! displayplayer (fx+ displayplayer 1))
         (when (fx= displayplayer MAXPLAYERS) (set! displayplayer 0))
         (unless (or (vector-ref playeringame displayplayer) (fx= displayplayer consoleplayer))
           (loop)))
       #t]
      ;; any other key pops up menu if in demos
      [(and (fx= gameaction ga_nothing) (not singledemo)
            (or demoplayback (fx= gamestate GS_DEMOSCREEN)))
       (if (or (fx= type ev_keydown)
               (and (fx= type ev_mouse) (not (fx= data1 0)))
               (and (fx= type ev_joystick) (not (fx= data1 0))))
           (begin (m-start-control-panel) #t)
           #f)]
      [(and (fx= gamestate GS_LEVEL)
            (or (hu-responder ev)         ; chat ate the event
                (st-responder ev)         ; status window ate it
                (am-responder ev)))       ; automap ate it
       #t]
      [(and (fx= gamestate GS_FINALE) (f-responder ev)) #t] ; finale ate the event
      [(fx= type ev_keydown)
       (if (fx= data1 KEY_PAUSE)
           (set! sendpause #t)
           (when (fx< data1 NUMKEYS)
             (vector-set! gamekeydown data1 #t)))
       #t]                              ; eat key down events
      [(fx= type ev_keyup)
       (when (fx< data1 NUMKEYS)
         (vector-set! gamekeydown data1 #f))
       #f]                              ; always let key up events filter down
      [(fx= type ev_mouse)
       (mousebuttons-set! 0 (fxlogtest data1 1))
       (mousebuttons-set! 1 (fxlogtest data1 2))
       (mousebuttons-set! 2 (fxlogtest data1 4))
       (set! mousex (fxquotient (fx* (event-data2 ev) (fx+ mouseSensitivity 5)) 10))
       (set! mousey (fxquotient (fx* (event-data3 ev) (fx+ mouseSensitivity 5)) 10))
       #t]                              ; eat events
      [(fx= type ev_joystick)
       (joybuttons-set! 0 (fxlogtest data1 1))
       (joybuttons-set! 1 (fxlogtest data1 2))
       (joybuttons-set! 2 (fxlogtest data1 4))
       (joybuttons-set! 3 (fxlogtest data1 8))
       (set! joyxmove (event-data2 ev))
       (set! joyymove (event-data3 ev))
       #t]                              ; eat events
      [else #f])))

(define player_names
  (vector HUSTR_PLRGREEN HUSTR_PLRINDIGO HUSTR_PLRBROWN HUSTR_PLRRED))

;; G_Ticker: make ticcmd_ts for the players.
(define (g-ticker)
  ;; do player reborns if needed
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (and (vector-ref playeringame i)
               (fx= (player-playerstate (vector-ref players i)) PST_REBORN))
      (g-do-reborn i)))
  ;; do things to change the game state
  (let loop ()
    (unless (fx= gameaction ga_nothing)
      (cond
        [(fx= gameaction ga_loadlevel) (g-do-load-level)]
        [(fx= gameaction ga_newgame) (g-do-new-game)]
        [(fx= gameaction ga_loadgame) (g-do-load-game)]
        [(fx= gameaction ga_savegame) (g-do-save-game)]
        [(fx= gameaction ga_playdemo) (g-do-play-demo)]
        [(fx= gameaction ga_completed) (g-do-completed)]
        [(fx= gameaction ga_victory) (f-start-finale)]
        [(fx= gameaction ga_worlddone) (g-do-world-done)]
        [(fx= gameaction ga_screenshot)
         (m-screen-shot)
         (set! gameaction ga_nothing)])
      (loop)))
  ;; get commands, check consistancy, and build new consistancy check
  (let ([buf (fxremainder (fxquotient gametic ticdup) BACKUPTICS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (let ([cmd (player-cmd (vector-ref players i))])
          (struct-copy! cmd (vector-ref (vector-ref netcmds i) buf))
          (when demoplayback
            (g-read-demo-ticcmd cmd))
          (when demorecording
            (g-write-demo-ticcmd cmd))
          ;; check for turbo cheats
          (when (and (fx> (ticcmd-forwardmove cmd) TURBOTHRESHOLD)
                     (fx= (fxand gametic 31) 0)
                     (fx= (fxand (fxsra gametic 5) 3) i))
            (player-message-set! (vector-ref players consoleplayer)
                                 (format "~a is turbo!" (vector-ref player_names i))))
          (when (and netgame (not netdemo) (fx= (fxremainder gametic ticdup) 0))
            (when (and (fx> gametic BACKUPTICS)
                       (not (fx= (fxvector-ref (vector-ref consistancy i) buf) (ticcmd-consistancy cmd))))
              (i-error "consistency failure (~a should be ~a)"
                       (ticcmd-consistancy cmd) (fxvector-ref (vector-ref consistancy i) buf)))
            (fxvector-set! (vector-ref consistancy i) buf
                           (s16 (let ([mo (player-mo (vector-ref players i))])
                                  (if mo (mobj-x mo) rndindex)))))))))
  ;; check for special buttons
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (let ([buttons (ticcmd-buttons (player-cmd (vector-ref players i)))])
        (when (fxlogtest buttons BT_SPECIAL)
          (cond
            [(fx= (fxand buttons BT_SPECIALMASK) BTS_PAUSE)
             (set! paused (not paused))
             (if paused (s-pause-sound) (s-resume-sound))]
            [(fx= (fxand buttons BT_SPECIALMASK) BTS_SAVEGAME)
             (when (string=? savedescription "")
               (set! savedescription "NET GAME"))
             (set! savegameslot (fxsra (fxand buttons BTS_SAVEMASK) BTS_SAVESHIFT))
             (set! gameaction ga_savegame)])))))
  ;; do main actions
  (cond
    [(fx= gamestate GS_LEVEL)
     (p-ticker)
     (st-ticker)
     (am-ticker)
     (hu-ticker)]
    [(fx= gamestate GS_INTERMISSION) (wi-ticker)]
    [(fx= gamestate GS_FINALE) (f-ticker)]
    [(fx= gamestate GS_DEMOSCREEN) (d-page-ticker)]))

;; PLAYER STRUCTURE FUNCTIONS
;; also see P_SpawnPlayer in P_Things

;; G_PlayerFinishLevel: can when a player completes a level.
(define (g-player-finish-level n)
  (let ([p (vector-ref players n)])
    (fxvector-fill! (player-powers p) 0)
    (vector-fill! (player-cards p) #f)
    (mobj-flags-set! (player-mo p) (fxand (mobj-flags (player-mo p)) (fxnot MF_SHADOW))) ; cancel invisibility
    (player-extralight-set! p 0)        ; cancel gun flashes
    (player-fixedcolormap-set! p 0)     ; cancel ir gogles
    (player-damagecount-set! p 0)       ; no palette changes
    (player-bonuscount-set! p 0)))

;; G_CheckSpot: returns false if the player cannot be respawned at the
;; given mapthing_t spot because something is occupying it.
(define (g-check-spot playernum mthing)
  (let ([x (int32 (fxsll (mapthing-x mthing) FRACBITS))]
        [y (int32 (fxsll (mapthing-y mthing) FRACBITS))]
        [pmo (player-mo (vector-ref players playernum))])
    (if (not pmo)
        ;; first spawn of level, before corpses
        (let loop ([i 0])
          (or (fx= i playernum)
              (let ([mo (player-mo (vector-ref players i))])
                (and (not (and (fx= (mobj-x mo) x) (fx= (mobj-y mo) y)))
                     (loop (fx+ i 1))))))
        (and (p-check-position pmo x y)
             (begin
               ;; flush an old corpse if needed
               (when (fx>= bodyqueslot BODYQUESIZE)
                 (p-remove-mobj (vector-ref bodyque (fxremainder bodyqueslot BODYQUESIZE))))
               (vector-set! bodyque (fxremainder bodyqueslot BODYQUESIZE) pmo)
               (set! bodyqueslot (fx+ bodyqueslot 1))
               ;; spawn a teleport fog
               (let* ([ss (r-point-in-subsector x y)]
                      [an (fxsrl (uint32 (* ANG45 (quotient (mapthing-angle mthing) 45))) ANGLETOFINESHIFT)]
                      [mo (p-spawn-mobj (int32 (+ x (* 20 (finecosine an))))
                                        (int32 (+ y (* 20 (fxvector-ref finesine an))))
                                        (sector-floorheight (subsector-sector ss)) MT_TFOG)])
                 (unless (fx= (player-viewz (vector-ref players consoleplayer)) 1)
                   (s-start-sound mo sfx_telept))) ; don't start sound on first frame
               #t)))))

;; G_DeathMatchSpawnPlayer: spawns a player at one of the random death
;; match spots, called at level load and each death.
(define (g-death-match-spawn-player playernum)
  (let ([selections deathmatch_p])
    (when (fx< selections 4)
      (i-error "Only ~a deathmatch spots, 4 required" selections))
    (let loop ([j 0])
      (if (fx= j 20)
          ;; no good spot, so the player will probably get stuck
          (p-spawn-player (vector-ref playerstarts playernum))
          (let* ([i (fxremainder (p-random) selections)]
                 [spot (vector-ref deathmatchstarts i)])
            (if (g-check-spot playernum spot)
                (begin
                  (mapthing-type-set! spot (fx+ playernum 1))
                  (p-spawn-player spot))
                (loop (fx+ j 1))))))))

;; G_DoReborn
(define (g-do-reborn playernum)
  (if (not netgame)
      ;; reload the level from scratch
      (set! gameaction ga_loadlevel)
      (begin
        ;; respawn at the start: first dissasociate the corpse
        (mobj-player-set! (player-mo (vector-ref players playernum)) #f)
        (cond
          ;; spawn at random spot if in death match
          [(not (fx= deathmatch 0)) (g-death-match-spawn-player playernum)]
          [(g-check-spot playernum (vector-ref playerstarts playernum))
           (p-spawn-player (vector-ref playerstarts playernum))]
          [else
           ;; try to spawn at one of the other players spots
           (let loop ([i 0])
             (if (fx= i MAXPLAYERS)
                 ;; he's going to be inside something. Too bad.
                 (p-spawn-player (vector-ref playerstarts playernum))
                 (let ([spot (vector-ref playerstarts i)])
                   (if (g-check-spot playernum spot)
                       (begin
                         (mapthing-type-set! spot (fx+ playernum 1)) ; fake as other player
                         (p-spawn-player spot)
                         (mapthing-type-set! spot (fx+ i 1)))       ; restore
                       (loop (fx+ i 1))))))]))))

(define (g-screen-shot)
  (set! gameaction ga_screenshot))

;; DOOM Par Times
(define pars
  (vector (fxvector 0 0 0 0 0 0 0 0 0 0)
          (fxvector 0 30 75 120 90 165 180 180 30 165)
          (fxvector 0 90 90 90 120 90 360 240 30 170)
          (fxvector 0 90 45 90 150 90 90 165 30 135)))

;; DOOM II Par Times
(define cpars
  (fxvector 30 90 120 120 90 150 120 120 270 90        ;  1-10
            210 150 150 150 210 150 420 150 210 150    ; 11-20
            240 150 180 150 150 300 330 420 300 180    ; 21-30
            120 30))                                    ; 31-32

;; G_DoCompleted
(define (g-do-completed)
  (set! gameaction ga_nothing)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (g-player-finish-level i)))       ; take away cards and stuff
  (when automapactive
    (am-stop))
  (cond
    [(and (not (fx= gamemode commercial)) (fx= gamemap 8))
     ;; victory
     (set! gameaction ga_victory)]
    [else
     (when (and (not (fx= gamemode commercial)) (fx= gamemap 9))
       ;; exit secret level
       (vector-for-each (lambda (p) (player-didsecret-set! p #t)) players))
     (wbstartstruct-didsecret-set! wminfo (player-didsecret (vector-ref players consoleplayer)))
     (wbstartstruct-epsd-set! wminfo (fx- gameepisode 1))
     (wbstartstruct-last-set! wminfo (fx- gamemap 1))
     ;; wminfo.next is 0 biased, unlike gamemap
     (if (fx= gamemode commercial)
         (if secretexit
             (case gamemap
               [(15) (wbstartstruct-next-set! wminfo 30)]
               [(31) (wbstartstruct-next-set! wminfo 31)])
             (case gamemap
               [(31 32) (wbstartstruct-next-set! wminfo 15)]
               [else (wbstartstruct-next-set! wminfo gamemap)]))
         (cond
           [secretexit (wbstartstruct-next-set! wminfo 8)] ; go to secret level
           [(fx= gamemap 9)
            ;; returning from secret level
            (case gameepisode
              [(1) (wbstartstruct-next-set! wminfo 3)]
              [(2) (wbstartstruct-next-set! wminfo 5)]
              [(3) (wbstartstruct-next-set! wminfo 6)]
              [(4) (wbstartstruct-next-set! wminfo 2)])]
           [else (wbstartstruct-next-set! wminfo gamemap)])) ; go to next level
     (wbstartstruct-maxkills-set! wminfo totalkills)
     (wbstartstruct-maxitems-set! wminfo totalitems)
     (wbstartstruct-maxsecret-set! wminfo totalsecret)
     (wbstartstruct-maxfrags-set! wminfo 0)
     (wbstartstruct-partime-set! wminfo
                                 (fx* 35 (if (fx= gamemode commercial)
                                             (fxvector-ref cpars (fx- gamemap 1))
                                             (fxvector-ref (vector-ref pars gameepisode) gamemap))))
     (wbstartstruct-pnum-set! wminfo consoleplayer)
     (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
       (let ([wp (vector-ref (wbstartstruct-plyr wminfo) i)] [p (vector-ref players i)])
         (wbplayerstruct-in-set! wp (vector-ref playeringame i))
         (wbplayerstruct-skills-set! wp (player-killcount p))
         (wbplayerstruct-sitems-set! wp (player-itemcount p))
         (wbplayerstruct-ssecret-set! wp (player-secretcount p))
         (wbplayerstruct-stime-set! wp leveltime)
         (wbplayerstruct-frags-set! wp (fxvector-copy (player-frags p)))))
     (set! gamestate GS_INTERMISSION)
     (set! viewactive #f)
     (set! automapactive #f)
     ;; (statcopy: the -statcopy statistics driver is not supported)
     (wi-start wminfo)]))

;; G_WorldDone
(define (g-world-done)
  (set! gameaction ga_worlddone)
  (when secretexit
    (player-didsecret-set! (vector-ref players consoleplayer) #t))
  (when (fx= gamemode commercial)
    (when (case gamemap
            [(15 31) secretexit]
            [(6 11 20 30) #t]
            [else #f])
      (f-start-finale))))

(define (g-do-world-done)
  (set! gamestate GS_LEVEL)
  (set! gamemap (fx+ (wbstartstruct-next wminfo) 1))
  (g-do-load-level)
  (set! gameaction ga_nothing)
  (set! viewactive #t))

;; G_InitFromSavegame / G_LoadGame / G_SaveGame.
;; TODO(milestone 9): p_saveg.
(define savename "")
(define (g-load-game name)
  (set! savename name)
  (set! gameaction ga_loadgame))

(define (g-do-load-game)
  (set! gameaction ga_nothing)
  (unported 'G_DoLoadGame))

;; G_SaveGame: called by the menu task. Description is a 24 byte text
;; string.
(define (g-save-game slot description)
  (set! savegameslot slot)
  (set! savedescription description)
  (set! sendsave #t))

(define (g-do-save-game)
  (set! gameaction ga_nothing)
  (set! savedescription "")
  (unported 'G_DoSaveGame))

;; G_InitNew: can be called by the startup code or the menu task;
;; consoleplayer, displayplayer, playeringame[] should be set.
(define d-skill 0)
(define d-episode 0)
(define d-map 0)

(define (g-defered-init-new skill episode map)
  (set! d-skill skill)
  (set! d-episode episode)
  (set! d-map map)
  (set! gameaction ga_newgame))

(define (g-do-new-game)
  (set! demoplayback #f)
  (set! netdemo #f)
  (set! netgame #f)
  (set! deathmatch 0)
  (vector-set! playeringame 1 #f)
  (vector-set! playeringame 2 #f)
  (vector-set! playeringame 3 #f)
  (set! respawnparm #f)
  (set! fastparm #f)
  (set! nomonsters #f)
  (set! consoleplayer 0)
  (g-init-new d-skill d-episode d-map)
  (set! gameaction ga_nothing))

(define (g-init-new skill episode map)
  (when paused
    (set! paused #f)
    (s-resume-sound))
  (let* ([skill (if (fx> skill sk_nightmare) sk_nightmare skill)]
         ;; This was quite messy with SPECIAL and commented parts.
         ;; Supposedly hacks to make the latest edition work. It might not
         ;; work properly.
         [episode (if (fx< episode 1) 1 episode)]
         [episode (cond [(fx= gamemode retail) (fxmin episode 4)]
                        [(fx= gamemode shareware) (fxmin episode 1)] ; only episode 1 on shareware
                        [else (fxmin episode 3)])]
         [map (if (fx< map 1) 1 map)]
         [map (if (and (fx> map 9) (not (fx= gamemode commercial))) 9 map)])
    (m-clear-random)
    (set! respawnmonsters (or (fx= skill sk_nightmare) respawnparm))
    (cond
      [(or fastparm (and (fx= skill sk_nightmare) (not (fx= gameskill sk_nightmare))))
       (do ([i S_SARG_RUN1 (fx+ i 1)]) ((fx> i S_SARG_PAIN2))
         (let ([st (vector-ref states i)])
           (state-tics-set! st (fxsra (state-tics st) 1))))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_BRUISERSHOT) (* 20 FRACUNIT))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_HEADSHOT) (* 20 FRACUNIT))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_TROOPSHOT) (* 20 FRACUNIT))]
      [(and (not (fx= skill sk_nightmare)) (fx= gameskill sk_nightmare))
       (do ([i S_SARG_RUN1 (fx+ i 1)]) ((fx> i S_SARG_PAIN2))
         (let ([st (vector-ref states i)])
           (state-tics-set! st (fxsll (state-tics st) 1))))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_BRUISERSHOT) (* 15 FRACUNIT))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_HEADSHOT) (* 10 FRACUNIT))
       (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_TROOPSHOT) (* 10 FRACUNIT))])
    ;; force players to be initialized upon first level load
    (vector-for-each (lambda (p) (player-playerstate-set! p PST_REBORN)) players)
    (set! usergame #t)                  ; will be set false if a demo
    (set! paused #f)
    (set! demoplayback #f)
    (set! automapactive #f)
    (set! viewactive #t)
    (set! gameepisode episode)
    (set! gamemap map)
    (set! gameskill skill)
    ;; set the sky map for the episode
    (set! skytexture
          (if (fx= gamemode commercial)
              (r-texture-num-for-name (cond [(fx< gamemap 12) "SKY1"] [(fx< gamemap 21) "SKY2"] [else "SKY3"]))
              (r-texture-num-for-name (case episode [(1) "SKY1"] [(2) "SKY2"] [(3) "SKY3"] [else "SKY4"]))))
    (g-do-load-level)))

;; DEMO RECORDING
(define DEMOMARKER #x80)

(define (next-demo-byte)
  (let ([b (bytevector-u8-ref demobuffer demo-p)])
    (set! demo-p (fx+ demo-p 1))
    b))

(define (put-demo-byte! b)
  (bytevector-u8-set! demobuffer demo-p (fxand b #xFF))
  (set! demo-p (fx+ demo-p 1)))

;; G_ReadDemoTiccmd
(define (g-read-demo-ticcmd cmd)
  (if (fx= (bytevector-u8-ref demobuffer demo-p) DEMOMARKER)
      ;; end of demo data stream
      (g-check-demo-status)
      (begin
        (ticcmd-forwardmove-set! cmd (s8 (next-demo-byte)))
        (ticcmd-sidemove-set! cmd (s8 (next-demo-byte)))
        (ticcmd-angleturn-set! cmd (s16 (fxsll (next-demo-byte) 8)))
        (ticcmd-buttons-set! cmd (next-demo-byte)))))

;; G_WriteDemoTiccmd. The command is then read back, so play goes on with
;; exactly what was recorded (angleturn rounded to its high byte).
(define (g-write-demo-ticcmd cmd)
  (when (vector-ref gamekeydown (char->integer #\q)) ; press q to end demo recording
    (g-check-demo-status))
  (put-demo-byte! (ticcmd-forwardmove cmd))
  (put-demo-byte! (ticcmd-sidemove cmd))
  (put-demo-byte! (fxsra (fx+ (ticcmd-angleturn cmd) 128) 8))
  (put-demo-byte! (ticcmd-buttons cmd))
  (set! demo-p (fx- demo-p 4))
  (if (fx> demo-p (fx- demoend 16))
      ;; no more space
      (g-check-demo-status)
      (g-read-demo-ticcmd cmd)))        ; make SURE it is exactly the same

;; G_RecordDemo
(define (g-record-demo name)
  (set! usergame #f)
  (set! demoname (string-append name ".lmp"))
  (let* ([i (m-check-parm "-maxdemo")]
         [maxsize (if (and (fx> i 0) (fx< i (fx- myargc 1)))
                      (* 1024 (or (string->number (vector-ref myargv (fx+ i 1))) 0))
                      #x20000)])
    (set! demobuffer (make-bytevector maxsize 0))
    (set! demoend maxsize))
  (set! demorecording #t))

(define VERSION 110)

(define (g-begin-recording)
  (set! demo-p 0)
  (put-demo-byte! VERSION)
  (put-demo-byte! gameskill)
  (put-demo-byte! gameepisode)
  (put-demo-byte! gamemap)
  (put-demo-byte! deathmatch)
  (put-demo-byte! (if respawnparm 1 0))
  (put-demo-byte! (if fastparm 1 0))
  (put-demo-byte! (if nomonsters 1 0))
  (put-demo-byte! consoleplayer)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (put-demo-byte! (if (vector-ref playeringame i) 1 0))))

;; G_PlayDemo

(define (g-defered-play-demo name)
  (set! defdemoname name)
  (set! gameaction ga_playdemo))

;; G_DoPlayDemo. Like the oracle, this also accepts version 109 demos (the
;; shareware's own).
(define (g-do-play-demo)
  (g-play-demo-bytes (w-cache-lump-name defdemoname)))

;; The rest of G_DoPlayDemo, from a demo's bytes (tests play demo files
;; with this).
(define (g-play-demo-bytes bv)
  (set! gameaction ga_nothing)
  (set! demobuffer bv)
  (set! demo-p 0)
  (let ([version (next-demo-byte)])
    (if (not (memv version (list VERSION 109)))
        (begin
          (display "Demo is from a different game version!\n" (current-error-port))
          (set! gameaction ga_nothing))
        (let* ([skill (next-demo-byte)]
               [episode (next-demo-byte)]
               [map (next-demo-byte)])
          (set! deathmatch (next-demo-byte))
          (set! respawnparm (not (fx= (next-demo-byte) 0)))
          (set! fastparm (not (fx= (next-demo-byte) 0)))
          (set! nomonsters (not (fx= (next-demo-byte) 0)))
          (set! consoleplayer (next-demo-byte))
          (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
            (vector-set! playeringame i (not (fx= (next-demo-byte) 0))))
          (when (vector-ref playeringame 1)
            (set! netgame #t)
            (set! netdemo #t))
          ;; don't spend a lot of time in loadlevel
          (set! precache #f)
          (g-init-new skill episode map)
          (set! precache #t)
          (set! usergame #f)
          (set! demoplayback #t)))))

;; G_TimeDemo
(define (g-time-demo name)
  (set! nodrawers (not (fx= (m-check-parm "-nodraw") 0)))
  (set! noblit (not (fx= (m-check-parm "-noblit") 0)))
  (set! timingdemo #t)
  (set! singletics #t)
  (set! defdemoname name)
  (set! gameaction ga_playdemo))

;; G_CheckDemoStatus: called after a death or level completion to allow
;; demos to be cleaned up. Returns true if a new demo loop action will take
;; place.
(define (g-check-demo-status)
  (cond
    [timingdemo
     (i-error "timed ~a gametics in ~a realtics" gametic (fx- (i-get-time) starttime))]
    [demoplayback
     (when singledemo
       (i-quit))
     (set! demoplayback #f)
     (set! netdemo #f)
     (set! netgame #f)
     (set! deathmatch 0)
     (vector-set! playeringame 1 #f)
     (vector-set! playeringame 2 #f)
     (vector-set! playeringame 3 #f)
     (set! respawnparm #f)
     (set! fastparm #f)
     (set! nomonsters #f)
     (set! consoleplayer 0)
     (d-advance-demo)
     #t]
    [demorecording
     (put-demo-byte! DEMOMARKER)
     (m-write-file demoname demobuffer demo-p)
     (set! demorecording #f)
     (i-error "Demo ~a recorded" demoname)]
    [else #f]))
