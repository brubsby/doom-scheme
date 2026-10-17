;;; m_menu.c: DOOM selection menu, options, episode etc. Sliders and
;;; icons. Kinda widget stuff. Included by d_main.sls.

;; (mouseSensitivity, showMessages, detailLevel and screenblocks: m_misc's
;; defaults)
(define screenSize 0)                   ; temp for screenblocks (0-9)
(define quickSaveSlot -1)               ; -1 = no quicksave slot picked!
(define messageToPrint 0)               ; 1 = message to be printed
(define messageString "")               ; ...and here is the message string!
(define messageLastMenuActive #f)
(define messageNeedsInput #f)           ; timed message = no input from user
(define messageRoutine #f)

(define gammamsg (vector GAMMALVL0 GAMMALVL1 GAMMALVL2 GAMMALVL3 GAMMALVL4))

;; we are going to be entering a savegame string
(define saveStringEnter 0)
(define saveSlot 0)                     ; which slot to save in
(define saveCharIndex 0)                ; which char we're editing
;; old save description before edit
(define saveOldString (make-bytevector SAVESTRINGSIZE 0))

(define inhelpscreens #f)

(define SKULLXOFF -32)
(define LINEHEIGHT 16)

;; char savegamestrings[10][SAVESTRINGSIZE], contiguous as in C (a string
;; without its NUL runs on into the next slot).
(define savegamestrings (make-bytevector (* 10 SAVESTRINGSIZE) 0))
(define (savestring-offset i) (fx* i SAVESTRINGSIZE))

;; The C string at offset in bv, as a Scheme string.
(define (c-string bv offset)
  (let loop ([i offset] [acc '()])
    (if (or (fx= i (bytevector-length bv)) (fx= (bytevector-u8-ref bv i) 0))
        (list->string (reverse acc))
        (loop (fx+ i 1) (cons (integer->char (bytevector-u8-ref bv i)) acc)))))

;; menuitem_t: status (0 = no cursor here, 1 = ok, 2 = arrows ok, -1 =
;; blank line), name, routine (choice = menu item #; if status = 2,
;; choice=0:leftarrow,1:rightarrow), hotkey in menu.
(define-record-type menuitem
  (fields (mutable status) (immutable name) (mutable routine) (immutable alphaKey)))
;; menu_t: # of menu items, previous menu, menu items, draw routine, x,y of
;; menu, last item user was on in menu.
(define-record-type menu
  (fields (mutable numitems) (mutable prevMenu) (immutable menuitems) (mutable routine)
          (mutable x) (mutable y) (mutable lastOn)))

(define (item status name routine key)
  (make-menuitem status name routine (if (char? key) (char->integer key) key)))

(define itemOn 0)                       ; menu item skull is on
(define skullAnimCounter 0)             ; skull animation counter
(define whichSkull 0)                   ; which skull to draw
;; graphic name of skulls
(define skullName (vector "M_SKULL1" "M_SKULL2"))
(define currentMenu #f)                 ; current menudef

;; DOOM MENU
(define newgame 0)
(define readthis 4)
(define quitdoom 5)
(define MainMenu
  (vector (item 1 "M_NGAME" (lambda (c) (m-new-game c)) #\n)
          (item 1 "M_OPTION" (lambda (c) (m-options c)) #\o)
          (item 1 "M_LOADG" (lambda (c) (m-load-game c)) #\l)
          (item 1 "M_SAVEG" (lambda (c) (m-save-game c)) #\s)
          ;; Another hickup with Special edition.
          (item 1 "M_RDTHIS" (lambda (c) (m-read-this c)) #\r)
          (item 1 "M_QUITG" (lambda (c) (m-quit-doom c)) #\q)))
(define MainDef (make-menu 6 #f MainMenu (lambda () (m-draw-main-menu)) 97 64 0))

;; EPISODE SELECT
(define EpisodeMenu
  (vector (item 1 "M_EPI1" (lambda (c) (m-episode c)) #\k)
          (item 1 "M_EPI2" (lambda (c) (m-episode c)) #\t)
          (item 1 "M_EPI3" (lambda (c) (m-episode c)) #\i)
          (item 1 "M_EPI4" (lambda (c) (m-episode c)) #\t)))
(define EpiDef (make-menu 4 MainDef EpisodeMenu (lambda () (m-draw-episode)) 48 63 0))

;; NEW GAME
(define nightmare 4)
(define hurtme 2)
(define NewGameMenu
  (vector (item 1 "M_JKILL" (lambda (c) (m-choose-skill c)) #\i)
          (item 1 "M_ROUGH" (lambda (c) (m-choose-skill c)) #\h)
          (item 1 "M_HURT" (lambda (c) (m-choose-skill c)) #\h)
          (item 1 "M_ULTRA" (lambda (c) (m-choose-skill c)) #\u)
          (item 1 "M_NMARE" (lambda (c) (m-choose-skill c)) #\n)))
(define NewDef (make-menu 5 EpiDef NewGameMenu (lambda () (m-draw-new-game)) 48 63 hurtme))

;; OPTIONS MENU
(define messages 1)
(define detail 2)
(define scrnsize 3)
(define mousesens 5)
(define OptionsMenu
  (vector (item 1 "M_ENDGAM" (lambda (c) (m-end-game c)) #\e)
          (item 1 "M_MESSG" (lambda (c) (m-change-messages c)) #\m)
          (item 1 "M_DETAIL" (lambda (c) (m-change-detail c)) #\g)
          (item 2 "M_SCRNSZ" (lambda (c) (m-size-display c)) #\s)
          (item -1 "" #f 0)
          (item 2 "M_MSENS" (lambda (c) (m-change-sensitivity c)) #\m)
          (item -1 "" #f 0)
          (item 1 "M_SVOL" (lambda (c) (m-sound c)) #\s)))
(define OptionsDef (make-menu 8 MainDef OptionsMenu (lambda () (m-draw-options)) 60 37 0))

;; Read This! MENU 1 & 2
(define ReadMenu1 (vector (item 1 "" (lambda (c) (m-read-this2 c)) 0)))
(define ReadDef1 (make-menu 1 MainDef ReadMenu1 (lambda () (m-draw-read-this1)) 280 185 0))
(define ReadMenu2 (vector (item 1 "" (lambda (c) (m-finish-read-this c)) 0)))
(define ReadDef2 (make-menu 1 ReadDef1 ReadMenu2 (lambda () (m-draw-read-this2)) 330 175 0))

;; SOUND VOLUME MENU
(define sfx_vol 0)
(define music_vol 2)
(define SoundMenu
  (vector (item 2 "M_SFXVOL" (lambda (c) (m-sfx-vol c)) #\s)
          (item -1 "" #f 0)
          (item 2 "M_MUSVOL" (lambda (c) (m-music-vol c)) #\m)
          (item -1 "" #f 0)))
(define SoundDef (make-menu 4 OptionsDef SoundMenu (lambda () (m-draw-sound)) 80 64 0))

;; LOAD GAME MENU
(define load_end 6)
(define LoadMenu
  (let ([v (make-vector load_end)])
    (do ([i 0 (fx+ i 1)]) ((fx= i load_end) v)
      (vector-set! v i (item 1 "" (lambda (c) (m-load-select c)) (fx+ (char->integer #\1) i))))))
(define LoadDef (make-menu load_end MainDef LoadMenu (lambda () (m-draw-load)) 80 54 0))

;; SAVE GAME MENU
(define SaveMenu
  (let ([v (make-vector load_end)])
    (do ([i 0 (fx+ i 1)]) ((fx= i load_end) v)
      (vector-set! v i (item 1 "" (lambda (c) (m-save-select c)) (fx+ (char->integer #\1) i))))))
(define SaveDef (make-menu load_end MainDef SaveMenu (lambda () (m-draw-save)) 80 54 0))

(define SAVEGAMENAME "doomsav")
(define (savegame-name i)
  (if (fx> (m-check-parm "-cdrom") 0)
      (format "c:\\doomdata\\~a~a.dsg" SAVEGAMENAME i)
      (format "~a~a.dsg" SAVEGAMENAME i)))

(define (set-savestring! i str)
  (let ([o (savestring-offset i)])
    (do ([j 0 (fx+ j 1)]) ((fx= j (string-length str)))
      (bytevector-u8-set! savegamestrings (fx+ o j) (char->integer (string-ref str j))))
    (bytevector-u8-set! savegamestrings (fx+ o (string-length str)) 0)))

;; M_ReadSaveStrings: read the strings from the savegame files
(define (m-read-save-strings)
  (do ([i 0 (fx+ i 1)]) ((fx= i load_end))
    (let ([name (savegame-name i)])
      (if (not (file-exists? name))
          (begin
            (set-savestring! i EMPTYSTRING)
            (menuitem-status-set! (vector-ref LoadMenu i) 0))
          (let ([bv (call-with-port (open-file-input-port name)
                      (lambda (p) (get-bytevector-n p SAVESTRINGSIZE)))])
            (unless (eof-object? bv)
              (bytevector-copy! bv 0 savegamestrings (savestring-offset i) (bytevector-length bv)))
            (menuitem-status-set! (vector-ref LoadMenu i) 1))))))

;; M_LoadGame & Cie.
(define (m-draw-load)
  (v-draw-patch-direct 72 28 0 (w-cache-lump-name "M_LOADG"))
  (do ([i 0 (fx+ i 1)]) ((fx= i load_end))
    (m-draw-save-load-border (menu-x LoadDef) (fx+ (menu-y LoadDef) (fx* LINEHEIGHT i)))
    (m-write-text (menu-x LoadDef) (fx+ (menu-y LoadDef) (fx* LINEHEIGHT i))
                  (c-string savegamestrings (savestring-offset i)))))

;; Draw border for the savegame description
(define (m-draw-save-load-border x y)
  (v-draw-patch-direct (fx- x 8) (fx+ y 7) 0 (w-cache-lump-name "M_LSLEFT"))
  (let loop ([i 0] [x x])
    (if (fx< i 24)
        (begin
          (v-draw-patch-direct x (fx+ y 7) 0 (w-cache-lump-name "M_LSCNTR"))
          (loop (fx+ i 1) (fx+ x 8)))
        (v-draw-patch-direct x (fx+ y 7) 0 (w-cache-lump-name "M_LSRGHT")))))

;; User wants to load this game
(define (m-load-select choice)
  (g-load-game (savegame-name choice))
  (m-clear-menus))

;; Selected from DOOM menu
(define (m-load-game choice)
  (if netgame
      (m-start-message LOADNET #f #f)
      (begin
        (m-setup-next-menu LoadDef)
        (m-read-save-strings))))

;; M_SaveGame & Cie.
(define (m-draw-save)
  (v-draw-patch-direct 72 28 0 (w-cache-lump-name "M_SAVEG"))
  (do ([i 0 (fx+ i 1)]) ((fx= i load_end))
    (m-draw-save-load-border (menu-x LoadDef) (fx+ (menu-y LoadDef) (fx* LINEHEIGHT i)))
    (m-write-text (menu-x LoadDef) (fx+ (menu-y LoadDef) (fx* LINEHEIGHT i))
                  (c-string savegamestrings (savestring-offset i))))
  (unless (fx= saveStringEnter 0)
    (let ([i (m-string-width (c-string savegamestrings (savestring-offset saveSlot)))])
      (m-write-text (fx+ (menu-x LoadDef) i) (fx+ (menu-y LoadDef) (fx* LINEHEIGHT saveSlot)) "_"))))

;; M_Responder calls this when user is finished
(define (m-do-save slot)
  (g-save-game slot (c-string savegamestrings (savestring-offset slot)))
  (m-clear-menus)
  ;; PICK QUICKSAVE SLOT YET?
  (when (fx= quickSaveSlot -2)
    (set! quickSaveSlot slot)))

;; User wants to save. Start string input for M_Responder
(define (m-save-select choice)
  ;; we are going to be intercepting all chars
  (set! saveStringEnter 1)
  (set! saveSlot choice)
  (let ([o (savestring-offset choice)])
    (bytevector-copy! savegamestrings o saveOldString 0 SAVESTRINGSIZE)
    (when (string=? (c-string savegamestrings o) EMPTYSTRING)
      (bytevector-u8-set! savegamestrings o 0))
    (set! saveCharIndex (string-length (c-string savegamestrings o)))))

;; Selected from DOOM menu
(define (m-save-game choice)
  (cond
    [(not usergame) (m-start-message SAVEDEAD #f #f)]
    [(not (fx= gamestate GS_LEVEL)) (void)]
    [else
     (m-setup-next-menu SaveDef)
     (m-read-save-strings)]))

;; M_QuickSave
(define (m-quick-save-response ch)
  (when (fx= ch (char->integer #\y))
    (m-do-save quickSaveSlot)
    (s-start-sound #f sfx_swtchx)))

;; QSPROMPT and QLPROMPT have one %s.
(define (sprintf-s fmt s)
  (let ([i (let loop ([i 0]) (if (string=? (substring fmt i (fx+ i 2)) "%s") i (loop (fx+ i 1))))])
    (string-append (substring fmt 0 i) s (substring fmt (fx+ i 2) (string-length fmt)))))

(define (m-quick-save)
  (cond
    [(not usergame) (s-start-sound #f sfx_oof)]
    [(not (fx= gamestate GS_LEVEL)) (void)]
    [(fx< quickSaveSlot 0)
     (m-start-control-panel)
     (m-read-save-strings)
     (m-setup-next-menu SaveDef)
     (set! quickSaveSlot -2)]           ; means to pick a slot now
    [else
     (m-start-message (sprintf-s QSPROMPT (c-string savegamestrings (savestring-offset quickSaveSlot)))
                      m-quick-save-response #t)]))

;; M_QuickLoad
(define (m-quick-load-response ch)
  (when (fx= ch (char->integer #\y))
    (m-load-select quickSaveSlot)
    (s-start-sound #f sfx_swtchx)))

(define (m-quick-load)
  (cond
    [netgame (m-start-message QLOADNET #f #f)]
    [(fx< quickSaveSlot 0) (m-start-message QSAVESPOT #f #f)]
    [else
     (m-start-message (sprintf-s QLPROMPT (c-string savegamestrings (savestring-offset quickSaveSlot)))
                      m-quick-load-response #t)]))

;; Read This Menus. Had a "quick hack to fix romero bug"
(define (m-draw-read-this1)
  (set! inhelpscreens #t)
  (cond
    [(fx= gamemode commercial)
     (v-draw-patch-direct 0 0 0 (w-cache-lump-name "HELP"))]
    [(memv gamemode (list shareware registered retail))
     (v-draw-patch-direct 0 0 0 (w-cache-lump-name "HELP1"))]))

;; Read This Menus - optional second page.
(define (m-draw-read-this2)
  (set! inhelpscreens #t)
  (cond
    ;; This hack keeps us from having to change menus.
    [(memv gamemode (list retail commercial))
     (v-draw-patch-direct 0 0 0 (w-cache-lump-name "CREDIT"))]
    [(memv gamemode (list shareware registered))
     (v-draw-patch-direct 0 0 0 (w-cache-lump-name "HELP2"))]))

;; Change Sfx & Music volumes
(define (m-draw-sound)
  (v-draw-patch-direct 60 38 0 (w-cache-lump-name "M_SVOL"))
  (m-draw-thermo (menu-x SoundDef) (fx+ (menu-y SoundDef) (fx* LINEHEIGHT (fx+ sfx_vol 1)))
                 16 snd_SfxVolume)
  (m-draw-thermo (menu-x SoundDef) (fx+ (menu-y SoundDef) (fx* LINEHEIGHT (fx+ music_vol 1)))
                 16 snd_MusicVolume))

(define (m-sound choice)
  (m-setup-next-menu SoundDef))

(define (m-sfx-vol choice)
  (case choice
    [(0) (unless (fx= snd_SfxVolume 0) (set! snd_SfxVolume (fx- snd_SfxVolume 1)))]
    [(1) (when (fx< snd_SfxVolume 15) (set! snd_SfxVolume (fx+ snd_SfxVolume 1)))])
  (s-set-sfx-volume snd_SfxVolume))

(define (m-music-vol choice)
  (case choice
    [(0) (unless (fx= snd_MusicVolume 0) (set! snd_MusicVolume (fx- snd_MusicVolume 1)))]
    [(1) (when (fx< snd_MusicVolume 15) (set! snd_MusicVolume (fx+ snd_MusicVolume 1)))])
  (s-set-music-volume snd_MusicVolume))

(define (m-draw-main-menu)
  (v-draw-patch-direct 94 2 0 (w-cache-lump-name "M_DOOM")))

(define (m-draw-new-game)
  (v-draw-patch-direct 96 14 0 (w-cache-lump-name "M_NEWG"))
  (v-draw-patch-direct 54 38 0 (w-cache-lump-name "M_SKILL")))

(define (m-new-game choice)
  (cond
    [(and netgame (not demoplayback)) (m-start-message NEWGAME #f #f)]
    [(fx= gamemode commercial) (m-setup-next-menu NewDef)]
    [else (m-setup-next-menu EpiDef)]))

;; M_Episode
(define epi 0)

(define (m-draw-episode)
  (v-draw-patch-direct 54 38 0 (w-cache-lump-name "M_EPISOD")))

(define (m-verify-nightmare ch)
  (when (fx= ch (char->integer #\y))
    (g-defered-init-new nightmare (fx+ epi 1) 1)
    (m-clear-menus)))

(define (m-choose-skill choice)
  (if (fx= choice nightmare)
      (m-start-message NIGHTMARE m-verify-nightmare #t)
      (begin
        (g-defered-init-new choice (fx+ epi 1) 1)
        (m-clear-menus))))

(define (m-episode choice)
  (if (and (fx= gamemode shareware) (not (fx= choice 0)))
      (begin
        (m-start-message SWSTRING #f #f)
        (m-setup-next-menu ReadDef1))
      (let ([choice (if (and (fx= gamemode registered) (fx> choice 2))
                        ;; Yet another hack...
                        (begin
                          (display "M_Episode: 4th episode requires UltimateDOOM\n" (current-error-port))
                          0)
                        choice)])
        (set! epi choice)
        (m-setup-next-menu NewDef))))

;; M_Options
(define detailNames (vector "M_GDHIGH" "M_GDLOW"))
(define msgNames (vector "M_MSGOFF" "M_MSGON"))

(define (m-draw-options)
  (v-draw-patch-direct 108 15 0 (w-cache-lump-name "M_OPTTTL"))
  (v-draw-patch-direct (fx+ (menu-x OptionsDef) 175) (fx+ (menu-y OptionsDef) (fx* LINEHEIGHT detail)) 0
                       (w-cache-lump-name (vector-ref detailNames detailLevel)))
  (v-draw-patch-direct (fx+ (menu-x OptionsDef) 120) (fx+ (menu-y OptionsDef) (fx* LINEHEIGHT messages)) 0
                       (w-cache-lump-name (vector-ref msgNames showMessages)))
  (m-draw-thermo (menu-x OptionsDef) (fx+ (menu-y OptionsDef) (fx* LINEHEIGHT (fx+ mousesens 1)))
                 10 mouseSensitivity)
  (m-draw-thermo (menu-x OptionsDef) (fx+ (menu-y OptionsDef) (fx* LINEHEIGHT (fx+ scrnsize 1)))
                 9 screenSize))

(define (m-options choice)
  (m-setup-next-menu OptionsDef))

;; Toggle messages on/off
(define (m-change-messages choice)
  (set! showMessages (fx- 1 showMessages))
  (player-message-set! (vector-ref players consoleplayer)
                       (if (fx= showMessages 0) MSGOFF MSGON))
  (set! message_dontfuckwithme #t))

;; M_EndGame
(define (m-end-game-response ch)
  (when (fx= ch (char->integer #\y))
    (menu-lastOn-set! currentMenu itemOn)
    (m-clear-menus)
    (d-start-title)))

(define (m-end-game choice)
  (cond
    [(not usergame) (s-start-sound #f sfx_oof)]
    [netgame (m-start-message NETEND #f #f)]
    [else (m-start-message ENDGAME m-end-game-response #t)]))

;; M_ReadThis
(define (m-read-this choice)
  (m-setup-next-menu ReadDef1))

(define (m-read-this2 choice)
  (m-setup-next-menu ReadDef2))

(define (m-finish-read-this choice)
  (m-setup-next-menu MainDef))

;; M_QuitDOOM
(define quitsounds
  (fxvector sfx_pldeth sfx_dmpain sfx_popain sfx_slop sfx_telept sfx_posit1 sfx_posit3 sfx_sgtatk))
(define quitsounds2
  (fxvector sfx_vilact sfx_getpow sfx_boscub sfx_slop sfx_skeswg sfx_kntdth sfx_bspact sfx_sgtatk))

;; dstrings.c's endmsg[]. Two commas are missing in the C, so its strings 8
;; and 9, and 14 and 15, are joined, and the last of its 23 slots is NULL
;; (never chosen: M_QuitDOOM picks from 1-20).
(define NUM_QUITMESSAGES 22)
(define endmsg
  (vector
   ;; DOOM1
   QUITMSG
   "please don't leave, there's more\ndemons to toast!"
   "let's beat it -- this is turning\ninto a bloodbath!"
   "i wouldn't leave if i were you.\ndos is much worse."
   "you're trying to say you like dos\nbetter than me, right?"
   "don't leave yet -- there's a\ndemon around that corner!"
   "ya know, next time you come in here\ni'm gonna toast ya."
   (string-append "go ahead and leave. see if i care."
                  ;; QuitDOOM II messages
                  "you want to quit?\nthen, thou hast lost an eighth!")
   "don't go now, there's a \ndimensional shambler waiting\nat the dos prompt!"
   "get outta here and go back\nto your boring programs."
   "if i were your boss, i'd \n deathmatch ya in a minute!"
   "look, bud. you leave now\nand you forfeit your body count!"
   "just leave. when you come\nback, i'll be waiting with a bat."
   (string-append "you're lucky i don't smack\nyou for thinking about leaving."
                  ;; FinalDOOM?
                  "fuck you, pussy!\nget the fuck out!")
   "you quit and i'll jizz\nin your cystholes!"
   "if you leave, i'll make\nthe lord drink my jizz."
   "hey, ron! can we say\n'fuck' in the game?"
   "i'd leave: this is just\nmore monsters and levels.\nwhat a load."
   "suck it down, asshole!\nyou're a fucking wimp!"
   "don't quit now! we're \nstill spending your money!"
   ;; Internal debug. Different style, too.
   "THIS IS NO MESSAGE!\nPage intentionally left blank."))

(define (m-quit-response ch)
  (when (fx= ch (char->integer #\y))
    (unless netgame
      (s-start-sound #f (fxvector-ref (if (fx= gamemode commercial) quitsounds2 quitsounds)
                                      (fxand (fxsra gametic 2) 7)))
      (i-wait-vbl 105))
    (i-quit)))

(define (m-quit-doom choice)
  ;; We pick index 0 which is language sensitive, or one at random, between
  ;; 1 and maximum number.
  (m-start-message (string-append (vector-ref endmsg (if (fx= language english)
                                                         (fx+ (fxremainder gametic (fx- NUM_QUITMESSAGES 2)) 1)
                                                         0))
                                  "\n\n" DOSY)
                   m-quit-response #t))

(define (m-change-sensitivity choice)
  (case choice
    [(0) (unless (fx= mouseSensitivity 0) (set! mouseSensitivity (fx- mouseSensitivity 1)))]
    [(1) (when (fx< mouseSensitivity 9) (set! mouseSensitivity (fx+ mouseSensitivity 1)))]))

(define (m-change-detail choice)
  (set! detailLevel (fx- 1 detailLevel))
  ;; FIXME - does not work. Remove anyway?
  (display "M_ChangeDetail: low detail mode n.a.\n" (current-error-port)))

(define (m-size-display choice)
  (case choice
    [(0) (when (fx> screenSize 0)
           (set! screenblocks (fx- screenblocks 1))
           (set! screenSize (fx- screenSize 1)))]
    [(1) (when (fx< screenSize 8)
           (set! screenblocks (fx+ screenblocks 1))
           (set! screenSize (fx+ screenSize 1)))])
  (r-set-view-size screenblocks detailLevel))

;; Menu Functions

(define (m-draw-thermo x y thermWidth thermDot)
  (v-draw-patch-direct x y 0 (w-cache-lump-name "M_THERML"))
  (let loop ([i 0] [xx (fx+ x 8)])
    (if (fx< i thermWidth)
        (begin
          (v-draw-patch-direct xx y 0 (w-cache-lump-name "M_THERMM"))
          (loop (fx+ i 1) (fx+ xx 8)))
        (v-draw-patch-direct xx y 0 (w-cache-lump-name "M_THERMR"))))
  (v-draw-patch-direct (fx+ x 8 (fx* thermDot 8)) y 0 (w-cache-lump-name "M_THERMO")))

(define (m-draw-empty-cell menu item)
  (v-draw-patch-direct (fx- (menu-x menu) 10) (fx- (fx+ (menu-y menu) (fx* item LINEHEIGHT)) 1) 0
                       (w-cache-lump-name "M_CELL1")))

(define (m-draw-sel-cell menu item)
  (v-draw-patch-direct (fx- (menu-x menu) 10) (fx- (fx+ (menu-y menu) (fx* item LINEHEIGHT)) 1) 0
                       (w-cache-lump-name "M_CELL2")))

(define (m-start-message string routine input)
  (set! messageLastMenuActive menuactive)
  (set! messageToPrint 1)
  (set! messageString string)
  (set! messageRoutine routine)
  (set! messageNeedsInput input)
  (set! menuactive #t))

(define (m-stop-message)
  (set! menuactive messageLastMenuActive)
  (set! messageToPrint 0))

(define (toupper c)
  (if (fx<= (char->integer #\a) c (char->integer #\z)) (fx- c 32) c))

;; Find string width from hu_font chars
(define (m-string-width string)
  (let loop ([i 0] [w 0])
    (if (fx= i (string-length string))
        w
        (let ([c (fx- (toupper (char->integer (string-ref string i))) HU_FONTSTART)])
          (loop (fx+ i 1)
                (if (or (fx< c 0) (fx>= c HU_FONTSIZE))
                    (fx+ w 4)
                    (fx+ w (patch-width (vector-ref hu_font c)))))))))

;; Find string height from hu_font chars
(define (m-string-height string)
  (let ([height (patch-height (vector-ref hu_font 0))])
    (let loop ([i 0] [h height])
      (if (fx= i (string-length string))
          h
          (loop (fx+ i 1) (if (char=? (string-ref string i) #\newline) (fx+ h height) h))))))

;; Write a string using the hu_font
(define (m-write-text x y string)
  (let loop ([i 0] [cx x] [cy y])
    (when (fx< i (string-length string))
      (let ([c (char->integer (string-ref string i))])
        (cond
          [(fx= c 0) (void)]
          [(fx= c 10) (loop (fx+ i 1) x (fx+ cy 12))]
          [else
           (let ([c (fx- (toupper c) HU_FONTSTART)])
             (if (or (fx< c 0) (fx>= c HU_FONTSIZE))
                 (loop (fx+ i 1) (fx+ cx 4) cy)
                 (let ([w (patch-width (vector-ref hu_font c))])
                   (unless (fx> (fx+ cx w) SCREENWIDTH)
                     (v-draw-patch-direct cx cy 0 (vector-ref hu_font c))
                     (loop (fx+ i 1) (fx+ cx w) cy)))))])))))

;; CONTROL PANEL

;; M_Responder
(define joywait 0)
(define mousewait 0)
(define m-mousey 0)
(define m-lasty 0)
(define m-mousex 0)
(define m-lastx 0)

(define (m-responder ev)
  (let ([ch -1] [type (event-type ev)])
    (cond
      [(and (fx= type ev_joystick) (fx< joywait (i-get-time)))
       (cond
         [(fx= (event-data3 ev) -1)
          (set! ch KEY_UPARROW)
          (set! joywait (fx+ (i-get-time) 5))]
         [(fx= (event-data3 ev) 1)
          (set! ch KEY_DOWNARROW)
          (set! joywait (fx+ (i-get-time) 5))])
       (cond
         [(fx= (event-data2 ev) -1)
          (set! ch KEY_LEFTARROW)
          (set! joywait (fx+ (i-get-time) 2))]
         [(fx= (event-data2 ev) 1)
          (set! ch KEY_RIGHTARROW)
          (set! joywait (fx+ (i-get-time) 2))])
       (when (fxlogtest (event-data1 ev) 1)
         (set! ch KEY_ENTER)
         (set! joywait (fx+ (i-get-time) 5)))
       (when (fxlogtest (event-data1 ev) 2)
         (set! ch KEY_BACKSPACE)
         (set! joywait (fx+ (i-get-time) 5)))]
      [(and (fx= type ev_mouse) (fx< mousewait (i-get-time)))
       (set! m-mousey (fx+ m-mousey (event-data3 ev)))
       (cond
         [(fx< m-mousey (fx- m-lasty 30))
          (set! ch KEY_DOWNARROW)
          (set! mousewait (fx+ (i-get-time) 5))
          (set! m-lasty (fx- m-lasty 30))
          (set! m-mousey m-lasty)]
         [(fx> m-mousey (fx+ m-lasty 30))
          (set! ch KEY_UPARROW)
          (set! mousewait (fx+ (i-get-time) 5))
          (set! m-lasty (fx+ m-lasty 30))
          (set! m-mousey m-lasty)])
       (set! m-mousex (fx+ m-mousex (event-data2 ev)))
       (cond
         [(fx< m-mousex (fx- m-lastx 30))
          (set! ch KEY_LEFTARROW)
          (set! mousewait (fx+ (i-get-time) 5))
          (set! m-lastx (fx- m-lastx 30))
          (set! m-mousex m-lastx)]
         [(fx> m-mousex (fx+ m-lastx 30))
          (set! ch KEY_RIGHTARROW)
          (set! mousewait (fx+ (i-get-time) 5))
          (set! m-lastx (fx+ m-lastx 30))
          (set! m-mousex m-lastx)])
       (when (fxlogtest (event-data1 ev) 1)
         (set! ch KEY_ENTER)
         (set! mousewait (fx+ (i-get-time) 15)))
       (when (fxlogtest (event-data1 ev) 2)
         (set! ch KEY_BACKSPACE)
         (set! mousewait (fx+ (i-get-time) 15)))]
      [(fx= type ev_keydown)
       (set! ch (event-data1 ev))])
    (call/1cc
     (lambda (return)
       (when (fx= ch -1)
         (return #f))
       ;; Save Game string input
       (unless (fx= saveStringEnter 0)
         (let ([o (savestring-offset saveSlot)])
           (cond
             [(fx= ch KEY_BACKSPACE)
              (when (fx> saveCharIndex 0)
                (set! saveCharIndex (fx- saveCharIndex 1))
                (bytevector-u8-set! savegamestrings (fx+ o saveCharIndex) 0))]
             [(fx= ch KEY_ESCAPE)
              (set! saveStringEnter 0)
              (bytevector-copy! saveOldString 0 savegamestrings o SAVESTRINGSIZE)]
             [(fx= ch KEY_ENTER)
              (set! saveStringEnter 0)
              (unless (fx= (bytevector-u8-ref savegamestrings o) 0)
                (m-do-save saveSlot))]
             [else
              (let ([ch (toupper ch)])
                (unless (and (not (fx= ch 32))
                             (or (fx< (fx- ch HU_FONTSTART) 0) (fx>= (fx- ch HU_FONTSTART) HU_FONTSIZE)))
                  (when (and (fx>= ch 32) (fx<= ch 127)
                             (fx< saveCharIndex (fx- SAVESTRINGSIZE 1))
                             (fx< (m-string-width (c-string savegamestrings o)) (fx* (fx- SAVESTRINGSIZE 2) 8)))
                    (bytevector-u8-set! savegamestrings (fx+ o saveCharIndex) ch)
                    (set! saveCharIndex (fx+ saveCharIndex 1))
                    (bytevector-u8-set! savegamestrings (fx+ o saveCharIndex) 0))))]))
         (return #t))
       ;; Take care of any messages that need input
       (unless (fx= messageToPrint 0)
         (when (and messageNeedsInput
                    (not (memv ch (list 32 (char->integer #\n) (char->integer #\y) KEY_ESCAPE))))
           (return #f))
         (set! menuactive messageLastMenuActive)
         (set! messageToPrint 0)
         (when messageRoutine
           (messageRoutine ch))
         (set! menuactive #f)
         (s-start-sound #f sfx_swtchx)
         (return #t))
       (when (and devparm (fx= ch KEY_F1))
         (g-screen-shot)
         (return #t))
       ;; F-Keys
       (unless menuactive
         (cond
           [(fx= ch KEY_MINUS)          ; Screen size down
            (when (or automapactive chat_on)
              (return #f))
            (m-size-display 0)
            (s-start-sound #f sfx_stnmov)
            (return #t)]
           [(fx= ch KEY_EQUALS)         ; Screen size up
            (when (or automapactive chat_on)
              (return #f))
            (m-size-display 1)
            (s-start-sound #f sfx_stnmov)
            (return #t)]
           [(fx= ch KEY_F1)             ; Help key
            (m-start-control-panel)
            (set! currentMenu (if (fx= gamemode retail) ReadDef2 ReadDef1))
            (set! itemOn 0)
            (s-start-sound #f sfx_swtchn)
            (return #t)]
           [(fx= ch KEY_F2)             ; Save
            (m-start-control-panel)
            (s-start-sound #f sfx_swtchn)
            (m-save-game 0)
            (return #t)]
           [(fx= ch KEY_F3)             ; Load
            (m-start-control-panel)
            (s-start-sound #f sfx_swtchn)
            (m-load-game 0)
            (return #t)]
           [(fx= ch KEY_F4)             ; Sound Volume
            (m-start-control-panel)
            (set! currentMenu SoundDef)
            (set! itemOn sfx_vol)
            (s-start-sound #f sfx_swtchn)
            (return #t)]
           [(fx= ch KEY_F5)             ; Detail toggle
            (m-change-detail 0)
            (s-start-sound #f sfx_swtchn)
            (return #t)]
           [(fx= ch KEY_F6)             ; Quicksave
            (s-start-sound #f sfx_swtchn)
            (m-quick-save)
            (return #t)]
           [(fx= ch KEY_F7)             ; End game
            (s-start-sound #f sfx_swtchn)
            (m-end-game 0)
            (return #t)]
           [(fx= ch KEY_F8)             ; Toggle messages
            (m-change-messages 0)
            (s-start-sound #f sfx_swtchn)
            (return #t)]
           [(fx= ch KEY_F9)             ; Quickload
            (s-start-sound #f sfx_swtchn)
            (m-quick-load)
            (return #t)]
           [(fx= ch KEY_F10)            ; Quit DOOM
            (s-start-sound #f sfx_swtchn)
            (m-quit-doom 0)
            (return #t)]
           [(fx= ch KEY_F11)            ; gamma toggle
            (set! usegamma (fx+ usegamma 1))
            (when (fx> usegamma 4)
              (set! usegamma 0))
            (player-message-set! (vector-ref players consoleplayer) (vector-ref gammamsg usegamma))
            (i-set-palette (w-cache-lump-name "PLAYPAL"))
            (return #t)]))
       ;; Pop-up menu?
       (unless menuactive
         (when (fx= ch KEY_ESCAPE)
           (m-start-control-panel)
           (s-start-sound #f sfx_swtchn)
           (return #t))
         (return #f))
       ;; Keys usable within menu
       (let* ([items (menu-menuitems currentMenu)]
              [cur (lambda () (vector-ref items itemOn))])
         (cond
           [(fx= ch KEY_DOWNARROW)
            (let loop ()
              (set! itemOn (if (fx> (fx+ itemOn 1) (fx- (menu-numitems currentMenu) 1)) 0 (fx+ itemOn 1)))
              (s-start-sound #f sfx_pstop)
              (when (fx= (menuitem-status (cur)) -1) (loop)))
            #t]
           [(fx= ch KEY_UPARROW)
            (let loop ()
              (set! itemOn (if (fx= itemOn 0) (fx- (menu-numitems currentMenu) 1) (fx- itemOn 1)))
              (s-start-sound #f sfx_pstop)
              (when (fx= (menuitem-status (cur)) -1) (loop)))
            #t]
           [(fx= ch KEY_LEFTARROW)
            (when (and (menuitem-routine (cur)) (fx= (menuitem-status (cur)) 2))
              (s-start-sound #f sfx_stnmov)
              ((menuitem-routine (cur)) 0))
            #t]
           [(fx= ch KEY_RIGHTARROW)
            (when (and (menuitem-routine (cur)) (fx= (menuitem-status (cur)) 2))
              (s-start-sound #f sfx_stnmov)
              ((menuitem-routine (cur)) 1))
            #t]
           [(fx= ch KEY_ENTER)
            (when (and (menuitem-routine (cur)) (not (fx= (menuitem-status (cur)) 0)))
              (menu-lastOn-set! currentMenu itemOn)
              (if (fx= (menuitem-status (cur)) 2)
                  (begin
                    ((menuitem-routine (cur)) 1) ; right arrow
                    (s-start-sound #f sfx_stnmov))
                  (begin
                    ((menuitem-routine (cur)) itemOn)
                    (s-start-sound #f sfx_pistol))))
            #t]
           [(fx= ch KEY_ESCAPE)
            (menu-lastOn-set! currentMenu itemOn)
            (m-clear-menus)
            (s-start-sound #f sfx_swtchx)
            #t]
           [(fx= ch KEY_BACKSPACE)
            (menu-lastOn-set! currentMenu itemOn)
            (when (menu-prevMenu currentMenu)
              (set! currentMenu (menu-prevMenu currentMenu))
              (set! itemOn (menu-lastOn currentMenu))
              (s-start-sound #f sfx_swtchn))
            #t]
           [else
            (let ([hit (lambda (i)
                         (set! itemOn i)
                         (s-start-sound #f sfx_pstop)
                         #t)])
              (let loop ([i (fx+ itemOn 1)])
                (cond
                  [(fx< i (menu-numitems currentMenu))
                   (if (fx= (menuitem-alphaKey (vector-ref items i)) ch)
                       (hit i)
                       (loop (fx+ i 1)))]
                  [else
                   (let loop ([i 0])
                     (cond
                       [(fx> i itemOn) #f]
                       [(fx= (menuitem-alphaKey (vector-ref items i)) ch) (hit i)]
                       [else (loop (fx+ i 1))]))])))]))))))

;; M_StartControlPanel
(define (m-start-control-panel)
  ;; intro might call this repeatedly
  (unless menuactive
    (set! menuactive #t)
    (set! currentMenu MainDef)          ; JDC
    (set! itemOn (menu-lastOn currentMenu)))) ; JDC

;; M_Drawer: called after the view has been rendered, but before it has
;; been blitted.
(define (m-drawer)
  (set! inhelpscreens #f)
  (cond
    ;; Horiz. & Vertically center string and print it.
    [(not (fx= messageToPrint 0))
     ;; (as C: after a line ending in '\n', a remainder exactly as long as
     ;; that line is printed instead of it)
     (let ([msg messageString] [len (string-length messageString)])
       (let loop ([start 0] [y (fx- 100 (fxquotient (m-string-height msg) 2))])
         (when (fx< start len)
           (let* ([nl (let find ([i 0])
                        (cond [(fx= (fx+ start i) len) #f]
                              [(char=? (string-ref msg (fx+ start i)) #\newline) i]
                              [else (find (fx+ i 1))]))]
                  [i (or nl (fx- len start))]
                  [string (if nl (substring msg start (fx+ start i)) "")]
                  [start (if nl (fx+ start i 1) start)]
                  [rest (fx- len start)])
             (let-values ([(string start)
                           (if (fx= i rest)
                               (values (substring msg start len) (fx+ start i))
                               (values string start))])
               ;; C copies the line into char string[40]. A line of 40 or
               ;; more characters overflows it (strncpy leaves it
               ;; unterminated, so its width, and so where it's centered,
               ;; comes from whatever bytes follow on the stack): the port
               ;; prints the line, and marks its band as a garbage read.
               (when (and garbage-mask (fx>= (string-length string) 40))
                 (let ([h (patch-height (vector-ref hu_font 0))])
                   (do ([row (fxmax 0 (fx- y 2)) (fx+ row 1)])
                       ((fx>= row (fxmin SCREENHEIGHT (fx+ y h 2))))
                     (do ([x 0 (fx+ x 1)]) ((fx= x SCREENWIDTH))
                       (bytevector-u8-set! garbage-mask (fx+ (fx* row SCREENWIDTH) x) 1)))))
               (m-write-text (fx- 160 (fxquotient (m-string-width string) 2)) y string)
               (loop start (fx+ y (patch-height (vector-ref hu_font 0)))))))))]
    [(not menuactive) (void)]
    [else
     (when (menu-routine currentMenu)
       ((menu-routine currentMenu)))   ; call Draw routine
     ;; DRAW MENU
     (let ([x (menu-x currentMenu)]
           [items (menu-menuitems currentMenu)])
       (let loop ([i 0] [y (menu-y currentMenu)])
         (when (fx< i (menu-numitems currentMenu))
           (let ([name (menuitem-name (vector-ref items i))])
             (unless (string=? name "")
               (v-draw-patch-direct x y 0 (w-cache-lump-name name))))
           (loop (fx+ i 1) (fx+ y LINEHEIGHT))))
       ;; DRAW SKULL
       (v-draw-patch-direct (fx+ x SKULLXOFF) (fx+ (fx- (menu-y currentMenu) 5) (fx* itemOn LINEHEIGHT)) 0
                            (w-cache-lump-name (vector-ref skullName whichSkull))))]))

;; M_ClearMenus
(define (m-clear-menus)
  (set! menuactive #f))

;; M_SetupNextMenu
(define (m-setup-next-menu menudef)
  (set! currentMenu menudef)
  (set! itemOn (menu-lastOn currentMenu)))

;; M_Ticker
(define (m-ticker)
  (set! skullAnimCounter (fx- skullAnimCounter 1))
  (when (fx<= skullAnimCounter 0)
    (set! whichSkull (fxxor whichSkull 1))
    (set! skullAnimCounter 8)))

;; M_Init
(define (m-init)
  (set! currentMenu MainDef)
  (set! menuactive #f)
  (set! itemOn (menu-lastOn currentMenu))
  (set! whichSkull 0)
  (set! skullAnimCounter 10)
  (set! screenSize (fx- screenblocks 3))
  (set! messageToPrint 0)
  (set! messageString "")
  (set! messageLastMenuActive menuactive)
  (set! quickSaveSlot -1)
  ;; Here we could catch other version dependencies, like HELP1/2, and four
  ;; episodes.
  (cond
    [(fx= gamemode commercial)
     ;; This is used because DOOM 2 had only one HELP page. I use CREDIT as
     ;; second page now, but kept this hack for educational purposes.
     (vector-set! MainMenu readthis (vector-ref MainMenu quitdoom))
     (menu-numitems-set! MainDef (fx- (menu-numitems MainDef) 1))
     (menu-y-set! MainDef (fx+ (menu-y MainDef) 8))
     (menu-prevMenu-set! NewDef MainDef)
     (menu-routine-set! ReadDef1 (lambda () (m-draw-read-this1)))
     (menu-x-set! ReadDef1 330)
     (menu-y-set! ReadDef1 165)
     (menuitem-routine-set! (vector-ref ReadMenu1 0) (lambda (c) (m-finish-read-this c)))]
    ;; shareware: Episode 2 and 3 are handled, branching to an ad screen.
    ;; registered: We need to remove the fourth episode.
    [(memv gamemode (list shareware registered))
     (menu-numitems-set! EpiDef (fx- (menu-numitems EpiDef) 1))]))
