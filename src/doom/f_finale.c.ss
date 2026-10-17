;;; f_finale.c: the end of an episode: the text crawl, the art screens
;;; (and the bunny scroll), and DOOM II's cast. Included by d_main.sls.

;; Stage of animation: 0 = text, 1 = art screen, 2 = character cast
(define finalestage 0)
(define finalecount 0)

(define TEXTSPEED 3)
(define TEXTWAIT 250)

(define finaletext "")
(define finaleflat "")

;; F_StartFinale
(define (f-start-finale)
  (set! gameaction ga_nothing)
  (set! gamestate GS_FINALE)
  (set! viewactive #f)
  (set! automapactive #f)
  ;; Okay - IWAD dependend stuff. This has been changed severly, and some
  ;; stuff might have changed in the process.
  (cond
    ;; DOOM 1 - E1, E3 or E4, but each nine missions
    [(memv gamemode (list shareware registered retail))
     (s-change-music mus_victor #t)
     (case gameepisode
       [(1) (set! finaleflat "FLOOR4_8") (set! finaletext E1TEXT)]
       [(2) (set! finaleflat "SFLR6_1") (set! finaletext E2TEXT)]
       [(3) (set! finaleflat "MFLR8_4") (set! finaletext E3TEXT)]
       [(4) (set! finaleflat "MFLR8_3") (set! finaletext E4TEXT)])] ; (default: Ouch.)
    ;; DOOM II and missions packs with E1, M34
    [(fx= gamemode commercial)
     (s-change-music mus_read_m #t)
     (case gamemap
       [(6) (set! finaleflat "SLIME16") (set! finaletext C1TEXT)]
       [(11) (set! finaleflat "RROCK14") (set! finaletext C2TEXT)]
       [(20) (set! finaleflat "RROCK07") (set! finaletext C3TEXT)]
       [(30) (set! finaleflat "RROCK17") (set! finaletext C4TEXT)]
       [(15) (set! finaleflat "RROCK13") (set! finaletext C5TEXT)]
       [(31) (set! finaleflat "RROCK19") (set! finaletext C6TEXT)])] ; (default: Ouch.)
    ;; Indeterminate.
    [else
     (s-change-music mus_read_m #t)
     (set! finaleflat "F_SKY1")         ; Not used anywhere else.
     (set! finaletext C1TEXT)])         ; FIXME - other text, music?
  (set! finalestage 0)
  (set! finalecount 0))

(define (f-responder event)
  (if (fx= finalestage 2)
      (f-cast-responder event)
      #f))

;; F_Ticker
(define (f-ticker)
  ;; check for skipping
  (when (and (fx= gamemode commercial) (fx> finalecount 50))
    ;; go on to the next level
    (when (let loop ([i 0])
            (and (fx< i MAXPLAYERS)
                 (or (not (fx= (ticcmd-buttons (player-cmd (vector-ref players i))) 0))
                     (loop (fx+ i 1)))))
      (if (fx= gamemap 30)
          (f-start-cast)
          (set! gameaction ga_worlddone))))
  ;; advance animation
  (set! finalecount (fx+ finalecount 1))
  (cond
    [(fx= finalestage 2) (f-cast-ticker)]
    [(fx= gamemode commercial) (void)]
    [(and (fx= finalestage 0)
          (fx> finalecount (fx+ (fx* (string-length finaletext) TEXTSPEED) TEXTWAIT)))
     (set! finalecount 0)
     (set! finalestage 1)
     (set! wipegamestate -1)            ; force a wipe
     (when (fx= gameepisode 3)
       (s-start-music mus_bunny))]))

;; F_TextWrite
(define (f-text-write)
  ;; erase the entire screen to a tiled background
  (let ([src (w-cache-lump-name finaleflat)])
    (do ([y 0 (fx+ y 1)]) ((fx= y SCREENHEIGHT))
      (do ([x 0 (fx+ x 64)]) ((fx>= x SCREENWIDTH))
        (bytevector-copy! src (fxsll (fxand y 63) 6) screens (fx+ (fx* y SCREENWIDTH) x)
                          (fxmin 64 (fx- SCREENWIDTH x))))))
  (v-mark-rect 0 0 SCREENWIDTH SCREENHEIGHT)
  ;; draw some of the text onto the screen
  (let loop ([count (fxmax 0 (fxquotient (fx- finalecount 10) TEXTSPEED))]
             [i 0] [cx 10] [cy 10])
    (when (and (fx> count 0) (fx< i (string-length finaletext)))
      (let ([c (char->integer (string-ref finaletext i))])
        (if (fx= c 10)
            (loop (fx- count 1) (fx+ i 1) 10 (fx+ cy 11))
            (let ([c (fx- (toupper c) HU_FONTSTART)])
              ;; (C: c > HU_FONTSIZE, so '`' would read past hu_font)
              (if (or (fx< c 0) (fx> c HU_FONTSIZE))
                  (loop (fx- count 1) (fx+ i 1) (fx+ cx 4) cy)
                  (let ([w (patch-width (vector-ref hu_font c))])
                    (unless (fx> (fx+ cx w) SCREENWIDTH)
                      (v-draw-patch cx cy 0 (vector-ref hu_font c))
                      (loop (fx- count 1) (fx+ i 1) (fx+ cx w) cy))))))))))

;; Final DOOM 2 animation. Casting by id Software. in order of appearance
(define castorder
  (vector (cons CC_ZOMBIE MT_POSSESSED) (cons CC_SHOTGUN MT_SHOTGUY) (cons CC_HEAVY MT_CHAINGUY)
          (cons CC_IMP MT_TROOP) (cons CC_DEMON MT_SERGEANT) (cons CC_LOST MT_SKULL)
          (cons CC_CACO MT_HEAD) (cons CC_HELL MT_KNIGHT) (cons CC_BARON MT_BRUISER)
          (cons CC_ARACH MT_BABY) (cons CC_PAIN MT_PAIN) (cons CC_REVEN MT_UNDEAD)
          (cons CC_MANCU MT_FATSO) (cons CC_ARCH MT_VILE) (cons CC_SPIDER MT_SPIDER)
          (cons CC_CYBER MT_CYBORG) (cons CC_HERO MT_PLAYER) (cons #f 0)))

(define castnum 0)
(define casttics 0)
(define caststate #f)
(define castdeath #f)
(define castframes 0)
(define castonmelee 0)
(define castattacking #f)

(define (cast-info) (vector-ref mobjinfo-table (cdr (vector-ref castorder castnum))))

;; F_StartCast
(define (f-start-cast)
  (set! wipegamestate -1)               ; force a screen wipe
  (set! castnum 0)
  (set! caststate (vector-ref states (mobjinfo-seestate (cast-info))))
  (set! casttics (state-tics caststate))
  (set! castdeath #f)
  (set! finalestage 2)
  (set! castframes 0)
  (set! castonmelee 0)
  (set! castattacking #f)
  (s-change-music mus_evil #t))

;; F_CastTicker
(define (f-cast-ticker)
  (set! casttics (fx- casttics 1))
  (unless (fx> casttics 0)              ; not time to change state yet
    (call/1cc
     (lambda (stopattack)
       (define (stop-attack!)
         (set! castattacking #f)
         (set! castframes 0)
         (set! caststate (vector-ref states (mobjinfo-seestate (cast-info)))))
       (if (or (fx= (state-tics caststate) -1) (fx= (state-nextstate caststate) S_NULL))
           ;; switch from deathstate to next monster
           (begin
             (set! castnum (fx+ castnum 1))
             (set! castdeath #f)
             (unless (car (vector-ref castorder castnum))
               (set! castnum 0))
             (unless (fx= (mobjinfo-seesound (cast-info)) 0)
               (s-start-sound #f (mobjinfo-seesound (cast-info))))
             (set! caststate (vector-ref states (mobjinfo-seestate (cast-info))))
             (set! castframes 0))
           ;; just advance to next state in animation
           (if (eq? caststate (vector-ref states S_PLAY_ATK1))
               (begin (stop-attack!) (stopattack (void))) ; Oh, gross hack!
               (let ([st (state-nextstate caststate)])
                 (set! caststate (vector-ref states st))
                 (set! castframes (fx+ castframes 1))
                 ;; sound hacks....
                 (let ([sfx (cond
                              [(fx= st S_PLAY_ATK1) sfx_dshtgn]
                              [(fx= st S_POSS_ATK2) sfx_pistol]
                              [(fx= st S_SPOS_ATK2) sfx_shotgn]
                              [(fx= st S_VILE_ATK2) sfx_vilatk]
                              [(fx= st S_SKEL_FIST2) sfx_skeswg]
                              [(fx= st S_SKEL_FIST4) sfx_skepch]
                              [(fx= st S_SKEL_MISS2) sfx_skeatk]
                              [(memv st (list S_FATT_ATK8 S_FATT_ATK5 S_FATT_ATK2)) sfx_firsht]
                              [(memv st (list S_CPOS_ATK2 S_CPOS_ATK3 S_CPOS_ATK4)) sfx_shotgn]
                              [(fx= st S_TROO_ATK3) sfx_claw]
                              [(fx= st S_SARG_ATK2) sfx_sgtatk]
                              [(memv st (list S_BOSS_ATK2 S_BOS2_ATK2 S_HEAD_ATK2)) sfx_firsht]
                              [(fx= st S_SKULL_ATK2) sfx_sklatk]
                              [(memv st (list S_SPID_ATK2 S_SPID_ATK3)) sfx_shotgn]
                              [(fx= st S_BSPI_ATK2) sfx_plasma]
                              [(memv st (list S_CYBER_ATK2 S_CYBER_ATK4 S_CYBER_ATK6)) sfx_rlaunc]
                              [(fx= st S_PAIN_ATK3) sfx_sklatk]
                              [else 0])])
                   (unless (fx= sfx 0)
                     (s-start-sound #f sfx))))))
       (when (fx= castframes 12)
         ;; go into attack frame
         (set! castattacking #t)
         (set! caststate (vector-ref states (if (fx= castonmelee 0)
                                                (mobjinfo-missilestate (cast-info))
                                                (mobjinfo-meleestate (cast-info)))))
         (set! castonmelee (fxxor castonmelee 1))
         (when (eq? caststate (vector-ref states S_NULL))
           (set! caststate (vector-ref states (if (fx= castonmelee 0)
                                                  (mobjinfo-missilestate (cast-info))
                                                  (mobjinfo-meleestate (cast-info)))))))
       (when castattacking
         (when (or (fx= castframes 24)
                   (eq? caststate (vector-ref states (mobjinfo-seestate (cast-info)))))
           (stop-attack!)))))
    (set! casttics (state-tics caststate))
    (when (fx= casttics -1)
      (set! casttics 15))))

;; F_CastResponder
(define (f-cast-responder ev)
  (cond
    [(not (fx= (event-type ev) ev_keydown)) #f]
    [castdeath #t]                      ; already in dying frames
    [else
     ;; go into death frame
     (set! castdeath #t)
     (set! caststate (vector-ref states (mobjinfo-deathstate (cast-info))))
     (set! casttics (state-tics caststate))
     (set! castframes 0)
     (set! castattacking #f)
     (unless (fx= (mobjinfo-deathsound (cast-info)) 0)
       (s-start-sound #f (mobjinfo-deathsound (cast-info))))
     #t]))

(define (f-cast-print text)
  (define (glyph c) (fx- (toupper (char->integer c)) HU_FONTSTART))
  ;; find width
  (let ([width (let loop ([i 0] [width 0])
                 (if (fx= i (string-length text))
                     width
                     (let ([c (glyph (string-ref text i))])
                       (loop (fx+ i 1)
                             (if (or (fx< c 0) (fx> c HU_FONTSIZE))
                                 (fx+ width 4)
                                 (fx+ width (patch-width (vector-ref hu_font c))))))))])
    ;; draw it
    (let loop ([i 0] [cx (fx- 160 (fxquotient width 2))])
      (when (fx< i (string-length text))
        (let ([c (glyph (string-ref text i))])
          (if (or (fx< c 0) (fx> c HU_FONTSIZE))
              (loop (fx+ i 1) (fx+ cx 4))
              (begin
                (v-draw-patch cx 180 0 (vector-ref hu_font c))
                (loop (fx+ i 1) (fx+ cx (patch-width (vector-ref hu_font c)))))))))))

;; F_CastDrawer
(define (f-cast-drawer)
  ;; erase the entire screen to a background
  (v-draw-patch 0 0 0 (w-cache-lump-name "BOSSBACK"))
  (f-cast-print (car (vector-ref castorder castnum)))
  ;; draw the current frame in the middle of the screen
  (let* ([sprdef (vector-ref sprites (state-sprite caststate))]
         [sprframe (vector-ref (spritedef-spriteframes sprdef) (fxand (state-frame caststate) FF_FRAMEMASK))]
         [lump (fxvector-ref (spriteframe-lump sprframe) 0)]
         [flip (not (fx= (fxand (fxvector-ref (spriteframe-flip sprframe) 0) #xFF) 0))]
         [patch (w-cache-lump-num (fx+ lump firstspritelump))])
    (if flip
        (v-draw-patch-flipped 160 170 0 patch)
        (v-draw-patch 160 170 0 patch))))

;; F_DrawPatchCol
(define (f-draw-patch-col x patch col)
  ;; step through the posts in a column
  (let post ([c (patch-columnofs patch col)])
    (let ([topdelta (bytevector-u8-ref patch c)])
      (unless (fx= topdelta #xff)
        (let ([len (bytevector-u8-ref patch (fx+ c 1))])
          (do ([i 0 (fx+ i 1)]
               [dest (fx+ x (fx* topdelta SCREENWIDTH)) (fx+ dest SCREENWIDTH)])
              ((fx= i len))
            (bytevector-u8-set! screens dest (bytevector-u8-ref patch (fx+ c 3 i))))
          (post (fx+ c len 4)))))))

;; F_BunnyScroll
(define laststage 0)
(define (f-bunny-scroll)
  (let ([p1 (w-cache-lump-name "PFUB2")]
        [p2 (w-cache-lump-name "PFUB1")]
        [scrolled (fxmax 0 (fxmin 320 (fx- 320 (fxquotient (fx- finalecount 230) 2))))])
    (v-mark-rect 0 0 SCREENWIDTH SCREENHEIGHT)
    (do ([x 0 (fx+ x 1)]) ((fx= x SCREENWIDTH))
      (if (fx< (fx+ x scrolled) 320)
          (f-draw-patch-col x p1 (fx+ x scrolled))
          (f-draw-patch-col x p2 (fx- (fx+ x scrolled) 320))))
    (cond
      [(fx< finalecount 1130) (void)]
      [(fx< finalecount 1180)
       (v-draw-patch (fxquotient (fx- SCREENWIDTH (* 13 8)) 2) (fxquotient (fx- SCREENHEIGHT (* 8 8)) 2) 0
                     (w-cache-lump-name "END0"))
       (set! laststage 0)]
      [else
       (let ([stage (fxmin 6 (fxquotient (fx- finalecount 1180) 5))])
         (when (fx> stage laststage)
           (s-start-sound #f sfx_pistol)
           (set! laststage stage))
         (v-draw-patch (fxquotient (fx- SCREENWIDTH (* 13 8)) 2) (fxquotient (fx- SCREENHEIGHT (* 8 8)) 2) 0
                       (w-cache-lump-name (format "END~a" stage))))])))

;; F_Drawer
(define (f-drawer)
  (cond
    [(fx= finalestage 2) (f-cast-drawer)]
    [(fx= finalestage 0) (f-text-write)]
    [else
     (case gameepisode
       [(1) (v-draw-patch 0 0 0 (w-cache-lump-name (if (fx= gamemode retail) "CREDIT" "HELP2")))]
       [(2) (v-draw-patch 0 0 0 (w-cache-lump-name "VICTORY2"))]
       [(3) (f-bunny-scroll)]
       [(4) (v-draw-patch 0 0 0 (w-cache-lump-name "ENDPIC"))])]))
