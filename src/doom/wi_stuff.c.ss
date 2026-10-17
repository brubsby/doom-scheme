;;; wi_stuff.c: the intermission screens: level stats, and the episode map
;;; with "you are here". Included by d_main.sls.

(define NUMEPISODES 4)
(define NUMMAPS 9)

;; GLOBAL LOCATIONS
(define WI_TITLEY 2)
(define WI_SPACINGY 33)
;; SINGPLE-PLAYER STUFF
(define SP_STATSX 50)
(define SP_STATSY 50)
(define SP_TIMEX 16)
(define SP_TIMEY (fx- SCREENHEIGHT 32))
;; NET GAME STUFF
(define NG_STATSY 50)
(define (NG_STATSX) (fx+ 32 (fxquotient (patch-width star) 2) (if (fx= dofrags 0) 32 0)))
(define NG_SPACINGX 64)
;; DEATHMATCH STUFF
(define DM_MATRIXX 42)
(define DM_MATRIXY 68)
(define DM_SPACINGX 40)
(define DM_TOTALSX 269)
(define DM_KILLERSX 10)
(define DM_KILLERSY 100)
(define DM_VICTIMSX 5)
(define DM_VICTIMSY 50)

(define ANIM_ALWAYS 0)
(define ANIM_RANDOM 1)
(define ANIM_LEVEL 2)

;; anim_t: type, period in tics between animations, number of animation
;; frames, location, data1 (RANDOM: period deviation (<256), LEVEL: level),
;; data2 (RANDOM: random base period), the frames' graphics, and, zeroed
;; before use: next value of bcnt, last drawn frame, next frame number,
;; state.
(define-record-type anim
  (fields (immutable type) (immutable period) (immutable nanims) (immutable x) (immutable y)
          (immutable data1) (immutable data2) (immutable p)
          (mutable nexttic) (mutable lastdrawn) (mutable ctr) (mutable state)))
(define (mk-anim type period nanims x y . data)
  (make-anim type period nanims x y (if (pair? data) (car data) 0) 0 (make-vector 3 #f) 0 0 0 0))

(define lnodes
  (vector
   ;; Episode 0 World Map
   (vector '(185 . 164) '(148 . 143) '(69 . 122) '(209 . 102) '(116 . 89) '(166 . 55)
           '(71 . 56) '(135 . 29) '(71 . 24))
   ;; Episode 1 World Map should go here
   (vector '(254 . 25) '(97 . 50) '(188 . 64) '(128 . 78) '(214 . 92) '(133 . 130)
           '(208 . 136) '(148 . 140) '(235 . 158))
   ;; Episode 2 World Map should go here
   (vector '(156 . 168) '(48 . 154) '(174 . 95) '(265 . 75) '(130 . 48) '(279 . 23)
           '(198 . 48) '(140 . 25) '(281 . 136))))

;; Animation locations for episode 0 (1). Using patches saves a lot of
;; space, as they replace 320x200 full screen frames.
(define T3 (fxquotient TICRATE 3))
(define epsd0animinfo
  (vector (mk-anim ANIM_ALWAYS T3 3 224 104) (mk-anim ANIM_ALWAYS T3 3 184 160)
          (mk-anim ANIM_ALWAYS T3 3 112 136) (mk-anim ANIM_ALWAYS T3 3 72 112)
          (mk-anim ANIM_ALWAYS T3 3 88 96) (mk-anim ANIM_ALWAYS T3 3 64 48)
          (mk-anim ANIM_ALWAYS T3 3 192 40) (mk-anim ANIM_ALWAYS T3 3 136 16)
          (mk-anim ANIM_ALWAYS T3 3 80 16) (mk-anim ANIM_ALWAYS T3 3 64 24)))
(define epsd1animinfo
  (vector (mk-anim ANIM_LEVEL T3 1 128 136 1) (mk-anim ANIM_LEVEL T3 1 128 136 2)
          (mk-anim ANIM_LEVEL T3 1 128 136 3) (mk-anim ANIM_LEVEL T3 1 128 136 4)
          (mk-anim ANIM_LEVEL T3 1 128 136 5) (mk-anim ANIM_LEVEL T3 1 128 136 6)
          (mk-anim ANIM_LEVEL T3 1 128 136 7) (mk-anim ANIM_LEVEL T3 3 192 144 8)
          (mk-anim ANIM_LEVEL T3 1 128 136 8)))
(define epsd2animinfo
  (vector (mk-anim ANIM_ALWAYS T3 3 104 168) (mk-anim ANIM_ALWAYS T3 3 40 136)
          (mk-anim ANIM_ALWAYS T3 3 160 96) (mk-anim ANIM_ALWAYS T3 3 104 80)
          (mk-anim ANIM_ALWAYS T3 3 120 32) (mk-anim ANIM_ALWAYS (fxquotient TICRATE 4) 3 40 0)))
(define anims (vector epsd0animinfo epsd1animinfo epsd2animinfo (vector)))
(define (NUMANIMS e) (vector-length (vector-ref anims e)))

;; States for single-player
(define SHOWNEXTLOCDELAY 4)             ; in seconds

;; stateenum_t
(define NoState -1)
(define StatCount 0)
(define ShowNextLoc 1)

(define acceleratestage 0)              ; used to accelerate or skip a stage
(define me 0)                           ; wbs->pnum
(define wi-state NoState)                  ; specifies current state
(define wbs #f)                         ; contains information passed into intermission
(define plrs #f)                        ; wbs->plyr[]
(define cnt 0)                          ; used for general timing
(define bcnt 0)                         ; used for timing of background animation
(define firstrefresh 0)                 ; signals to refresh everything for one frame
(define cnt_kills (make-fxvector MAXPLAYERS 0))
(define cnt_items (make-fxvector MAXPLAYERS 0))
(define cnt_secret (make-fxvector MAXPLAYERS 0))
(define cnt_time 0)
(define cnt_par 0)
(define cnt_pause 0)
(define NUMCMAPS 0)                     ; # of commercial levels

;; GRAPHICS
(define bg #f)                          ; background (map of levels).
(define yah (vector #f #f))             ; You Are Here graphic
(define splat #f)                       ; splat
(define percent #f)                     ; %, : graphics
(define colon #f)
(define num (make-vector 10 #f))        ; 0-9 graphic
(define wiminus #f)                     ; minus sign
(define finished #f)                    ; "Finished!" graphics
(define entering #f)                    ; "Entering" graphic
(define sp_secret #f)                   ; "secret"
(define kills #f)                       ; "Kills", "Scrt", "Items", "Frags"
(define secret #f)
(define items #f)
(define frags #f)
(define time-patch #f)                  ; Time sucks.
(define par #f)
(define sucks #f)
(define killers #f)                     ; "killers", "victims"
(define victims #f)
(define total #f)                       ; "Total", your face, your dead face
(define star #f)
(define bstar #f)
(define wi-p (make-vector MAXPLAYERS #f)) ; "red P[1..MAXPLAYERS]"
(define bp (make-vector MAXPLAYERS #f)) ; "gray P[1..MAXPLAYERS]"
(define lnames (vector))                ; Name graphics of each level (centered)

(define (plr-field f i) (f (vector-ref plrs i)))

;; CODE

;; slam background
(define (wi-slam-background)
  (bytevector-copy! screens (screen-base 1) screens (screen-base 0) (fx* SCREENWIDTH SCREENHEIGHT))
  (v-mark-rect 0 0 SCREENWIDTH SCREENHEIGHT))

;; The ticker is used to detect keys because of timing issues in netgames.
(define (wi-responder ev) #f)

;; Draws "<Levelname> Finished!"
(define (wi-draw-lf)
  (let ([lname (vector-ref lnames (wbstartstruct-last wbs))])
    ;; draw <LevelName>
    (v-draw-patch (fxquotient (fx- SCREENWIDTH (patch-width lname)) 2) WI_TITLEY 0 lname)
    ;; draw "Finished!"
    (v-draw-patch (fxquotient (fx- SCREENWIDTH (patch-width finished)) 2)
                  (fx+ WI_TITLEY (fxquotient (fx* 5 (patch-height lname)) 4)) 0 finished)))

;; Draws "Entering <LevelName>"
(define (wi-draw-el)
  (let ([lname (vector-ref lnames (wbstartstruct-next wbs))])
    ;; draw "Entering"
    (v-draw-patch (fxquotient (fx- SCREENWIDTH (patch-width entering)) 2) WI_TITLEY 0 entering)
    ;; draw level
    (v-draw-patch (fxquotient (fx- SCREENWIDTH (patch-width lname)) 2)
                  (fx+ WI_TITLEY (fxquotient (fx* 5 (patch-height lname)) 4)) 0 lname)))

;; c: the patches to try in turn (C reads two, whatever follows the one
;; splat; here a missing second one just doesn't fit).
(define (wi-draw-on-lnode n c)
  (let* ([node (vector-ref (vector-ref lnodes (wbstartstruct-epsd wbs)) n)]
         [nx (car node)] [ny (cdr node)])
    (let loop ([i 0])
      (if (and (fx< i 2) (fx< i (vector-length c)))
          (let* ([p (vector-ref c i)]
                 [left (fx- nx (patch-leftoffset p))]
                 [top (fx- ny (patch-topoffset p))]
                 [right (fx+ left (patch-width p))]
                 [bottom (fx+ top (patch-height p))])
            (if (and (fx>= left 0) (fx< right SCREENWIDTH) (fx>= top 0) (fx< bottom SCREENHEIGHT))
                (v-draw-patch nx ny 0 p)
                (loop (fx+ i 1))))
          ;; DEBUG
          (printf "Could not place patch on level ~a" (fx+ n 1))))))

(define (wi-init-animated-back)
  (unless (or (fx= gamemode commercial) (fx> (wbstartstruct-epsd wbs) 2))
    (vector-for-each
     (lambda (a)
       ;; init variables
       (anim-ctr-set! a -1)
       ;; specify the next time to draw it
       (cond
         [(fx= (anim-type a) ANIM_ALWAYS)
          (anim-nexttic-set! a (fx+ bcnt 1 (fxremainder (m-random) (anim-period a))))]
         [(fx= (anim-type a) ANIM_RANDOM)
          (anim-nexttic-set! a (fx+ bcnt 1 (anim-data2 a) (fxremainder (m-random) (anim-data1 a))))]
         [(fx= (anim-type a) ANIM_LEVEL)
          (anim-nexttic-set! a (fx+ bcnt 1))]))
     (vector-ref anims (wbstartstruct-epsd wbs)))))

(define (wi-update-animated-back)
  (unless (or (fx= gamemode commercial) (fx> (wbstartstruct-epsd wbs) 2))
    (let ([as (vector-ref anims (wbstartstruct-epsd wbs))])
      (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length as)))
        (let ([a (vector-ref as i)])
          (when (fx= bcnt (anim-nexttic a))
            (cond
              [(fx= (anim-type a) ANIM_ALWAYS)
               (anim-ctr-set! a (fx+ (anim-ctr a) 1))
               (when (fx>= (anim-ctr a) (anim-nanims a)) (anim-ctr-set! a 0))
               (anim-nexttic-set! a (fx+ bcnt (anim-period a)))]
              [(fx= (anim-type a) ANIM_RANDOM)
               (anim-ctr-set! a (fx+ (anim-ctr a) 1))
               (if (fx= (anim-ctr a) (anim-nanims a))
                   (begin
                     (anim-ctr-set! a -1)
                     (anim-nexttic-set! a (fx+ bcnt (anim-data2 a) (fxremainder (m-random) (anim-data1 a)))))
                   (anim-nexttic-set! a (fx+ bcnt (anim-period a))))]
              [(fx= (anim-type a) ANIM_LEVEL)
               ;; gawd-awful hack for level anims
               (when (and (not (and (fx= wi-state StatCount) (fx= i 7)))
                          (fx= (wbstartstruct-next wbs) (anim-data1 a)))
                 (anim-ctr-set! a (fx+ (anim-ctr a) 1))
                 (when (fx= (anim-ctr a) (anim-nanims a))
                   (anim-ctr-set! a (fx- (anim-ctr a) 1)))
                 (anim-nexttic-set! a (fx+ bcnt (anim-period a))))])))))))

;; WI_drawAnimatedBack. C tests `if (commercial)`, the enum constant (2),
;; so linuxdoom never draws the animations (they still run, and their
;; M_Random calls happen).
(define (wi-draw-animated-back)
  (void))

;; Draws a number. If digits > 0, then use that many digits minimum,
;; otherwise only use as many as necessary. Returns new x position.
(define (wi-draw-num x y n digits)
  (let* ([fontwidth (patch-width (vector-ref num 0))]
         [digits (cond
                   [(fx>= digits 0) digits]
                   ;; make variable-length zeros 1 digit long
                   [(fx= n 0) 1]
                   ;; figure out # of digits in #
                   [else (let loop ([temp n] [d 0])
                           (if (fx= temp 0) d (loop (fxquotient temp 10) (fx+ d 1))))])]
         [neg (fx< n 0)]
         [n (fxabs n)])
    ;; if non-number, do not draw it
    (if (fx= n 1994)
        0
        ;; draw the new number
        (let loop ([digits digits] [n n] [x x])
          (if (fx> digits 0)
              (let ([x (fx- x fontwidth)])
                (v-draw-patch x y 0 (vector-ref num (fxremainder n 10)))
                (loop (fx- digits 1) (fxquotient n 10) x))
              ;; draw a minus sign if necessary
              (if neg
                  (let ([x (fx- x 8)])
                    (v-draw-patch x y 0 wiminus)
                    x)
                  x))))))

(define (wi-draw-percent x y p)
  (unless (fx< p 0)
    (v-draw-patch x y 0 percent)
    (wi-draw-num x y p -1)))

;; Display level completion time and par, or "sucks" message if overflow.
(define (wi-draw-time x y t)
  (unless (fx< t 0)
    (if (fx<= t (* 61 59))
        (let loop ([div 1] [x x])
          (let* ([n (fxremainder (fxquotient t div) 60)]
                 [x (fx- (wi-draw-num x y n 2) (patch-width colon))]
                 [div (fx* div 60)])
            ;; draw
            (when (or (fx= div 60) (not (fx= (fxquotient t div) 0)))
              (v-draw-patch x y 0 colon))
            (unless (fx= (fxquotient t div) 0)
              (loop div x))))
        ;; "sucks"
        (v-draw-patch (fx- x (patch-width sucks)) y 0 sucks))))

(define (wi-end)
  (void))                               ; (WI_unloadData)

(define (wi-init-no-state)
  (set! wi-state NoState)
  (set! acceleratestage 0)
  (set! cnt 10))

(define (wi-update-no-state)
  (wi-update-animated-back)
  (set! cnt (fx- cnt 1))
  (when (fx= cnt 0)
    (wi-end)
    (g-world-done)))

(define snl_pointeron #f)

(define (wi-init-show-next-loc)
  (set! wi-state ShowNextLoc)
  (set! acceleratestage 0)
  (set! cnt (* SHOWNEXTLOCDELAY TICRATE))
  (wi-init-animated-back))

(define (wi-update-show-next-loc)
  (wi-update-animated-back)
  (set! cnt (fx- cnt 1))
  (if (or (fx= cnt 0) (not (fx= acceleratestage 0)))
      (wi-init-no-state)
      (set! snl_pointeron (fx< (fxand cnt 31) 20))))

(define (wi-draw-show-next-loc)
  (wi-slam-background)
  ;; draw animated background
  (wi-draw-animated-back)
  (call/1cc
   (lambda (return)
     (unless (fx= gamemode commercial)
       (when (fx> (wbstartstruct-epsd wbs) 2)
         (wi-draw-el)
         (return (void)))
       (let ([last (if (fx= (wbstartstruct-last wbs) 8)
                       (fx- (wbstartstruct-next wbs) 1)
                       (wbstartstruct-last wbs))])
         ;; draw a splat on taken cities.
         (do ([i 0 (fx+ i 1)]) ((fx> i last))
           (wi-draw-on-lnode i (vector splat)))
         ;; splat the secret level?
         (when (wbstartstruct-didsecret wbs)
           (wi-draw-on-lnode 8 (vector splat)))
         ;; draw flashing ptr
         (when snl_pointeron
           (wi-draw-on-lnode (wbstartstruct-next wbs) yah))))
     ;; draws which level you are entering..
     (when (or (not (fx= gamemode commercial)) (not (fx= (wbstartstruct-next wbs) 30)))
       (wi-draw-el)))))

(define (wi-draw-no-state)
  (set! snl_pointeron #t)
  (wi-draw-show-next-loc))

(define (wi-frag-sum playernum)
  (let loop ([i 0] [frags 0])
    (if (fx= i MAXPLAYERS)
        ;; JDC hack - negative frags.
        (fx- frags (fxvector-ref (plr-field wbplayerstruct-frags playernum) playernum))
        (loop (fx+ i 1)
              (if (and (vector-ref playeringame i) (not (fx= i playernum)))
                  (fx+ frags (fxvector-ref (plr-field wbplayerstruct-frags playernum) i))
                  frags)))))

(define dm_state 0)
(define dm_frags (let ([v (make-vector MAXPLAYERS)])
                   (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS) v)
                     (vector-set! v i (make-fxvector MAXPLAYERS 0)))))
(define dm_totals (make-fxvector MAXPLAYERS 0))
(define (dm-frag i j) (fxvector-ref (vector-ref dm_frags i) j))
(define (dm-frag-set! i j v) (fxvector-set! (vector-ref dm_frags i) j v))

(define (wi-init-deathmatch-stats)
  (set! wi-state StatCount)
  (set! acceleratestage 0)
  (set! dm_state 1)
  (set! cnt_pause TICRATE)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (do ([j 0 (fx+ j 1)]) ((fx= j MAXPLAYERS))
        (when (vector-ref playeringame j)
          (dm-frag-set! i j 0)))
      (fxvector-set! dm_totals i 0)))
  (wi-init-animated-back))

(define (clamp99 v) (fxmax -99 (fxmin 99 v)))

(define (wi-update-deathmatch-stats)
  (wi-update-animated-back)
  (when (and (not (fx= acceleratestage 0)) (not (fx= dm_state 4)))
    (set! acceleratestage 0)
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (do ([j 0 (fx+ j 1)]) ((fx= j MAXPLAYERS))
          (when (vector-ref playeringame j)
            (dm-frag-set! i j (fxvector-ref (plr-field wbplayerstruct-frags i) j))))
        (fxvector-set! dm_totals i (wi-frag-sum i))))
    (s-start-sound #f sfx_barexp)
    (set! dm_state 4))
  (cond
    [(fx= dm_state 2)
     (when (fx= (fxand bcnt 3) 0)
       (s-start-sound #f sfx_pistol))
     (let ([stillticking #f])
       (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
         (when (vector-ref playeringame i)
           (do ([j 0 (fx+ j 1)]) ((fx= j MAXPLAYERS))
             (let ([target (fxvector-ref (plr-field wbplayerstruct-frags i) j)])
               (when (and (vector-ref playeringame j) (not (fx= (dm-frag i j) target)))
                 (dm-frag-set! i j (clamp99 (if (fx< target 0) (fx- (dm-frag i j) 1) (fx+ (dm-frag i j) 1))))
                 (set! stillticking #t))))
           (fxvector-set! dm_totals i (clamp99 (wi-frag-sum i)))))
       (unless stillticking
         (s-start-sound #f sfx_barexp)
         (set! dm_state (fx+ dm_state 1))))]
    [(fx= dm_state 4)
     (unless (fx= acceleratestage 0)
       (s-start-sound #f sfx_slop)
       (if (fx= gamemode commercial)
           (wi-init-no-state)
           (wi-init-show-next-loc)))]
    [(fxodd? dm_state)
     (set! cnt_pause (fx- cnt_pause 1))
     (when (fx= cnt_pause 0)
       (set! dm_state (fx+ dm_state 1))
       (set! cnt_pause TICRATE))]))

(define (wi-draw-deathmatch-stats)
  (wi-slam-background)
  ;; draw animated background
  (wi-draw-animated-back)
  (wi-draw-lf)
  ;; draw stat titles (top line)
  (v-draw-patch (fx- DM_TOTALSX (fxquotient (patch-width total) 2)) (fx+ (fx- DM_MATRIXY WI_SPACINGY) 10) 0 total)
  (v-draw-patch DM_KILLERSX DM_KILLERSY 0 killers)
  (v-draw-patch DM_VICTIMSX DM_VICTIMSY 0 victims)
  ;; draw P?
  (do ([i 0 (fx+ i 1)]
       [x (fx+ DM_MATRIXX DM_SPACINGX) (fx+ x DM_SPACINGX)]
       [y DM_MATRIXY (fx+ y WI_SPACINGY)])
      ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (let ([pw (fxquotient (patch-width (vector-ref wi-p i)) 2)])
        (v-draw-patch (fx- x pw) (fx- DM_MATRIXY WI_SPACINGY) 0 (vector-ref wi-p i))
        (v-draw-patch (fx- DM_MATRIXX pw) y 0 (vector-ref wi-p i))
        (when (fx= i me)
          (v-draw-patch (fx- x pw) (fx- DM_MATRIXY WI_SPACINGY) 0 bstar)
          (v-draw-patch (fx- DM_MATRIXX pw) y 0 star)))))
  ;; draw stats
  (let ([w (patch-width (vector-ref num 0))])
    (do ([i 0 (fx+ i 1)]
         [y (fx+ DM_MATRIXY 10) (fx+ y WI_SPACINGY)])
        ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (do ([j 0 (fx+ j 1)]
             [x (fx+ DM_MATRIXX DM_SPACINGX) (fx+ x DM_SPACINGX)])
            ((fx= j MAXPLAYERS))
          (when (vector-ref playeringame j)
            (wi-draw-num (fx+ x w) y (dm-frag i j) 2)))
        (wi-draw-num (fx+ DM_TOTALSX w) y (fxvector-ref dm_totals i) 2)))))

(define cnt_frags (make-fxvector MAXPLAYERS 0))
(define dofrags 0)
(define ng_state 0)

(define (wi-init-netgame-stats)
  (set! wi-state StatCount)
  (set! acceleratestage 0)
  (set! ng_state 1)
  (set! cnt_pause TICRATE)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (fxvector-set! cnt_kills i 0)
      (fxvector-set! cnt_items i 0)
      (fxvector-set! cnt_secret i 0)
      (fxvector-set! cnt_frags i 0)
      (set! dofrags (fx+ dofrags (wi-frag-sum i)))))
  (set! dofrags (if (fx= dofrags 0) 0 1))
  (wi-init-animated-back))

(define (pct field i maxf)
  (fxquotient (fx* (plr-field field i) 100) (maxf wbs)))

(define (wi-update-netgame-stats)
  (define (count-up! counts field maxf)
    (when (fx= (fxand bcnt 3) 0)
      (s-start-sound #f sfx_pistol))
    (let ([stillticking #f])
      (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
        (when (vector-ref playeringame i)
          (fxvector-set! counts i (fx+ (fxvector-ref counts i) 2))
          (let ([target (pct field i maxf)])
            (if (fx>= (fxvector-ref counts i) target)
                (fxvector-set! counts i target)
                (set! stillticking #t)))))
      stillticking))
  (wi-update-animated-back)
  (when (and (not (fx= acceleratestage 0)) (not (fx= ng_state 10)))
    (set! acceleratestage 0)
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (fxvector-set! cnt_kills i (pct wbplayerstruct-skills i wbstartstruct-maxkills))
        (fxvector-set! cnt_items i (pct wbplayerstruct-sitems i wbstartstruct-maxitems))
        (fxvector-set! cnt_secret i (pct wbplayerstruct-ssecret i wbstartstruct-maxsecret))
        (unless (fx= dofrags 0)
          (fxvector-set! cnt_frags i (wi-frag-sum i)))))
    (s-start-sound #f sfx_barexp)
    (set! ng_state 10))
  (cond
    [(fx= ng_state 2)
     (unless (count-up! cnt_kills wbplayerstruct-skills wbstartstruct-maxkills)
       (s-start-sound #f sfx_barexp)
       (set! ng_state (fx+ ng_state 1)))]
    [(fx= ng_state 4)
     (unless (count-up! cnt_items wbplayerstruct-sitems wbstartstruct-maxitems)
       (s-start-sound #f sfx_barexp)
       (set! ng_state (fx+ ng_state 1)))]
    [(fx= ng_state 6)
     (unless (count-up! cnt_secret wbplayerstruct-ssecret wbstartstruct-maxsecret)
       (s-start-sound #f sfx_barexp)
       (set! ng_state (fx+ ng_state 1 (if (fx= dofrags 0) 2 0))))]
    [(fx= ng_state 8)
     (when (fx= (fxand bcnt 3) 0)
       (s-start-sound #f sfx_pistol))
     (let ([stillticking #f])
       (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
         (when (vector-ref playeringame i)
           (fxvector-set! cnt_frags i (fx+ (fxvector-ref cnt_frags i) 1))
           (let ([fsum (wi-frag-sum i)])
             (if (fx>= (fxvector-ref cnt_frags i) fsum)
                 (fxvector-set! cnt_frags i fsum)
                 (set! stillticking #t)))))
       (unless stillticking
         (s-start-sound #f sfx_pldeth)
         (set! ng_state (fx+ ng_state 1))))]
    [(fx= ng_state 10)
     (unless (fx= acceleratestage 0)
       (s-start-sound #f sfx_sgcock)
       (if (fx= gamemode commercial)
           (wi-init-no-state)
           (wi-init-show-next-loc)))]
    [(fxodd? ng_state)
     (set! cnt_pause (fx- cnt_pause 1))
     (when (fx= cnt_pause 0)
       (set! ng_state (fx+ ng_state 1))
       (set! cnt_pause TICRATE))]))

(define (wi-draw-netgame-stats)
  (let ([pwidth (patch-width percent)]
        [ngx (NG_STATSX)])
    (wi-slam-background)
    ;; draw animated background
    (wi-draw-animated-back)
    (wi-draw-lf)
    ;; draw stat titles (top line)
    (v-draw-patch (fx- (fx+ ngx NG_SPACINGX) (patch-width kills)) NG_STATSY 0 kills)
    (v-draw-patch (fx- (fx+ ngx (fx* 2 NG_SPACINGX)) (patch-width items)) NG_STATSY 0 items)
    (v-draw-patch (fx- (fx+ ngx (fx* 3 NG_SPACINGX)) (patch-width secret)) NG_STATSY 0 secret)
    (unless (fx= dofrags 0)
      (v-draw-patch (fx- (fx+ ngx (fx* 4 NG_SPACINGX)) (patch-width frags)) NG_STATSY 0 frags))
    ;; draw stats
    (let loop ([i 0] [y (fx+ NG_STATSY (patch-height kills))])
      (when (fx< i MAXPLAYERS)
        (if (not (vector-ref playeringame i))
            (loop (fx+ i 1) y)
            (let ([x ngx] [pi (vector-ref wi-p i)])
              (v-draw-patch (fx- x (patch-width pi)) y 0 pi)
              (when (fx= i me)
                (v-draw-patch (fx- x (patch-width pi)) y 0 star))
              (let* ([x (fx+ x NG_SPACINGX)]
                     [_ (wi-draw-percent (fx- x pwidth) (fx+ y 10) (fxvector-ref cnt_kills i))]
                     [x (fx+ x NG_SPACINGX)]
                     [_ (wi-draw-percent (fx- x pwidth) (fx+ y 10) (fxvector-ref cnt_items i))]
                     [x (fx+ x NG_SPACINGX)]
                     [_ (wi-draw-percent (fx- x pwidth) (fx+ y 10) (fxvector-ref cnt_secret i))]
                     [x (fx+ x NG_SPACINGX)])
                (unless (fx= dofrags 0)
                  (wi-draw-num x (fx+ y 10) (fxvector-ref cnt_frags i) -1)))
              (loop (fx+ i 1) (fx+ y WI_SPACINGY))))))))

(define sp_state 0)

(define (wi-init-stats)
  (set! wi-state StatCount)
  (set! acceleratestage 0)
  (set! sp_state 1)
  (fxvector-set! cnt_kills 0 -1)
  (fxvector-set! cnt_items 0 -1)
  (fxvector-set! cnt_secret 0 -1)
  (set! cnt_time -1)
  (set! cnt_par -1)
  (set! cnt_pause TICRATE)
  (wi-init-animated-back))

(define (wi-update-stats)
  (wi-update-animated-back)
  (let ([kills% (lambda () (pct wbplayerstruct-skills me wbstartstruct-maxkills))]
        [items% (lambda () (pct wbplayerstruct-sitems me wbstartstruct-maxitems))]
        [secret% (lambda () (pct wbplayerstruct-ssecret me wbstartstruct-maxsecret))]
        [stime (lambda () (fxquotient (plr-field wbplayerstruct-stime me) TICRATE))]
        [partime (lambda () (fxquotient (wbstartstruct-partime wbs) TICRATE))])
    (define (count-up! counts target)
      (fxvector-set! counts 0 (fx+ (fxvector-ref counts 0) 2))
      (when (fx= (fxand bcnt 3) 0)
        (s-start-sound #f sfx_pistol))
      (when (fx>= (fxvector-ref counts 0) (target))
        (fxvector-set! counts 0 (target))
        (s-start-sound #f sfx_barexp)
        (set! sp_state (fx+ sp_state 1))))
    (when (and (not (fx= acceleratestage 0)) (not (fx= sp_state 10)))
      (set! acceleratestage 0)
      (fxvector-set! cnt_kills 0 (kills%))
      (fxvector-set! cnt_items 0 (items%))
      (fxvector-set! cnt_secret 0 (secret%))
      (set! cnt_time (stime))
      (set! cnt_par (partime))
      (s-start-sound #f sfx_barexp)
      (set! sp_state 10))
    (cond
      [(fx= sp_state 2) (count-up! cnt_kills kills%)]
      [(fx= sp_state 4) (count-up! cnt_items items%)]
      [(fx= sp_state 6) (count-up! cnt_secret secret%)]
      [(fx= sp_state 8)
       (when (fx= (fxand bcnt 3) 0)
         (s-start-sound #f sfx_pistol))
       (set! cnt_time (fx+ cnt_time 3))
       (when (fx>= cnt_time (stime))
         (set! cnt_time (stime)))
       (set! cnt_par (fx+ cnt_par 3))
       (when (fx>= cnt_par (partime))
         (set! cnt_par (partime))
         (when (fx>= cnt_time (stime))
           (s-start-sound #f sfx_barexp)
           (set! sp_state (fx+ sp_state 1))))]
      [(fx= sp_state 10)
       (unless (fx= acceleratestage 0)
         (s-start-sound #f sfx_sgcock)
         (if (fx= gamemode commercial)
             (wi-init-no-state)
             (wi-init-show-next-loc)))]
      [(fxodd? sp_state)
       (set! cnt_pause (fx- cnt_pause 1))
       (when (fx= cnt_pause 0)
         (set! sp_state (fx+ sp_state 1))
         (set! cnt_pause TICRATE))])))

(define (wi-draw-stats)
  ;; line height
  (let ([lh (fxquotient (fx* 3 (patch-height (vector-ref num 0))) 2)])
    (wi-slam-background)
    ;; draw animated background
    (wi-draw-animated-back)
    (wi-draw-lf)
    (v-draw-patch SP_STATSX SP_STATSY 0 kills)
    (wi-draw-percent (fx- SCREENWIDTH SP_STATSX) SP_STATSY (fxvector-ref cnt_kills 0))
    (v-draw-patch SP_STATSX (fx+ SP_STATSY lh) 0 items)
    (wi-draw-percent (fx- SCREENWIDTH SP_STATSX) (fx+ SP_STATSY lh) (fxvector-ref cnt_items 0))
    (v-draw-patch SP_STATSX (fx+ SP_STATSY (fx* 2 lh)) 0 sp_secret)
    (wi-draw-percent (fx- SCREENWIDTH SP_STATSX) (fx+ SP_STATSY (fx* 2 lh)) (fxvector-ref cnt_secret 0))
    (v-draw-patch SP_TIMEX SP_TIMEY 0 time-patch)
    (wi-draw-time (fx- (fxquotient SCREENWIDTH 2) SP_TIMEX) SP_TIMEY cnt_time)
    (when (fx< (wbstartstruct-epsd wbs) 3)
      (v-draw-patch (fx+ (fxquotient SCREENWIDTH 2) SP_TIMEX) SP_TIMEY 0 par)
      (wi-draw-time (fx- SCREENWIDTH SP_TIMEX) SP_TIMEY cnt_par))))

(define (wi-check-for-accelerate)
  ;; check for button presses to skip delays
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (let* ([player (vector-ref players i)]
             [buttons (ticcmd-buttons (player-cmd player))])
        (if (fxlogtest buttons BT_ATTACK)
            (begin
              (unless (player-attackdown player)
                (set! acceleratestage 1))
              (player-attackdown-set! player #t))
            (player-attackdown-set! player #f))
        (if (fxlogtest buttons BT_USE)
            (begin
              (unless (player-usedown player)
                (set! acceleratestage 1))
              (player-usedown-set! player #t))
            (player-usedown-set! player #f))))))

;; Updates stuff each tick
(define (wi-ticker)
  ;; counter for general background animation
  (set! bcnt (fx+ bcnt 1))
  (when (fx= bcnt 1)
    ;; intermission music
    (s-change-music (if (fx= gamemode commercial) mus_dm2int mus_inter) #t))
  (wi-check-for-accelerate)
  (cond
    [(fx= wi-state StatCount)
     (cond [(not (fx= deathmatch 0)) (wi-update-deathmatch-stats)]
           [netgame (wi-update-netgame-stats)]
           [else (wi-update-stats)])]
    [(fx= wi-state ShowNextLoc) (wi-update-show-next-loc)]
    [(fx= wi-state NoState) (wi-update-no-state)]))

(define (wi-load-data)
  (let* ([epsd (wbstartstruct-epsd wbs)]
         [name (cond
                 [(fx= gamemode commercial) "INTERPIC"]
                 [(and (fx= gamemode retail) (fx= epsd 3)) "INTERPIC"]
                 [else (format "WIMAP~a" epsd)])])
    ;; background
    (set! bg (w-cache-lump-name name))
    (v-draw-patch 0 0 1 bg)
    (if (fx= gamemode commercial)
        (begin
          (set! NUMCMAPS 32)
          (set! lnames (make-vector NUMCMAPS))
          (do ([i 0 (fx+ i 1)]) ((fx= i NUMCMAPS))
            (vector-set! lnames i (w-cache-lump-name (format "CWILV~2,'0d" i)))))
        (begin
          (set! lnames (make-vector NUMMAPS))
          (do ([i 0 (fx+ i 1)]) ((fx= i NUMMAPS))
            (vector-set! lnames i (w-cache-lump-name (format "WILV~a~a" epsd i))))
          ;; you are here
          (vector-set! yah 0 (w-cache-lump-name "WIURH0"))
          ;; you are here (alt.)
          (vector-set! yah 1 (w-cache-lump-name "WIURH1"))
          ;; splat
          (set! splat (w-cache-lump-name "WISPLAT"))
          (when (fx< epsd 3)
            (let ([as (vector-ref anims epsd)])
              (do ([j 0 (fx+ j 1)]) ((fx= j (vector-length as)))
                (let ([a (vector-ref as j)])
                  (do ([i 0 (fx+ i 1)]) ((fx= i (anim-nanims a)))
                    ;; MONDO HACK!
                    (vector-set! (anim-p a) i
                                 (if (or (not (fx= epsd 1)) (not (fx= j 8)))
                                     ;; animations
                                     (w-cache-lump-name (format "WIA~a~2,'0d~2,'0d" epsd j i))
                                     ;; HACK ALERT!
                                     (vector-ref (anim-p (vector-ref (vector-ref anims 1) 4)) i)))))))))))
  ;; More hacks on minus sign.
  (set! wiminus (w-cache-lump-name "WIMINUS"))
  ;; numbers 0-9
  (do ([i 0 (fx+ i 1)]) ((fx= i 10))
    (vector-set! num i (w-cache-lump-name (format "WINUM~a" i))))
  (set! percent (w-cache-lump-name "WIPCNT"))     ; percent sign
  (set! finished (w-cache-lump-name "WIF"))       ; "finished"
  (set! entering (w-cache-lump-name "WIENTER"))   ; "entering"
  (set! kills (w-cache-lump-name "WIOSTK"))       ; "kills"
  (set! secret (w-cache-lump-name "WIOSTS"))      ; "scrt"
  (set! sp_secret (w-cache-lump-name "WISCRT2"))  ; "secret"
  ;; Yuck. ("items": C tests `if (french)`, which is always true.)
  (set! items (w-cache-lump-name (if (and netgame (fx= deathmatch 0)) "WIOBJ" "WIOSTI")))
  (set! frags (w-cache-lump-name "WIFRGS"))       ; "frgs"
  (set! colon (w-cache-lump-name "WICOLON"))      ; ":"
  (set! time-patch (w-cache-lump-name "WITIME"))  ; "time"
  (set! sucks (w-cache-lump-name "WISUCKS"))      ; "sucks"
  (set! par (w-cache-lump-name "WIPAR"))          ; "par"
  (set! killers (w-cache-lump-name "WIKILRS"))    ; "killers" (vertical)
  (set! victims (w-cache-lump-name "WIVCTMS"))    ; "victims" (horiz)
  (set! total (w-cache-lump-name "WIMSTT"))       ; "total"
  (set! star (w-cache-lump-name "STFST01"))       ; your face
  (set! bstar (w-cache-lump-name "STFDEAD0"))     ; dead face
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (vector-set! wi-p i (w-cache-lump-name (format "STPB~a" i)))       ; "1,2,3,4"
    (vector-set! bp i (w-cache-lump-name (format "WIBP~a" (fx+ i 1)))))) ; "1,2,3,4"

(define (wi-drawer)
  (cond
    [(fx= wi-state StatCount)
     (cond [(not (fx= deathmatch 0)) (wi-draw-deathmatch-stats)]
           [netgame (wi-draw-netgame-stats)]
           [else (wi-draw-stats)])]
    [(fx= wi-state ShowNextLoc) (wi-draw-show-next-loc)]
    [(fx= wi-state NoState) (wi-draw-no-state)]))

(define (wi-init-variables wbstartstruct)
  (set! wbs wbstartstruct)
  (set! acceleratestage 0)
  (set! cnt 0)
  (set! bcnt 0)
  (set! firstrefresh 1)
  (set! me (wbstartstruct-pnum wbs))
  (set! plrs (wbstartstruct-plyr wbs))
  (when (fx= (wbstartstruct-maxkills wbs) 0) (wbstartstruct-maxkills-set! wbs 1))
  (when (fx= (wbstartstruct-maxitems wbs) 0) (wbstartstruct-maxitems-set! wbs 1))
  (when (fx= (wbstartstruct-maxsecret wbs) 0) (wbstartstruct-maxsecret-set! wbs 1))
  (when (and (not (fx= gamemode retail)) (fx> (wbstartstruct-epsd wbs) 2))
    (wbstartstruct-epsd-set! wbs (fx- (wbstartstruct-epsd wbs) 3))))

(define (wi-start wbstartstruct)
  (wi-init-variables wbstartstruct)
  (wi-load-data)
  (cond [(not (fx= deathmatch 0)) (wi-init-deathmatch-stats)]
        [netgame (wi-init-netgame-stats)]
        [else (wi-init-stats)]))
