;;; p_saveg.c: archiving the game state into a savegame and back.
;;; Included by p_local.sls.
;;;
;;; linuxdoom copies whole structs into the savegame with memcpy, so a
;;; savegame is a dump of the 32-bit i386 layouts of player_t, mobj_t and
;;; the special thinkers, pointers and padding included. The layouts below
;;; list each struct's fields in C order with their C types, and the
;;; offsets follow from the i386 alignment rules (tests/saveg.ss checks
;;; them against the oracle's compiler). Fields that hold pointers C
;;; doesn't swizzle are written as 0 here; on load, C overwrites them all
;;; except mobj_t's tracer, which keeps a stale pointer (it's only used by
;;; DOOM II's revenant and arch-vile) and here becomes NULL.

;; save_p and the buffer it points into (g_game sets both). While saving,
;; savemask (the same size, or #f) gets a 1 for each byte whose value C
;; leaves undefined: pointers, padding, and the bytes PADSAVEP skips.
;; Tests compare savegames with the oracle's on the other bytes.
(define-global savebuffer (make-bytevector 0))
(define-global save_p 0)
(define-global savemask #f)

(define (mask! from to)
  (when savemask
    (do ([i from (fx+ i 1)]) ((fx>= i to))
      (bytevector-u8-set! savemask i 1))))

;; Pads save_p to a 4-byte boundary so that the load/save works on
;; SGI&Gecko. (C pads the address; both its buffers are 4-aligned.)
(define (padsavep)
  (let ([new (fx+ save_p (fxand (fx- 4 (fxand save_p 3)) 3))])
    (mask! save_p new)
    (set! save_p new)))

;; A layout is a list of fields (cname type get set): type is one of the
;; scalar types below or (struct layout), whose get returns the inner
;; record. get is (lambda (obj) value); set is (lambda (obj value)), or #f
;; for fields the loader ignores. Pointers are written by get as an
;; integer (or #f for 0).
(define scalar-sizes '((int . 4) (uint . 4) (bool . 4) (ptr . 4) (short . 2) (schar . 1) (byte . 1)))

(define (field-align type)
  (if (pair? type)
      (apply fxmax (map (lambda (f) (field-align (cadr f))) (cadr type)))
      (cdr (assq type scalar-sizes))))

(define (field-size type)
  (if (pair? type) (layout-size (cadr type)) (cdr (assq type scalar-sizes))))

(define (align n a) (fx* (fxquotient (fx+ n (fx- a 1)) a) a))

;; ((cname . offset) ...) and the size (sizeof, padded to the alignment).
(define (layout-offsets layout)
  (let loop ([fs layout] [off 0] [acc '()])
    (if (null? fs)
        (reverse acc)
        (let* ([type (cadr (car fs))] [off (align off (field-align type))])
          (loop (cdr fs) (fx+ off (field-size type)) (cons (cons (car (car fs)) off) acc))))))
(define (layout-size layout)
  (let ([last (car (last-pair layout))])
    (align (fx+ (cdr (assq (car last) (layout-offsets layout))) (field-size (cadr last)))
           (field-align (list 'struct layout)))))

(define (layout-write! bv base layout obj)
  (define (defined! o n)
    (when savemask
      (do ([i 0 (fx+ i 1)]) ((fx= i n))
        (bytevector-u8-set! savemask (fx+ o i) 0))))
  (mask! base (fx+ base (layout-size layout)))
  (for-each
   (lambda (f off)
     (let ([type (cadr f)] [v ((caddr f) obj)] [o (fx+ base (cdr off))])
       (unless (or (pair? type) (eq? type 'ptr))
         (defined! o (field-size type)))
       (case type
         [(int uint ptr) (bytevector-u32-set! bv o (logand (or v 0) #xffffffff) (endianness little))]
         [(bool) (bytevector-u32-set! bv o (if v 1 0) (endianness little))]
         [(short) (bytevector-u16-set! bv o (fxand v #xffff) (endianness little))]
         [(schar byte) (bytevector-u8-set! bv o (fxand v #xff))]
         [else (layout-write! bv o (cadr type) v)])))
   layout (layout-offsets layout)))

(define (layout-read! bv base layout obj)
  (for-each
   (lambda (f off)
     (let ([type (cadr f)] [set (cadddr f)] [o (fx+ base (cdr off))])
       (if (pair? type)
           (layout-read! bv o (cadr type) ((caddr f) obj))
           (when set
             (set obj (case type
                        [(int) (bytevector-s32-ref bv o (endianness little))]
                        [(uint ptr) (bytevector-u32-ref bv o (endianness little))]
                        [(bool) (not (fx= 0 (bytevector-s32-ref bv o (endianness little))))]
                        [(short) (bytevector-s16-ref bv o (endianness little))]
                        [(schar) (bytevector-s8-ref bv o)]
                        [(byte) (bytevector-u8-ref bv o)]))))))
   layout (layout-offsets layout)))

;; Field helpers: a record field, an element of a vector or fxvector
;; field, a pointer the loader ignores.
(define-syntax fld
  (syntax-rules ()
    [(_ cname type get set) (list 'cname 'type get set)]))
(define (elts cname type n get)
  (let loop ([i 0])
    (if (fx= i n)
        '()
        (cons (let ([i i])
                (list (string->symbol (format "~a[~a]" cname i)) type
                      (lambda (o) (let ([v (get o)]) (if (fxvector? v) (fxvector-ref v i) (vector-ref v i))))
                      (lambda (o x) (let ([v (get o)]) (if (fxvector? v) (fxvector-set! v i x) (vector-set! v i x))))))
              (loop (fx+ i 1))))))
(define (ptr cname) (list cname 'ptr (lambda (o) 0) #f))

;; thinker_t: prev, next, function. Only whether the function is NULL
;; survives a savegame (a ceiling in stasis has none).
(define thinker-layout
  (list (ptr 'prev) (ptr 'next)
        (list 'function 'ptr (lambda (o) (if (thinker-func o) 1 0)) #f)))
(define thinker-field (list 'thinker (list 'struct thinker-layout) (lambda (o) o) #f))

(define ticcmd-layout
  (list (fld forwardmove schar ticcmd-forwardmove ticcmd-forwardmove-set!)
        (fld sidemove schar ticcmd-sidemove ticcmd-sidemove-set!)
        (fld angleturn short ticcmd-angleturn ticcmd-angleturn-set!)
        (fld consistancy short ticcmd-consistancy ticcmd-consistancy-set!)
        (fld chatchar byte ticcmd-chatchar ticcmd-chatchar-set!)
        (fld buttons byte ticcmd-buttons ticcmd-buttons-set!)))

;; A psprite's state is saved as its index in states (NULL stays 0, and 0
;; loads as NULL).
(define pspdef-layout
  (list (fld state int
             (lambda (p) (let ([s (pspdef-state p)]) (if s (state-index s) 0)))
             (lambda (p i) (pspdef-state-set! p (if (fx= i 0) #f (vector-ref states i)))))
        (fld tics int pspdef-tics pspdef-tics-set!)
        (fld sx int pspdef-sx pspdef-sx-set!)
        (fld sy int pspdef-sy pspdef-sy-set!)))

(define mapthing-layout
  (list (fld x short mapthing-x mapthing-x-set!)
        (fld y short mapthing-y mapthing-y-set!)
        (fld angle short mapthing-angle mapthing-angle-set!)
        (fld type short mapthing-type mapthing-type-set!)
        (fld options short mapthing-options mapthing-options-set!)))

(define player-layout
  `(,(ptr 'mo)
    ,(fld playerstate int player-playerstate player-playerstate-set!)
    (cmd (struct ,ticcmd-layout) ,player-cmd #f)
    ,(fld viewz int player-viewz player-viewz-set!)
    ,(fld viewheight int player-viewheight player-viewheight-set!)
    ,(fld deltaviewheight int player-deltaviewheight player-deltaviewheight-set!)
    ,(fld bob int player-bob player-bob-set!)
    ,(fld health int player-health player-health-set!)
    ,(fld armorpoints int player-armorpoints player-armorpoints-set!)
    ,(fld armortype int player-armortype player-armortype-set!)
    ,@(elts 'powers 'int NUMPOWERS player-powers)
    ,@(elts 'cards 'bool NUMCARDS player-cards)
    ,(fld backpack bool player-backpack player-backpack-set!)
    ,@(elts 'frags 'int MAXPLAYERS player-frags)
    ,(fld readyweapon int player-readyweapon player-readyweapon-set!)
    ,(fld pendingweapon int player-pendingweapon player-pendingweapon-set!)
    ,@(elts 'weaponowned 'bool NUMWEAPONS player-weaponowned)
    ,@(elts 'ammo 'int NUMAMMO player-ammo)
    ,@(elts 'maxammo 'int NUMAMMO player-maxammo)
    ,(fld attackdown bool player-attackdown player-attackdown-set!)
    ,(fld usedown bool player-usedown player-usedown-set!)
    ,(fld cheats int player-cheats player-cheats-set!)
    ,(fld refire int player-refire player-refire-set!)
    ,(fld killcount int player-killcount player-killcount-set!)
    ,(fld itemcount int player-itemcount player-itemcount-set!)
    ,(fld secretcount int player-secretcount player-secretcount-set!)
    ,(ptr 'message)
    ,(fld damagecount int player-damagecount player-damagecount-set!)
    ,(fld bonuscount int player-bonuscount player-bonuscount-set!)
    ,(ptr 'attacker)
    ,(fld extralight int player-extralight player-extralight-set!)
    ,(fld fixedcolormap int player-fixedcolormap player-fixedcolormap-set!)
    ,(fld colormap int player-colormap player-colormap-set!)
    (|psprites[0]| (struct ,pspdef-layout) ,(lambda (p) (vector-ref (player-psprites p) 0)) #f)
    (|psprites[1]| (struct ,pspdef-layout) ,(lambda (p) (vector-ref (player-psprites p) 1)) #f)
    ,(fld didsecret bool player-didsecret player-didsecret-set!)))

;; mobj_t. state is saved as its index, player as its index + 1 (0 for
;; none); the loader recomputes the links, subsector, info, floorz and
;; ceilingz, and clears target.
(define mobj-layout
  `(,thinker-field
    ,(fld x int degenmobj-x degenmobj-x-set!)
    ,(fld y int degenmobj-y degenmobj-y-set!)
    ,(fld z int degenmobj-z degenmobj-z-set!)
    ,(ptr 'snext) ,(ptr 'sprev)
    ,(fld angle uint mobj-angle mobj-angle-set!)
    ,(fld sprite int mobj-sprite mobj-sprite-set!)
    ,(fld frame int mobj-frame mobj-frame-set!)
    ,(ptr 'bnext) ,(ptr 'bprev) ,(ptr 'subsector)
    ,(fld floorz int mobj-floorz mobj-floorz-set!)
    ,(fld ceilingz int mobj-ceilingz mobj-ceilingz-set!)
    ,(fld radius int mobj-radius mobj-radius-set!)
    ,(fld height int mobj-height mobj-height-set!)
    ,(fld momx int mobj-momx mobj-momx-set!)
    ,(fld momy int mobj-momy mobj-momy-set!)
    ,(fld momz int mobj-momz mobj-momz-set!)
    ,(fld validcount int mobj-validcount mobj-validcount-set!)
    ,(fld type int mobj-type mobj-type-set!)
    ,(ptr 'info)
    ,(fld tics int mobj-tics mobj-tics-set!)
    ,(fld state int (lambda (m) (state-index (mobj-state m)))
          (lambda (m i) (mobj-state-set! m (vector-ref states i))))
    ,(fld flags int mobj-flags mobj-flags-set!)
    ,(fld health int mobj-health mobj-health-set!)
    ,(fld movedir int mobj-movedir mobj-movedir-set!)
    ,(fld movecount int mobj-movecount mobj-movecount-set!)
    ,(ptr 'target)
    ,(fld reactiontime int mobj-reactiontime mobj-reactiontime-set!)
    ,(fld threshold int mobj-threshold mobj-threshold-set!)
    ,(fld player int
          (lambda (m) (let ([p (mobj-player m)]) (if p (fx+ (player-index p) 1) 0)))
          (lambda (m i) (mobj-player-set! m (if (fx= i 0) #f (vector-ref players (fx- i 1))))))
    ,(fld lastlook int mobj-lastlook mobj-lastlook-set!)
    (spawnpoint (struct ,mapthing-layout) ,mobj-spawnpoint #f)
    ,(ptr 'tracer)))

;; The specials' sector is saved as its index.
(define (sector-field get set)
  (list 'sector 'int (lambda (o) (sector-index (get o)))
        (lambda (o i) (set o (vector-ref sectors i)))))

(define ceiling-layout
  (list thinker-field
        (fld type int ceilingmove-type ceilingmove-type-set!)
        (sector-field ceilingmove-sector ceilingmove-sector-set!)
        (fld bottomheight int ceilingmove-bottomheight ceilingmove-bottomheight-set!)
        (fld topheight int ceilingmove-topheight ceilingmove-topheight-set!)
        (fld speed int ceilingmove-speed ceilingmove-speed-set!)
        (fld crush bool ceilingmove-crush ceilingmove-crush-set!)
        (fld direction int ceilingmove-direction ceilingmove-direction-set!)
        (fld tag int ceilingmove-tag ceilingmove-tag-set!)
        (fld olddirection int ceilingmove-olddirection ceilingmove-olddirection-set!)))

(define vldoor-layout
  (list thinker-field
        (fld type int vldoor-type vldoor-type-set!)
        (sector-field vldoor-sector vldoor-sector-set!)
        (fld topheight int vldoor-topheight vldoor-topheight-set!)
        (fld speed int vldoor-speed vldoor-speed-set!)
        (fld direction int vldoor-direction vldoor-direction-set!)
        (fld topwait int vldoor-topwait vldoor-topwait-set!)
        (fld topcountdown int vldoor-topcountdown vldoor-topcountdown-set!)))

(define floormove-layout
  (list thinker-field
        (fld type int floormove-type floormove-type-set!)
        (fld crush bool floormove-crush floormove-crush-set!)
        (sector-field floormove-sector floormove-sector-set!)
        (fld direction int floormove-direction floormove-direction-set!)
        (fld newspecial int floormove-newspecial floormove-newspecial-set!)
        (fld texture short floormove-texture floormove-texture-set!)
        (fld floordestheight int floormove-floordestheight floormove-floordestheight-set!)
        (fld speed int floormove-speed floormove-speed-set!)))

(define plat-layout
  (list thinker-field
        (sector-field plat-sector plat-sector-set!)
        (fld speed int plat-speed plat-speed-set!)
        (fld low int plat-low plat-low-set!)
        (fld high int plat-high plat-high-set!)
        (fld wait int plat-wait plat-wait-set!)
        (fld count int plat-count plat-count-set!)
        (fld status int plat-status plat-status-set!)
        (fld oldstatus int plat-oldstatus plat-oldstatus-set!)
        (fld crush bool plat-crush plat-crush-set!)
        (fld tag int plat-tag plat-tag-set!)
        (fld type int plat-type plat-type-set!)))

(define lightflash-layout
  (list thinker-field
        (sector-field lightflash-sector lightflash-sector-set!)
        (fld count int lightflash-count lightflash-count-set!)
        (fld maxlight int lightflash-maxlight lightflash-maxlight-set!)
        (fld minlight int lightflash-minlight lightflash-minlight-set!)
        (fld maxtime int lightflash-maxtime lightflash-maxtime-set!)
        (fld mintime int lightflash-mintime lightflash-mintime-set!)))

(define strobe-layout
  (list thinker-field
        (sector-field strobe-sector strobe-sector-set!)
        (fld count int strobe-count strobe-count-set!)
        (fld minlight int strobe-minlight strobe-minlight-set!)
        (fld maxlight int strobe-maxlight strobe-maxlight-set!)
        (fld darktime int strobe-darktime strobe-darktime-set!)
        (fld brighttime int strobe-brighttime strobe-brighttime-set!)))

(define glow-layout
  (list thinker-field
        (sector-field glow-sector glow-sector-set!)
        (fld minlight int glow-minlight glow-minlight-set!)
        (fld maxlight int glow-maxlight glow-maxlight-set!)
        (fld direction int glow-direction glow-direction-set!)))

;; For tests: the C struct name and layout of each.
(define saveg-layouts
  `((thinker_t . ,thinker-layout) (ticcmd_t . ,ticcmd-layout) (pspdef_t . ,pspdef-layout)
    (mapthing_t . ,mapthing-layout) (player_t . ,player-layout) (mobj_t . ,mobj-layout)
    (ceiling_t . ,ceiling-layout) (vldoor_t . ,vldoor-layout) (floormove_t . ,floormove-layout)
    (plat_t . ,plat-layout) (lightflash_t . ,lightflash-layout) (strobe_t . ,strobe-layout)
    (glow_t . ,glow-layout)))

(define (save-struct! layout obj)
  (padsavep)
  (layout-write! savebuffer save_p layout obj)
  (set! save_p (fx+ save_p (layout-size layout))))

(define (load-struct! layout obj)
  (padsavep)
  (layout-read! savebuffer save_p layout obj)
  (set! save_p (fx+ save_p (layout-size layout))))

(define (save-byte! b)
  (bytevector-u8-set! savebuffer save_p (fxand b #xff))
  (set! save_p (fx+ save_p 1)))
(define (load-byte!)
  (let ([b (bytevector-u8-ref savebuffer save_p)])
    (set! save_p (fx+ save_p 1))
    b))

;; P_ArchivePlayers
(define (p-archive-players)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (save-struct! player-layout (vector-ref players i)))))

;; P_UnArchivePlayers
(define (p-unarchive-players)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (when (vector-ref playeringame i)
      (let ([p (vector-ref players i)])
        (load-struct! player-layout p)
        ;; will be set when unarc thinker
        (player-mo-set! p #f)
        (player-message-set! p #f)
        (player-attacker-set! p #f)))))

;; P_ArchiveWorld
(define (p-archive-world)
  (define (put v)
    (bytevector-u16-set! savebuffer save_p (fxand v #xffff) (endianness little))
    (set! save_p (fx+ save_p 2)))
  ;; do sectors
  (vector-for-each
   (lambda (sec)
     (put (fxsra (sector-floorheight sec) FRACBITS))
     (put (fxsra (sector-ceilingheight sec) FRACBITS))
     (put (sector-floorpic sec))
     (put (sector-ceilingpic sec))
     (put (sector-lightlevel sec))
     (put (sector-special sec))         ; needed?
     (put (sector-tag sec)))            ; needed?
   sectors)
  ;; do lines
  (vector-for-each
   (lambda (li)
     (put (line-flags li))
     (put (line-special li))
     (put (line-tag li))
     (do ([j 0 (fx+ j 1)]) ((fx= j 2))
       (let ([sn (fxvector-ref (line-sidenum li) j)])
         (unless (fx= sn -1)
           (let ([si (vector-ref sides sn)])
             (put (fxsra (side-textureoffset si) FRACBITS))
             (put (fxsra (side-rowoffset si) FRACBITS))
             (put (side-toptexture si))
             (put (side-bottomtexture si))
             (put (side-midtexture si)))))))
   lines))

;; P_UnArchiveWorld
(define (p-unarchive-world)
  (define (get)
    (let ([v (bytevector-s16-ref savebuffer save_p (endianness little))])
      (set! save_p (fx+ save_p 2))
      v))
  (define (get-fixed) (int32 (fxsll (get) FRACBITS)))
  ;; do sectors
  (vector-for-each
   (lambda (sec)
     (sector-floorheight-set! sec (get-fixed))
     (sector-ceilingheight-set! sec (get-fixed))
     (sector-floorpic-set! sec (get))
     (sector-ceilingpic-set! sec (get))
     (sector-lightlevel-set! sec (get))
     (sector-special-set! sec (get))    ; needed?
     (sector-tag-set! sec (get))        ; needed?
     (sector-specialdata-set! sec #f)
     (sector-soundtarget-set! sec #f))
   sectors)
  ;; do lines
  (vector-for-each
   (lambda (li)
     (line-flags-set! li (get))
     (line-special-set! li (get))
     (line-tag-set! li (get))
     (do ([j 0 (fx+ j 1)]) ((fx= j 2))
       (let ([sn (fxvector-ref (line-sidenum li) j)])
         (unless (fx= sn -1)
           (let ([si (vector-ref sides sn)])
             (side-textureoffset-set! si (get-fixed))
             (side-rowoffset-set! si (get-fixed))
             (side-toptexture-set! si (get))
             (side-bottomtexture-set! si (get))
             (side-midtexture-set! si (get)))))))
   lines))

;; Thinkers: thinkerclass_t
(define tc_end 0)
(define tc_mobj 1)

;; P_ArchiveThinkers
(define (p-archive-thinkers)
  ;; save off the current thinkers
  (let loop ([th (thinker-next thinkercap)])
    (unless (eq? th thinkercap)
      (when (eq? (thinker-func th) p-mobj-thinker)
        (save-byte! tc_mobj)
        (save-struct! mobj-layout th))
      ;; I_Error ("P_ArchiveThinkers: Unknown thinker function");
      (loop (thinker-next th))))
  ;; add a terminating marker
  (save-byte! tc_end))

;; P_UnArchiveThinkers
(define (p-unarchive-thinkers)
  ;; remove all the current thinkers
  (let loop ([th (thinker-next thinkercap)])
    (unless (eq? th thinkercap)
      (let ([next (thinker-next th)])
        (when (eq? (thinker-func th) p-mobj-thinker)
          (p-remove-mobj th))
        (loop next))))
  (p-init-thinkers)
  ;; read in saved thinkers
  (let loop ()
    (let ([tclass (load-byte!)])
      (cond
        [(fx= tclass tc_end) (void)]    ; end of list
        [(fx= tclass tc_mobj)
         (let ([mobj (make-mobj)])
           (load-struct! mobj-layout mobj)
           (mobj-target-set! mobj #f)
           (when (mobj-player mobj)
             (player-mo-set! (mobj-player mobj) mobj))
           (p-set-thing-position mobj)
           (mobj-info-set! mobj (vector-ref mobjinfo-table (mobj-type mobj)))
           (let ([sec (subsector-sector (mobj-subsector mobj))])
             (mobj-floorz-set! mobj (sector-floorheight sec))
             (mobj-ceilingz-set! mobj (sector-ceilingheight sec)))
           (thinker-func-set! mobj p-mobj-thinker)
           (p-add-thinker mobj)
           (loop))]
        [else (i-error "Unknown tclass ~a in savegame" tclass)]))))

;; specials_e
(define tc_ceiling 0)
(define tc_door 1)
(define tc_floor 2)
(define tc_plat 3)
(define tc_flash 4)
(define tc_strobe 5)
(define tc_glow 6)
(define tc_endspecials 7)

;; P_ArchiveSpecials. Things to handle: T_MoveCeiling, T_VerticalDoor,
;; T_MoveFloor, T_LightFlash, T_StrobeFlash, T_Glow, T_PlatRaise. (Not
;; T_FireFlicker: fire flickers are lost. Nor plats in stasis, whose
;; function is NULL: only ceilings are looked for among those.)
(define (p-archive-specials)
  ;; save off the current thinkers
  (let loop ([th (thinker-next thinkercap)])
    (unless (eq? th thinkercap)
      (let ([f (thinker-func th)])
        (define (save tc layout)
          (save-byte! tc)
          (save-struct! layout th))
        (cond
          [(not f)
           (when (let find ([i 0])
                   (and (fx< i MAXCEILINGS)
                        (or (eq? (vector-ref activeceilings i) th) (find (fx+ i 1)))))
             (save tc_ceiling ceiling-layout))]
          [(eq? f t-move-ceiling) (save tc_ceiling ceiling-layout)]
          [(eq? f t-vertical-door) (save tc_door vldoor-layout)]
          [(eq? f t-move-floor) (save tc_floor floormove-layout)]
          [(eq? f t-plat-raise) (save tc_plat plat-layout)]
          [(eq? f t-light-flash) (save tc_flash lightflash-layout)]
          [(eq? f t-strobe-flash) (save tc_strobe strobe-layout)]
          [(eq? f t-glow) (save tc_glow glow-layout)]))
      (loop (thinker-next th))))
  ;; add a terminating marker
  (save-byte! tc_endspecials))

;; P_UnArchiveSpecials
(define (p-unarchive-specials)
  ;; read in saved thinkers
  (let loop ()
    (let ([tclass (load-byte!)])
      ;; Each special's thinker function is NULL in a savegame only for a
      ;; ceiling in stasis.
      (define (load make layout func)
        (let ([th (make)])
          (padsavep)
          (let ([f (fx+ save_p (cdr (assq 'function (layout-offsets thinker-layout))))])
            (layout-read! savebuffer save_p layout th)
            (thinker-func-set! th (if (fx= 0 (bytevector-u32-ref savebuffer f (endianness little)))
                                      #f
                                      func)))
          (set! save_p (fx+ save_p (layout-size layout)))
          th))
      (cond
        [(fx= tclass tc_endspecials) (void)] ; end of list
        [(fx= tclass tc_ceiling)
         (let ([ceiling (load make-ceilingmove ceiling-layout t-move-ceiling)])
           (sector-specialdata-set! (ceilingmove-sector ceiling) ceiling)
           (p-add-thinker ceiling)
           (p-add-active-ceiling ceiling)
           (loop))]
        [(fx= tclass tc_door)
         (let ([door (load make-vldoor vldoor-layout t-vertical-door)])
           (sector-specialdata-set! (vldoor-sector door) door)
           (thinker-func-set! door t-vertical-door)
           (p-add-thinker door)
           (loop))]
        [(fx= tclass tc_floor)
         (let ([floor (load make-floormove floormove-layout t-move-floor)])
           (sector-specialdata-set! (floormove-sector floor) floor)
           (thinker-func-set! floor t-move-floor)
           (p-add-thinker floor)
           (loop))]
        [(fx= tclass tc_plat)
         (let ([plat (load make-plat plat-layout t-plat-raise)])
           (sector-specialdata-set! (plat-sector plat) plat)
           (p-add-thinker plat)
           (p-add-active-plat plat)
           (loop))]
        [(fx= tclass tc_flash)
         (let ([flash (load make-lightflash lightflash-layout t-light-flash)])
           (thinker-func-set! flash t-light-flash)
           (p-add-thinker flash)
           (loop))]
        [(fx= tclass tc_strobe)
         (let ([strobe (load make-strobe strobe-layout t-strobe-flash)])
           (thinker-func-set! strobe t-strobe-flash)
           (p-add-thinker strobe)
           (loop))]
        [(fx= tclass tc_glow)
         (let ([glow (load make-glow glow-layout t-glow)])
           (thinker-func-set! glow t-glow)
           (p-add-thinker glow)
           (loop))]
        [else (i-error "P_UnarchiveSpecials:Unknown tclass ~a in savegame" tclass)]))))
