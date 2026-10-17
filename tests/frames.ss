;;; The whole game, frame by frame, against the oracle: the main loop, the
;;; status bar, the heads-up text, the menus, the screen wipe, the
;;; intermission, the finale, and the palette, as well as the simulation.
;;;
;;; Each case runs the oracle and then the port (d-run, headless) with the
;;; same arguments, the same scripted keys (ORACLE_KEYS: after frame F,
;;; press keys; +k holds, -k releases) and level exits (ORACLE_EXIT), with
;;; fake time (one tic per I_GetTime call) in both. Every trace field of
;;; every frame must match, including rndindex, framehash and palettehash.
;;;
;;; The one exception: pixels that vanilla draws from memory it doesn't own
;;; (DESIGN.md, "Garbage reads"). The renderer marks those in garbage-mask;
;;; for a frame whose only difference is its framehash, the oracle is run
;;; again to dump that frame, and every differing pixel must be marked.
;;;
;;; The sound too: the oracle runs linuxdoom's sound server in lockstep
;;; (build/oracle/sndserver: one mixed buffer after each frame), and every
;;; buffer the port's server mixes must hash the same.
;;;
;;; Savegames: the oracle and the port each run in their own directory
;;; (dir/NAME/oracle and dir/NAME/port, with a fresh HOME inside), where
;;; they save and load. Every savegame the port writes must equal the
;;; oracle's file of the same name, except for the bytes C leaves undefined
;;; (pointers and padding; p_saveg's savemask). A case can start from files
;;; another case left (its oracle's), copied into both directories.
;;;
;;; CASE=name runs one case; without it, each case runs in its own process.
(import (chezscheme) (doom d_main) (doom doomdef) (doom doomstat) (doom d_event)
        (doom i_system) (doom i_video) (doom trace) (doom v_video) (doom r_local)
        (doom p_local) (doom i_sound) (doom m_argv))
(load "tests/lib.ss")

;; Key codes for scripts.
(define ESC 27) (define ENTER 13) (define UP 173) (define DOWN 175) (define LEFT 172)
(define RIGHT 174) (define BKSP 127) (define F1 187) (define F2 188) (define F3 189) (define F4 190)
(define F5 191) (define F6 192) (define F7 193) (define F8 194) (define F9 195) (define F10 196) (define F11 215)
(define TAB 9) (define MINUS 45) (define EQUALS 61) (define PAUSE 255) (define FIRE 157)
(define USE 32)
(define (chars s) (map char->integer (string->list s)))

;; A script: list of (frame key ...), where a key is a number (press), or
;; (+ k) / (- k) (hold / release). Exits: list of (frame . n|s), n for the
;; normal exit and s for the secret one.
(define (script->string script)
  (apply string-append
         (let loop ([l script] [first #t])
           (if (null? l)
               '()
               (cons (format "~a~a:~a" (if first "" ";") (caar l)
                             (apply string-append
                                    (let keys ([ks (cdar l)] [first #t])
                                      (if (null? ks)
                                          '()
                                          (cons (format "~a~a" (if first "" ",")
                                                        (let ([k (car ks)])
                                                          (if (pair? k)
                                                              (format "~a~a" (car k) (cadr k))
                                                              k)))
                                                (keys (cdr ks) #f))))))
                     (loop (cdr l) #f))))))

;; The cases: name, arguments, keys, exits, when to stop (max gametic, or
;; #f for the oracle's natural end), and optionally files to start with:
;; ((case file) ...), from that case's oracle directory, or (write path
;; text), relative to both run directories; and (wad dir) runs both with
;; DOOMWADDIR=wads/dir instead of wads (tools/fetch-freedoom.sh).
(define cases
  `(;; -timedemo: the status bar, messages, palette flashes, the wipe into
    ;; the level.
    ("demo1" ("-timedemo" "demo1") () () #f)
    ("demo2" ("-timedemo" "demo2") () () #f)
    ("demo3" ("-timedemo" "demo3") () () #f)
    ;; The title loop: pages, demos, and the wipes between them.
    ("attract" () () () 12000)
    ;; The menus, from the title loop: Read This!, options (messages,
    ;; detail, screen size, mouse, sound volume), load, the shareware
    ;; episode message, the nightmare prompt, then a new game.
    ("menus" ()
     ((5 ,ESC) (8 ,DOWN) (9 ,UP) (10 ,@(chars "r")) (11 ,ENTER) (14 ,ENTER) (17 ,ENTER)
      (20 ,@(chars "o")) (21 ,ENTER) (24 ,ENTER) (26 ,DOWN) (27 ,ENTER) (29 ,DOWN)
      (30 ,ENTER) (31 ,DOWN) (32 ,LEFT) (33 ,LEFT) (34 ,RIGHT) (35 ,DOWN) (36 ,RIGHT ,RIGHT)
      (37 ,DOWN) (38 ,ENTER) (40 ,LEFT) (41 ,LEFT) (42 ,DOWN ,DOWN) (43 ,RIGHT) (44 ,BKSP)
      (46 ,BKSP) (48 ,DOWN) (49 ,ENTER) (51 ,ESC)
      (53 ,ESC) (54 ,@(chars "n")) (55 ,ENTER) (57 ,DOWN) (58 ,ENTER) (60 ,ENTER)
      (63 ,ESC) (64 ,ENTER) (65 ,ENTER) (66 ,DOWN ,DOWN) (67 ,ENTER) (69 ,@(chars "n"))
      (71 ,ESC) (72 ,ENTER) (73 ,ENTER) (74 ,UP) (75 ,ENTER) (80 ,ENTER))
     () 1500)
    ;; In a game: the automap and its title and cheats, the status bar
    ;; cheats (god, ammo, the powers and their palettes, mypos), messages,
    ;; screen sizes, help, pause, gamma, the sound, save, load and quit
    ;; menus, idclev, and ending the game back to the title loop.
    ("ingame" ("-warp" "1" "1" "-skill" "3")
     ((3 ,TAB) (5 ,@(chars "iddt")) (7 ,@(chars "iddt")) (9 ,TAB) (11 ,@(chars "iddqd"))
      (14 ,@(chars "idkfa")) (17 (+ 51)) (18 (- 51)) (20 (+ ,UP)) (22 (+ ,FIRE)) (26 (- ,FIRE))
      (30 (- ,UP)) (32 ,@(chars "idbeholdv")) (35 ,@(chars "idbeholds")) (38 ,@(chars "idbeholdr"))
      (41 ,@(chars "idbeholdl")) (44 ,@(chars "idbeholdi")) (47 ,@(chars "idbehold"))
      (49 ,@(chars "idchoppers")) (52 ,@(chars "idmypos")) (55 ,F8) (57 ,F8) (59 ,MINUS)
      (60 ,MINUS) (62 ,EQUALS ,EQUALS ,EQUALS) (64 ,EQUALS ,EQUALS) (66 ,EQUALS)
      (68 ,MINUS ,MINUS ,MINUS) (70 ,F1) (73 ,ESC) (75 ,PAUSE) (80 ,PAUSE) (82 ,F11 ,F11)
      (85 ,F4 ,LEFT ,ESC) (88 ,F2) (89 ,ENTER) (90 ,@(chars "abc") ,BKSP) (91 ,ESC) (92 ,ESC)
      (93 ,F3) (94 ,ESC) (95 ,F5) (97 ,F10) (99 ,@(chars "n")) (101 ,TAB) (102 ,@(chars "f"))
      (103 ,@(chars "m")) (104 ,@(chars "g") ,@(chars "0") (+ ,MINUS)) (106 (- ,MINUS))
      (107 ,@(chars "c")) (108 ,TAB) (110 ,@(chars "idclev12")) (120 ,F7) (122 ,@(chars "y")))
     () 1000)
    ;; The intermission: stats, accelerated by fire, the episode map, and
    ;; on into E1M2.
    ("intermission" ("-warp" "1" "1" "-skill" "3")
     ((40 (+ ,FIRE)) (41 (- ,FIRE)) (50 (+ ,FIRE)) (51 (- ,FIRE)) (60 (+ ,USE)) (61 (- ,USE)))
     ((10 . n)) 700)
    ;; The secret exit to E1M9 and back to E1M4: the stats run out and wait,
    ;; and the map's "you are here" blinks out its delay.
    ("secret" ("-warp" "1" "3" "-skill" "2")
     ((80 (+ ,FIRE)) (81 (- ,FIRE)) (180 (+ ,FIRE)) (181 (- ,FIRE)))
     ((10 . s) (130 . n)) 1300)
    ;; The end of the episode: the finale's text, the wipe to its picture,
    ;; and the menu over it.
    ("finale" ("-warp" "1" "8" "-skill" "3")
     ((420 ,ESC) (424 ,ESC))
     ((10 . n)) 2300)
    ;; Saving and loading in a game with doors, lifts, lights and monsters
    ;; on the move: save to slot 1 from the menu, play on, quick save to
    ;; slot 2, play on, load slot 1 from the menu, then quick load slot 2.
    ("saveload" ("-warp" "1" "3" "-skill" "3")
     ((3 (+ ,UP)) (20 (- ,UP)) (21 (+ ,USE)) (22 (- ,USE)) (24 (+ ,FIRE)) (40 (- ,FIRE))
      (45 ,F2) (47 ,ENTER) (48 ,@(chars "first")) (49 ,ENTER)
      (55 (+ ,LEFT)) (60 (- ,LEFT)) (61 (+ ,UP)) (90 (- ,UP))
      (100 ,F6) (102 ,DOWN) (103 ,ENTER) (104 ,@(chars "quick")) (105 ,ENTER)
      (108 (+ ,FIRE)) (130 (- ,FIRE))
      (150 ,F3) (152 ,ENTER) (170 (+ ,UP)) (200 (- ,UP))
      (210 ,F9) (212 ,@(chars "y")) (215 (+ ,RIGHT)) (240 (- ,RIGHT)))
     () 1400)
    ;; Starting from the oracle's savegames, by -loadgame.
    ("loadgame" ("-loadgame" "0")
     ((5 (+ ,UP)) (40 (- ,UP)) (41 (+ ,FIRE)) (60 (- ,FIRE)))
     () 300
     (("saveload" "doomsav0.dsg") ("saveload" "doomsav1.dsg")))
;; Freedoom's IWADs as DOOM II and The Ultimate DOOM (tools/
    ;; fetch-freedoom.sh; skipped when absent): their demos, which run
    ;; DOOM II's monsters (the arch-vile, revenant tracers, mancubi...),
    ;; maps and switches, and episode 4.
    ("fd2-demo1" ("-timedemo" "demo1") () () #f ((wad "freedoom2")))
    ("fd2-demo2" ("-timedemo" "demo2") () () #f ((wad "freedoom2")))
    ("fd2-demo3" ("-timedemo" "demo3") () () #f ((wad "freedoom2")))
    ("fd1-demo1" ("-timedemo" "demo1") () () #f ((wad "freedoom1")))
    ("fd1-demo2" ("-timedemo" "demo2") () () #f ((wad "freedoom1")))
    ;; (Its map needs more than linuxdoom's default 2 MB zone, mb_used.)
    ("fd1-demo3" ("-timedemo" "demo3") () () #f
     ((wad "freedoom1") (write "home/.doomrc" "mb_used\t\t16\n")))
;; DOOM II's front end: its title loop (TITLEPIC, CREDIT, the demos);
    ;; its intermission, and the text screen after MAP06; the end, after
    ;; MAP30: the text, then the cast call, keys advancing it to deaths.
    ("fd2-attract" () () () 12000 ((wad "freedoom2")))
    ("fd2-text" ("-warp" "6" "-skill" "3")
     ;; (the intermission and the text screen go on when a ticcmd has
     ;; buttons: keys held over a tic)
     ((40 (+ ,FIRE)) (41 (- ,FIRE)) (50 (+ ,FIRE)) (51 (- ,FIRE)) (60 (+ ,USE)) (61 (- ,USE))
      (150 (+ ,USE)) (152 (- ,USE)))
     ((10 . n)) 2500 ((wad "freedoom2")))
    ("fd2-cast" ("-warp" "30" "-skill" "3")
     ((40 (+ ,FIRE)) (41 (- ,FIRE)) (50 (+ ,FIRE)) (51 (- ,FIRE)) (60 (+ ,USE)) (61 (- ,USE))
      (150 (+ ,USE)) (152 (- ,USE))
      (300 ,USE) (380 ,USE) (420 ,USE) (500 ,USE) (560 ,USE) (640 ,USE) (700 ,USE))
     ((10 . n)) 8000 ((wad "freedoom2")))
    ;; The Ultimate DOOM's episode 4: its end (E4M8's exit, the text and
    ;; the picture).
    ("fd1-e4" ("-warp" "4" "8" "-skill" "3")
     ((420 ,ESC) (424 ,ESC))
     ((10 . n)) 2500 ((wad "freedoom1")))
;; -debugfile (debug0.txt, TryRunTics' log, compared), and a response
    ;; file (@file: arguments separated by anything but '!' to 'z').
    ("debugfile" ("-warp" "1" "1" "-debugfile")
     ((5 (+ ,UP)) (40 (- ,UP)))
     () 400)
    ("responsefile" ("@args.rsp" "-nomonsters")
     () () 400
     ((write "args.rsp" "-warp 1 3\n-skill\t4|-respawn")))
    ;; -timer: a deathmatch level ends after so many minutes (one here).
    ("timer" ("-warp" "1" "1" "-deathmatch" "-timer" "1")
     () () 2400)
    ;; -devparm: the tic-rate dots, and screenshots (F1), which must be the
    ;; oracle's PCX files byte for byte.
    ("screenshot" ("-warp" "1" "1" "-devparm")
     ((5 ,F1) (8 (+ ,UP)) (12 ,F1) (13 (- ,UP)))
     () 150)
    ;; The config file: a .doomrc read the way linuxdoom's fscanf and
    ;; sscanf read it (octal, hex, a key without a value that takes the
    ;; next line as its value and keeps the last number, a quoted string
    ;; that loses its last character), the game played with what it set
    ;; (keys, screen size, detail, gamma, sound), then quit from the menu,
    ;; which writes .doomrc back: both must write the same file.
    ("config" ("-warp" "1" "1")
     ((3 (+ 119)) (20 (- 119)) (21 (+ 97)) (26 (- 97)) (27 ,@(chars "t") ,@(chars "hi") ,ENTER)
      (30 ,F10) (33 ,@(chars "y")))
     () 2000
     ((write "home/.doomrc"
             ,(string-append
               "mouse_sensitivity\t\t7\nsfx_volume\t\t010\nmusic_volume\t\t0x3\n"
               "show_messages\t\t1\nkey_up\t\t119\nkey_down 115\n  key_strafeleft\t97\n"
               "screenblocks\t\t7\ndetaillevel\t\t1\nusegamma\t\t2\n"
               "chatmacro0\t\t\"Hello there \"\nbogus_entry\t\t5\nsnd_channels\n"
               "key_fire\t\t102\n\n\nmousetype\t\t\"logitech\"\n"))))))

(define case-names (map car cases))

(define (case-ref name)
  (let ([c (assoc name cases)])
    (if (= (length c) 5) (append c '(())) c)))

(define dir "build/test/frames")

;; A fresh directory for one side of a case, with the case's files.
(define (setup-run-dir name side files)
  (let ([d (format "~a/~a/~a" dir name side)])
    (system (format "rm -rf ~a && mkdir -p ~a/home" d d))
    (for-each (lambda (f)
                (cond
                  [(eq? (car f) 'wad) (void)]
                  [(eq? (car f) 'write)
                    (call-with-output-file (format "~a/~a" d (cadr f))
                      (lambda (p) (display (caddr f) p)))]
                  [else (system (format "cp ~a/~a/oracle/~a ~a/" dir (car f) (cadr f) d))]))
              files)
    d))

;; The case's WAD directory.
(define (wad-dir files)
  (let ([w (assq 'wad files)])
    (format "~a/wads~a" (current-directory) (if w (string-append "/" (cadr w)) ""))))

;; Run the oracle on a case, writing dir/NAME.trace. With raw-frames (a
;; list, or 'all), also dump those frames as dir/NAME-oracle/fNNNN.raw.
(define (run-oracle name raw-frames)
  (apply
   (lambda (name args keys exits maxgametic files)
     (let ([rundir (setup-run-dir name "oracle" files)]
           [rawdir (format "~a/~a-oracle" dir name)])
       (system (format "rm -rf ~a && mkdir -p ~a" rawdir rawdir))
       (system
        (format "cd ~a && env -i HOME=$PWD/home DOOMWADDIR=~a ORACLE_TRACE=~a/~a/~a.trace ORACLE_SNDSERVER=~a/build/oracle/sndserver ORACLE_AUDIO=~a/~a/~a.audio ORACLE_FAKETIME=1 ~a ~a ~a ~a setarch -R ~a/build/oracle/doom-oracle ~a > ~a.log 2>&1"
                rundir (wad-dir files) (current-directory) dir name
                (current-directory) (current-directory) dir name
                (if maxgametic (format "ORACLE_MAX_GAMETIC=~a" maxgametic) "")
                (if (null? keys) "" (format "ORACLE_KEYS='~a'" (script->string keys)))
                (if (null? exits)
                    ""
                    (format "ORACLE_EXIT='~a'"
                            (apply string-append
                                   (map (lambda (e) (format "~a:~a;" (car e) (cdr e))) exits))))
                (if (pair? raw-frames)
                    (format "ORACLE_RAW_DIR=~a/~a ORACLE_RAW_FRAMES=~a" (current-directory) rawdir
                            (apply string-append
                                   (cons (number->string (car raw-frames))
                                         (map (lambda (f) (format ",~a" f)) (cdr raw-frames)))))
                    (if raw-frames (format "ORACLE_RAW_DIR=~a/~a" (current-directory) rawdir) ""))
                (current-directory)
                (apply string-append (map (lambda (a) (string-append " " a)) args))
                name))))
   (case-ref name)))

(define (read-trace path)
  (call-with-input-file path
    (lambda (p)
      (let loop ([acc '()])
        (let ([line (get-line p)])
          (if (eof-object? line)
              (list->vector (reverse acc))
              (loop (cons (with-input-from-string (string-append "(" line ")") read) acc))))))))

(define (read-raw path)
  (call-with-port (open-file-input-port path) get-bytevector-all))
(define (write-raw path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))

(define framehash-index
  (let loop ([l trace-field-names] [i 0])
    (if (eq? (car l) 'framehash) i (loop (cdr l) (+ i 1)))))

;; The port's run of a case. Returns (values frames first-mismatch
;; garbage-candidates audio-hashes savegames), where a candidate is a frame
;; differing only in framehash (saved with its mask as
;; dir/NAME-port/fNNNN.{raw,mask}), and savegames lists (file bytes mask).
(define (run-port name theirs)
  (apply
   (lambda (name args keys exits maxgametic files)
     (let ([mask (make-bytevector (* SCREENWIDTH SCREENHEIGHT) 0)]
           [n 0] [mismatch #f] [candidates '()] [audio '()] [saves '()]
           [rundir (setup-run-dir name "port" files)]
           [wads (wad-dir files)]
           [portdir (format "~a/~a/~a-port" (current-directory) dir name)])
       (system (format "rm -rf ~a && mkdir -p ~a" portdir portdir))
       (set! headless #t)
       (set! i-fake-time #t)
       (set! garbage-mask mask)
       (set! saveg-hook (lambda (file bytes mask) (set! saves (cons (list file bytes mask) saves))))
       (set! sound-hook (lambda (buf) (set! audio (cons (words-hash buf 0 (bytevector-length buf)) audio))))
       (set! d-frame-hook
             (lambda ()
               ;; The oracle's trace has ended (it exits in its trace hook
               ;; unless the game quit first): one frame past it, stop.
               (when (or mismatch (= n (vector-length theirs)))
                 (raise 'stop-case))
               (set! n (+ n 1))
               (let ([ours (trace-fields (framehash) (words-hash last-palette 0 768))]
                     [t (and (<= n (vector-length theirs)) (vector-ref theirs (- n 1)))])
                 (cond
                   [(not t) (set! mismatch (or mismatch (list 'frame n 'past-the-oracles-end)))]
                   [(equal? ours t) (void)]
                   [(equal? (list-head ours framehash-index) (list-head t framehash-index))
                    (set! candidates (cons n candidates))
                    (write-raw (format "~a/f~4,'0d.raw" portdir n)
                               (let ([bv (make-bytevector (* SCREENWIDTH SCREENHEIGHT))])
                                 (bytevector-copy! screens 0 bv 0 (bytevector-length bv))
                                 bv))
                    (write-raw (format "~a/f~4,'0d.mask" portdir n) mask)]
                   [else
                    (set! mismatch
                          (or mismatch
                              (list 'frame n 'fields
                                    (filter (lambda (x) x)
                                            (map (lambda (nm a b) (and (not (equal? a b)) (list nm a b)))
                                                 trace-field-names ours t)))))]))
               (bytevector-fill! mask 0)
               ;; after frame n: the scripted keys and exits
               (for-each (lambda (k)
                           (let ([down (lambda (k) (d-post-event (make-event ev_keydown k 0 0)))]
                                 [up (lambda (k) (d-post-event (make-event ev_keyup k 0 0)))])
                             (cond
                               [(not (pair? k)) (down k) (up k)]
                               [(eq? (car k) '+) (down (cadr k))]
                               [else (up (cadr k))])))
                         (let ([e (assv n keys)]) (if e (cdr e) '())))
               (for-each (lambda (e)
                           (when (= n (car e))
                             (if (eq? (cdr e) 's) (g-secret-exit-level) (g-exit-level))))
                         exits)))
       (putenv "DOOMWADDIR" wads)
       (putenv "HOME" (format "~a/~a/home" (current-directory) rundir))
       (parameterize ([current-output-port (open-output-string)]
                      [current-error-port (open-output-string)]
                      [current-directory rundir])
         (guard (c [(eq? c 'stop-case) (void)])
           (d-run args)))
       (values n mismatch (reverse candidates) (list->vector (reverse audio)) (reverse saves))))
   (case-ref name)))

;; #f if ours and theirs agree on every byte the mask doesn't exclude, else
;; the count of differing bytes and the first few offsets.
(define (savegame-diff ours mask theirs)
  (if (not (= (bytevector-length ours) (bytevector-length theirs)))
      (list 'lengths (bytevector-length ours) (bytevector-length theirs))
      (let loop ([i 0] [bad '()])
        (cond
          [(= i (bytevector-length ours))
           (and (pair? bad) (list (length bad) 'bytes 'first (list-head (reverse bad) (min 8 (length bad)))))]
          [(and (= (bytevector-u8-ref mask i) 0)
                (not (= (bytevector-u8-ref ours i) (bytevector-u8-ref theirs i))))
           (loop (+ i 1) (cons i bad))]
          [else (loop (+ i 1) bad)]))))

(define (filter-map f l)
  (let loop ([l l] [acc '()])
    (cond [(null? l) (reverse acc)]
          [(f (car l)) => (lambda (x) (loop (cdr l) (cons x acc)))]
          [else (loop (cdr l) acc)])))

(define (remove-duplicates l)
  (let loop ([l l] [acc '()])
    (cond [(null? l) (reverse acc)]
          [(member (car l) acc) (loop (cdr l) acc)]
          [else (loop (cdr l) (cons (car l) acc))])))

;; Differing pixels not marked as garbage reads.
(define (unexplained ours theirs mask)
  (let loop ([i 0] [k 0])
    (if (= i (bytevector-length ours))
        k
        (loop (+ i 1)
              (if (and (not (= (bytevector-u8-ref ours i) (bytevector-u8-ref theirs i)))
                       (= (bytevector-u8-ref mask i) 0))
                  (+ k 1)
                  k)))))

(define (run-case name)
  (if (file-exists? (wad-dir (list-ref (case-ref name) 5)))
      (run-case* name)
      (printf "skip ~a: no ~a (make freedoom)\n" name (wad-dir (list-ref (case-ref name) 5)))))

(define (run-case* name)
  (run-oracle name #f)
  (let ([theirs (read-trace (format "~a/~a.trace" dir name))])
    (let-values ([(n mismatch candidates audio saves) (run-port name theirs)])
      (check (format "~a: ~a frames match (all fields)" name (vector-length theirs)) #f mismatch)
      (check (format "~a: same number of frames" name) (vector-length theirs) n)
      ;; The oracle mixes after its last frame unless it exits in that
      ;; frame's trace hook (ORACLE_MAX_GAMETIC); the port mixes after its
      ;; last frame before it stops in the next frame's hook.
      (let* ([oracle-audio (vector-map cadr (read-trace (format "~a/~a.audio" dir name)))]
             [k (min (vector-length audio) (vector-length oracle-audio))])
        (check (format "~a: ~a mixed sound buffers match" name k) #f
               (let loop ([i 0])
                 (cond
                   [(= i k) #f]
                   [(= (vector-ref audio i) (vector-ref oracle-audio i)) (loop (+ i 1))]
                   [else (list 'buffer (+ i 1))])))
        (check (format "~a: buffer count" name) #t
               (and (> k 0) (<= -1 (- (vector-length oracle-audio) (vector-length audio)) 1))))
      ;; The config file, if the oracle wrote one (only I_Quit does).
      (let ([theirs (format "~a/~a/oracle/home/.doomrc" dir name)])
        (when (file-exists? theirs)
          (check (format "~a: .doomrc written as the oracle's" name)
                 (read-raw theirs)
                 (read-raw (format "~a/~a/port/home/.doomrc" dir name)))))
      ;; -debugfile's logs: the oracle's, then (the port runs one frame
      ;; past the oracle's last) at most one more line.
      (for-each
       (lambda (f)
         (let ([theirs (read-raw (format "~a/~a/oracle/~a" dir name f))]
               [ours (let ([p (format "~a/~a/port/~a" dir name f)])
                       (if (file-exists? p) (read-raw p) (bytevector)))])
           (check (format "~a: ~a written as the oracle's" name f) #t
                  (and (>= (bytevector-length ours) (bytevector-length theirs))
                       (let loop ([i 0])
                         (or (= i (bytevector-length theirs))
                             (and (= (bytevector-u8-ref ours i) (bytevector-u8-ref theirs i))
                                  (loop (+ i 1)))))
                       (<= (let count ([i (bytevector-length theirs)] [n 0])
                             (if (= i (bytevector-length ours))
                                 n
                                 (count (+ i 1) (if (= (bytevector-u8-ref ours i) 10) (+ n 1) n))))
                           1)))))
       (filter (lambda (f) (and (> (string-length f) 9) (string=? (substring f 0 5) "debug")
                                (string=? (substring f (- (string-length f) 4) (string-length f)) ".txt")))
               (directory-list (format "~a/~a/oracle" dir name))))
      ;; Screenshots. (pcx_t's reserved byte, 64, is left uninitialized.)
      (for-each
       (lambda (f)
         (define (pcx path)
           (and (file-exists? path)
                (let ([bv (read-raw path)])
                  (when (> (bytevector-length bv) 64) (bytevector-u8-set! bv 64 0))
                  bv)))
         (check (format "~a: ~a written as the oracle's" name f)
                (pcx (format "~a/~a/oracle/~a" dir name f))
                (pcx (format "~a/~a/port/~a" dir name f))))
       (filter (lambda (f) (and (> (string-length f) 4)
                                (string=? (substring f (- (string-length f) 4) (string-length f)) ".pcx")))
               (directory-list (format "~a/~a/oracle" dir name))))
      ;; The savegames: the oracle's last file of each name (the port's last
      ;; save of that name).
      (for-each
       (lambda (file)
         (let* ([s (assoc file (reverse saves))]
                [theirs (read-raw (format "~a/~a/oracle/~a" dir name file))])
           (check (format "~a: ~a (~a bytes) matches the oracle's but for undefined bytes"
                          name file (bytevector-length (cadr s)))
                  #f
                  (savegame-diff (cadr s) (caddr s) theirs))))
       (remove-duplicates (map car saves)))
      (unless (null? candidates)
        (run-oracle name candidates)
        (let ([bad (filter-map (lambda (f)
                             (let ([u (unexplained
                                       (read-raw (format "~a/~a-port/f~4,'0d.raw" dir name f))
                                       (read-raw (format "~a/~a-oracle/f~4,'0d.raw" dir name f))
                                       (read-raw (format "~a/~a-port/f~4,'0d.mask" dir name f)))])
                               (and (> u 0) (list f u))))
                           candidates)])
          (check (format "~a: ~a frames differ only in garbage reads ~a" name (length candidates)
                         (if (> (length candidates) 8) "" candidates))
                 '()
                 ;; (frame unexplained-pixels), the first few
                 (if (> (length bad) 5) (append (list-head bad 5) '(...)) bad)))))))

;; VIEW=name dumps all the oracle's frames of a case (for writing scripts).
(let ([name (getenv "CASE")] [view (getenv "VIEW")])
  (cond
    [view (run-oracle view 'all)]
    [name (run-case name)]
    [else
      (for-each
       (lambda (name)
         (unless (= 0 (system (format "CASE=~a scheme --libdirs src --compile-imported-libraries --script tests/frames.ss" name)))
           (set! failures (+ failures 1))))
       case-names)]))
(finish)
