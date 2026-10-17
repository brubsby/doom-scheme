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
;;; CASE=name runs one case; without it, each case runs in its own process.
(import (chezscheme) (doom d_main) (doom doomdef) (doom doomstat) (doom d_event)
        (doom i_system) (doom i_video) (doom trace) (doom v_video) (doom r_local)
        (doom p_local))
(load "tests/lib.ss")

;; Key codes for scripts.
(define ESC 27) (define ENTER 13) (define UP 173) (define DOWN 175) (define LEFT 172)
(define RIGHT 174) (define BKSP 127) (define F1 187) (define F2 188) (define F3 189) (define F4 190)
(define F5 191) (define F7 193) (define F8 194) (define F10 196) (define F11 215)
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

;; The cases: name, arguments, keys, exits, and when to stop (max
;; gametic, or #f for the oracle's natural end).
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
     ((10 . n)) 2300)))

(define case-names (map car cases))

(define (case-ref name) (assoc name cases))

(define dir "build/test/frames")

;; Run the oracle on a case, writing dir/NAME.trace. With raw-frames (a
;; list, or 'all), also dump those frames as dir/NAME-oracle/fNNNN.raw.
(define (run-oracle name raw-frames)
  (apply
   (lambda (name args keys exits maxgametic)
     (let ([home (format "~a/home-~a" dir name)]
           [rawdir (format "~a/~a-oracle" dir name)])
       (system (format "rm -rf ~a && mkdir -p ~a ~a" home home rawdir))
       (system
        (format "cd ~a && env -i HOME=$PWD/home-~a DOOMWADDIR=~a/wads ORACLE_TRACE=$PWD/~a.trace ORACLE_FAKETIME=1 ~a ~a ~a ~a setarch -R ~a/build/oracle/doom-oracle ~a > ~a.log 2>&1"
                dir name (current-directory) name
                (if maxgametic (format "ORACLE_MAX_GAMETIC=~a" maxgametic) "")
                (if (null? keys) "" (format "ORACLE_KEYS='~a'" (script->string keys)))
                (if (null? exits)
                    ""
                    (format "ORACLE_EXIT='~a'"
                            (apply string-append
                                   (map (lambda (e) (format "~a:~a;" (car e) (cdr e))) exits))))
                (if (pair? raw-frames)
                    (format "ORACLE_RAW_DIR=$PWD/~a-oracle ORACLE_RAW_FRAMES=~a" name
                            (apply string-append
                                   (cons (number->string (car raw-frames))
                                         (map (lambda (f) (format ",~a" f)) (cdr raw-frames)))))
                    (if raw-frames (format "ORACLE_RAW_DIR=$PWD/~a-oracle" name) ""))
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
;; garbage-candidates), where a candidate is a frame differing only in
;; framehash (saved with its mask as dir/NAME-port/fNNNN.{raw,mask}).
(define (run-port name theirs)
  (apply
   (lambda (name args keys exits maxgametic)
     (let ([mask (make-bytevector (* SCREENWIDTH SCREENHEIGHT) 0)]
           [n 0] [mismatch #f] [candidates '()]
           [portdir (format "~a/~a-port" dir name)])
       (system (format "rm -rf ~a && mkdir -p ~a" portdir portdir))
       (set! headless #t)
       (set! i-fake-time #t)
       (set! garbage-mask mask)
       (set! d-frame-hook
             (lambda ()
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
               (when (or mismatch (= n (vector-length theirs)))
                 (i-quit))
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
       (parameterize ([current-output-port (open-output-string)]
                      [current-error-port (open-output-string)])
         (d-run args))
       (values n mismatch (reverse candidates))))
   (case-ref name)))

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
  (run-oracle name #f)
  (let ([theirs (read-trace (format "~a/~a.trace" dir name))])
    (let-values ([(n mismatch candidates) (run-port name theirs)])
      (check (format "~a: ~a frames match (all fields)" name (vector-length theirs)) #f mismatch)
      (check (format "~a: same number of frames" name) (vector-length theirs) n)
      (unless (null? candidates)
        (run-oracle name candidates)
        (let ([bad (filter (lambda (f)
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
         (unless (= 0 (system (format "CASE=~a scheme --libdirs src --script tests/frames.ss" name)))
           (set! failures (+ failures 1))))
       case-names)]))
(finish)
