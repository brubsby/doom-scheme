;;; Netgames between the port and the oracle: one node is linuxdoom, the
;;; other the port, over UDP on this machine (127.0.0.1 and 127.0.0.2; each
;;; binds its own address, see net-bind-address and ORACLE_NETBIND). Both
;;; run with fake time and scripted keys, each for its own player. The game
;;; is lockstep, so both must have the same simulation at every gametic:
;;; linuxdoom's own consistency check (each node sends its player's x with
;;; its commands) would stop the game otherwise, and here every line the
;;; oracle traces is compared with the port's state at that gametic.
;;;
;;; - coop: the oracle is the key player (player 1, who sends the game's
;;;   settings), the port player 2, both moving and shooting on E1M1;
;;;   the port's player chats.
;;; - dup: coop with -dup 2 and -extratic.
;;; - reborn: coop on E1M2 at nightmare, where the players die and respawn.
;;; - deathmatch: the port is the key player, with -deathmatch, and the two
;;;   players hunt each other.
(import (chezscheme) (doom d_main) (doom doomstat) (doom i_system) (doom i_video)
        (doom d_event) (doom trace) (doom i_net))
(load "tests/lib.ss")

(define UP 173) (define LEFT 172) (define RIGHT 174) (define FIRE 157) (define USE 32)
(define ENTER 13)
(define (chars s) (map char->integer (string->list s)))

(define compared-fields
  '(gametic gamestate leveltime prndindex px py pz pangle pmomx pmomy health armor nmobjs
    mobjhash sectorhash))
(define (field-index name)
  (let loop ([l trace-field-names] [i 0])
    (if (eq? (car l) name) i (loop (cdr l) (+ i 1)))))
(define (select fields)
  (map (lambda (name) (list-ref fields (field-index name))) compared-fields))

(define (read-trace path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (p)
          (let loop ([acc '()])
            (let ([line (get-line p)])
              (if (eof-object? line)
                  (reverse acc)
                  (loop (cons (with-input-from-string (string-append "(" line ")") read) acc)))))))
      '()))

;; Keys script -> ORACLE_KEYS string (as tests/frames.ss).
(define (script->string script)
  (let loop ([l script] [acc ""])
    (if (null? l)
        acc
        (loop (cdr l)
              (string-append acc (if (string=? acc "") "" ";") (number->string (caar l)) ":"
                             (let keys ([ks (cdar l)] [s ""])
                               (if (null? ks)
                                   s
                                   (keys (cdr ks)
                                         (string-append s (if (string=? s "") "" ",")
                                                        (let ([k (car ks)])
                                                          (if (pair? k)
                                                              (format "~a~a" (car k) (cadr k))
                                                              (number->string k))))))))))))

(define dir "build/test/net")

;; name, oracle's -net player, port's, extra args (both), oracle keys, port
;; keys, gametics
(define cases
  `(("coop" 1 2 ("-warp" "1" "1" "-skill" "3")
     ((10 (+ ,UP)) (60 (- ,UP)) (61 (+ ,LEFT)) (70 (- ,LEFT)) (71 (+ ,FIRE)) (90 (- ,FIRE))
      (91 (+ ,UP)) (130 (- ,UP)))
     ((10 (+ ,RIGHT)) (20 (- ,RIGHT)) (21 (+ ,UP)) (80 (- ,UP)) (81 (+ ,FIRE)) (100 (- ,FIRE))
      (105 ,@(chars "t") ,@(chars "hello") ,ENTER) (110 (+ ,USE)) (112 (- ,USE)))
     1500)
;; -dup 2 (each command runs two tics) and -extratic (every packet
    ;; resends the previous tic too).
    ("dup" 1 2 ("-warp" "1" "1" "-skill" "3" "-dup" "2" "-extratic")
     ((10 (+ ,UP)) (40 (- ,UP)) (41 (+ ,FIRE)) (60 (- ,FIRE)) (61 (+ ,LEFT)) (70 (- ,LEFT)))
     ((10 (+ ,LEFT)) (20 (- ,LEFT)) (21 (+ ,UP)) (50 (- ,UP)) (51 (+ ,FIRE)) (70 (- ,FIRE)))
     1500)
    ;; Nightmare on E1M2: the players stand about until the monsters kill
    ;; them, and respawn (use), again and again.
    ("reborn" 1 2 ("-warp" "1" "2" "-skill" "5")
     ,(apply append
             (map (lambda (f) `((,f (+ ,FIRE)) (,(+ f 5) (- ,FIRE)) (,(+ f 20) (+ ,USE)) (,(+ f 22) (- ,USE))))
                  (map (lambda (i) (+ 10 (* 25 i))) (iota 110))))
     ,(apply append
             (map (lambda (f) `((,f (+ ,LEFT)) (,(+ f 3) (- ,LEFT)) (,(+ f 12) (+ ,USE)) (,(+ f 14) (- ,USE))))
                  (map (lambda (i) (+ 12 (* 25 i))) (iota 110))))
     3000)
    ("deathmatch" 2 1 ("-warp" "1" "1" "-skill" "3" "-deathmatch")
     ((10 (+ ,LEFT)) (40 (- ,LEFT)) (41 (+ ,FIRE)) (41 (+ ,UP)) (120 (- ,UP))
      (130 (+ ,RIGHT)) (136 (- ,RIGHT)) (200 (+ ,LEFT)) (203 (- ,LEFT)) (260 (+ ,UP)) (300 (- ,UP))
      (400 (- ,FIRE)) (401 (+ ,USE)) (402 (- ,USE)) (410 (+ ,FIRE)) (600 (- ,FIRE)) (601 (+ ,USE))
      (602 (- ,USE)) (610 (+ ,FIRE)))
     ((10 (+ ,RIGHT)) (30 (- ,RIGHT)) (31 (+ ,FIRE)) (31 (+ ,UP)) (100 (- ,UP))
      (150 (+ ,LEFT)) (154 (- ,LEFT)) (220 (+ ,RIGHT)) (224 (- ,RIGHT)) (300 (+ ,UP)) (340 (- ,UP))
      (400 (- ,FIRE)) (401 (+ ,USE)) (402 (- ,USE)) (410 (+ ,FIRE)) (600 (- ,FIRE)) (601 (+ ,USE))
      (602 (- ,USE)) (610 (+ ,FIRE)))
     4000)))

(define (address n) (format "127.0.0.~a" n))

(define (run-case name)
  (let ([c (assoc name cases)])
    (run-case* name (list-ref c 1) (list-ref c 2) (list-ref c 3) (list-ref c 4) (list-ref c 5)
               (list-ref c 6))))

(define (run-case* name oplayer pplayer args okeys pkeys stop)
     (let* ([rundir (format "~a/~a" dir name)]
            [otrace (format "~a/~a/oracle.trace" (current-directory) rundir)])
       (system (format "rm -rf ~a && mkdir -p ~a/home ~a/port-home" rundir rundir rundir))
       ;; the oracle, in the background
       (let ([oracle
              (process
               (format "cd ~a && exec env -i HOME=$PWD/home DOOMWADDIR=~a/wads ORACLE_TRACE=~a ORACLE_FAKETIME=1 ORACLE_MAX_GAMETIC=~a ORACLE_NETBIND=~a ORACLE_KEYS='~a' timeout 120 setarch -R ~a/build/oracle/doom-oracle -net ~a .~a~a > oracle.log 2>&1"
                       rundir (current-directory) otrace stop (address oplayer) (script->string okeys)
                       (current-directory) oplayer (address pplayer)
                       (apply string-append (map (lambda (a) (string-append " " a)) args))))])
         ;; the port
         (let ([table (make-eqv-hashtable)] [n 0] [result #f] [started (time-second (current-time))])
           (set! headless #t)
           (set! i-fake-time #t)
           (set! net-bind-address (address pplayer))
           (set! d-tic-hook
                 (lambda ()
                   (hashtable-set! table gametic (select (trace-fields 0 0)))
                   (when (>= gametic stop) (raise 'stop))))
           (set! d-frame-hook
                 (lambda ()
                   (set! n (+ n 1))
                   ;; (a stuck game fails rather than hangs)
                   (when (> (- (time-second (current-time)) started) 180)
                     (raise 'timeout))
                   (for-each (lambda (k)
                               (let ([down (lambda (k) (d-post-event (make-event ev_keydown k 0 0)))]
                                     [up (lambda (k) (d-post-event (make-event ev_keyup k 0 0)))])
                                 (cond
                                   [(not (pair? k)) (down k) (up k)]
                                   [(eq? (car k) '+) (down (cadr k))]
                                   [else (up (cadr k))])))
                             (apply append (map cdr (filter (lambda (e) (= n (car e))) pkeys))))))
           (putenv "DOOMWADDIR" (format "~a/wads" (current-directory)))
           (putenv "HOME" (format "~a/~a/port-home" (current-directory) rundir))
           (set! result
                 (guard (c [(eq? c 'stop) 'stopped]
                           [#t (with-output-to-string (lambda () (display-condition c)))])
                   (with-output-to-file (format "~a/port.log" rundir)
                     (lambda () (d-run (append (list "-net" (number->string pplayer)
                                                     (string-append "." (address oplayer)))
                                               args)))
                     'replace)))
           ;; wait for the oracle (it stops at the same gametic)
           (let ([in (car oracle)])
             (get-string-all in)
             (close-port in))
           (let* ([theirs (read-trace otrace)]
                  [mismatch
                   (let loop ([l theirs])
                     (cond
                       [(null? l) #f]
                       [else
                        (let* ([t (select (car l))] [o (hashtable-ref table (car t) #f)])
                          (cond
                            [(= (car t) 0) (loop (cdr l))] ; (before the first tic)
                            [(> (car t) stop) (loop (cdr l))] ; (the port stopped)
                            [(not o) (list 'no-port-state-at-gametic (car t))]
                            [(equal? o t) (loop (cdr l))]
                            [else (list 'gametic (car t)
                                        (filter (lambda (x) x)
                                                (map (lambda (nm a b) (and (not (equal? a b)) (list nm a b)))
                                                     compared-fields o t)))]))]))])
             (check (format "~a: the port ran to gametic ~a" name stop) 'stopped result)
             (check (format "~a: the oracle ran to gametic ~a" name stop) #t
                    (and (pair? theirs) (>= (car (list-ref theirs (- (length theirs) 1))) stop)))
             (check (format "~a: ~a oracle frames, the same simulation in the port" name (length theirs))
                    #f mismatch))))))

(let ([name (getenv "NET_CASE")])
  (if name
      (run-case name)
      (for-each
       (lambda (c)
         (unless (= 0 (system (format "NET_CASE=~a scheme --libdirs src --compile-imported-libraries --script tests/net.ss" (car c))))
           (set! failures (+ failures 1))))
       cases)))
(finish)
