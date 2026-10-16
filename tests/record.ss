;;; Demo recording, round trip through the oracle. The port runs
;;; `-record build/test/rec -warp 1 1` with scripted key events (walking,
;;; turning, firing, using, weapon changes, a pause and unpause), until the
;;; script presses q, which ends the recording. Then the oracle and the port
;;; each play build/test/rec.lmp with -timedemo. The oracle's playback must
;;; match the port's recording run tic for tic, and so must the port's own
;;; playback. (Each run is its own process; REC_CASE selects one.)
(import (chezscheme) (doom d_main) (doom doomdef) (doom doomstat) (doom d_event)
        (doom i_system) (doom trace))
(load (if (file-exists? "tests/lib.ss") "tests/lib.ss" "../../tests/lib.ss"))

(define compared-fields
  '(gametic gamestate leveltime prndindex px py pz pangle pmomx pmomy health armor nmobjs
    mobjhash sectorhash))
(define (field-index name)
  (let loop ([l trace-field-names] [i 0])
    (if (eq? (car l) name) i (loop (cdr l) (+ i 1)))))
(define (select fields)
  (map (lambda (name) (list-ref fields (field-index name))) compared-fields))

(define (read-trace path)
  (call-with-input-file path
    (lambda (p)
      (let loop ([acc '()])
        (let ([line (get-line p)])
          (if (eof-object? line)
              (reverse acc)
              (loop (cons (with-input-from-string (string-append "(" line ")") read) acc))))))))

(define (write-trace path rows)
  (call-with-output-file path
    (lambda (p)
      (for-each (lambda (r)
                  (for-each (lambda (x) (put-string p (number->string x)) (put-char p #\space)) r)
                  (newline p))
                rows))
    'replace))

;; The recording script: tic -> list of (type . key) events to post after
;; that tic. Runs of held keys change every 5-40 tics.
(define END-TIC 1500)
(define seed 777)
(define (rand n)
  (set! seed (mod (+ (* seed 1103515245) 12345) 2147483648))
  (mod (quotient seed 65536) n))
(define script
  (let ([keys (list KEY_UPARROW KEY_UPARROW KEY_DOWNARROW KEY_LEFTARROW KEY_RIGHTARROW KEY_RSHIFT
                    KEY_RALT (char->integer #\,) (char->integer #\.) KEY_RCTRL KEY_RCTRL
                    (char->integer #\space) (char->integer #\2) (char->integer #\3)
                    (char->integer #\1))]
        [table (make-eqv-hashtable)])
    (define (post! tic type key)
      (hashtable-update! table tic (lambda (l) (append l (list (cons type key)))) '()))
    (let loop ([t 1] [held '()])
      (when (< t END-TIC)
        (let* ([new (filter (lambda (k) (< (rand 100) 20)) keys)]
               [len (+ 5 (rand 36))])
          (for-each (lambda (k) (unless (memv k new) (post! t ev_keyup k))) held)
          (for-each (lambda (k) (unless (memv k held) (post! t ev_keydown k))) new)
          (loop (+ t len) new))))
    ;; a pause and unpause
    (post! 400 ev_keydown KEY_PAUSE)
    (post! 401 ev_keyup KEY_PAUSE)
    (post! 450 ev_keydown KEY_PAUSE)
    (post! 451 ev_keyup KEY_PAUSE)
    ;; press q to end demo recording
    (post! END-TIC ev_keydown (char->integer #\q))
    table))

;; Run the port, recording the selected fields after every tic.
(define (run-port args on-tic)
  (let ([rows '()])
    (set! d-tic-hook
          (lambda ()
            (set! rows (cons (select (trace-fields 0 0)) rows))
            (on-tic gametic)))
    (let ([rc (parameterize ([current-output-port (open-output-string)]
                             [current-error-port (open-output-string)])
                (d-run args))])
      (values rc (reverse rows)))))

(define (first-diff a b)
  (let loop ([a a] [b b] [i 1])
    (cond [(and (null? a) (null? b)) #f]
          [(or (null? a) (null? b)) (list 'length-differs-at i)]
          [(equal? (car a) (car b)) (loop (cdr a) (cdr b) (+ i 1))]
          [else (list 'tic i 'a (car a) 'b (car b))])))

(set! nodrawers #t)
(set! i-fake-time #t)

(define (run case) (system (format "REC_CASE=~a scheme --libdirs src --script tests/record.ss" case)))

(case (getenv "REC_CASE")
  [("record")
   (let-values ([(rc rows) (run-port '("-record" "build/test/rec" "-warp" "1" "1" "-skill" "3")
                                     (lambda (tic)
                                       (for-each (lambda (e) (d-post-event (make-event (car e) (cdr e) 0 0)))
                                                 (hashtable-ref script tic '()))))])
     (check "recording ends with \"Demo ... recorded\"" 255 rc)
     (write-trace "build/test/rec-record.trace" rows))]
  [("play")
   ;; (run from build/test: -timedemo takes a lump name, as in C)
   (let-values ([(rc rows) (run-port '("-timedemo" "rec" "-nodraw") (lambda (tic) (void)))])
     (check "playback ends with the timing error" 255 rc)
     (write-trace "rec-play.trace" rows))]
  [else
   (unless (file-exists? "build/test") (mkdir "build/test"))
   (for-each delete-file (filter file-exists? '("build/test/rec.lmp" "build/test/rec.trace")))
   (check "record run" 0 (run "record"))
   (check "demo written" #t (file-exists? "build/test/rec.lmp"))
   (check "oracle playback"
          0 (system "DEMODIR=build/test tools/oracle/run.sh rec build/test/rec.trace > /dev/null"))
   (check "port playback"
          0 (system "cd build/test && DOOMWADDIR=../../wads REC_CASE=play scheme --libdirs ../../src --script ../../tests/record.ss"))
   (let ([recorded (read-trace "build/test/rec-record.trace")]
         [oracle (map select (read-trace "build/test/rec.trace"))]
         [played (read-trace "build/test/rec-play.trace")])
     (check (format "oracle playback matches the recording run (~a tics)" (length recorded))
            #f (first-diff recorded oracle))
     (check "port playback matches the recording run" #f (first-diff recorded played))
     (check "the demo pauses" #t
            (let loop ([r recorded] [prev #f])
              (cond [(null? r) #f]
                    [(and prev (= (list-ref (car r) 2) (list-ref prev 2))) #t] ; leveltime stood still
                    [else (loop (cdr r) (car r))]))))])
(finish)
