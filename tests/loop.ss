;;; The game loop against the oracle: D_DoomMain, D_DoomLoop, TryRunTics,
;;; the title / demo sequence, and -timedemo, run through the port's own
;;; d-run, comparing the simulation at each gametic.
;;;
;;; - attract: no arguments, so the title loop (title, DEMO1, credits,
;;;   DEMO2, HELP2, DEMO3, title...), with fake time (one tic per
;;;   I_GetTime call) in both, to gametic 12000. The oracle traces once per
;;;   frame (several tics), so we compare at its frames' gametics.
;;; - timedemo: -timedemo demoN, every tic.
;;;
;;; Fields are the simulation's (as tests/sim.ss); with no input and no
;;; front end, rndindex and the frame are not compared.
(import (chezscheme) (doom d_main) (doom doomstat) (doom i_system) (doom trace))
(load "tests/lib.ss")

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

;; Run the port with args, recording the selected fields after every tic
;; (until gametic `stop`, if given). Returns (values exit-code table).
(define (run-port args stop)
  (let ([table (make-eqv-hashtable)])
    (set! d-tic-hook
          (lambda ()
            (hashtable-set! table gametic (select (trace-fields 0 0)))
            (when (and stop (>= gametic stop))
              (i-quit))))
    (let ([rc (parameterize ([current-output-port (open-output-string)]
                             [current-error-port (open-output-string)])
                (d-run args))])
      (set! d-tic-hook #f)
      (values rc table))))

;; First oracle line whose gametic's state differs, or #f.
(define (compare theirs table)
  (let loop ([l theirs] [n 0])
    (cond
      [(null? l) (values n #f)]
      [else
       (let* ([t (select (car l))]
              [o (hashtable-ref table (car t) #f)])
         (cond
           [(not o) (values n (list 'no-port-state-at-gametic (car t)))]
           [(equal? o t) (loop (cdr l) (+ n 1))]
           [else
            (values n (list 'gametic (car t) 'fields
                            (filter (lambda (x) x)
                                    (map (lambda (name a b) (and (not (equal? a b)) (list name a b)))
                                         compared-fields o t))))]))])))

(set! nodrawers #t)
(set! i-fake-time #t)

;; Each case runs in its own process, as each oracle run does (a game
;; starts from fresh globals).
(define (run-case name)
  (if (string=? name "attract")
      (let* ([theirs (read-trace "build/oracle/attract.trace")]
             [last (car (car (last-pair theirs)))])
        (let-values ([(rc table) (run-port '() last)])
          (let-values ([(n mismatch) (compare theirs table)])
            (check (format "attract loop: ~a frames to gametic ~a match" n last) #f mismatch))))
      (let ([theirs (read-trace (format "build/oracle/~a.trace" name))])
        (let-values ([(rc table) (run-port (list "-timedemo" name "-nodraw") #f)])
          (check (format "-timedemo ~a ends with the timing error" name) 255 rc)
          (let-values ([(n mismatch) (compare theirs table)])
            (check (format "-timedemo ~a: ~a tics match" name n) #f mismatch)
            (check (format "-timedemo ~a: same number of tics" name)
                   (length theirs) (hashtable-size table)))))))

(let ([name (getenv "LOOP_CASE")])
  (if name
      (run-case name)
      (for-each
       (lambda (name)
         (unless (= 0 (system (format "LOOP_CASE=~a scheme --libdirs src --script tests/loop.ss" name)))
           (set! failures (+ failures 1))))
       '("attract" "demo1" "demo2" "demo3"))))
(finish)
