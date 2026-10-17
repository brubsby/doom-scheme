;;; -uncapped against the normal loop: the title loop (DEMO1 with its
;;; monsters, doors and lifts) to gametic 4000, with fake time, once
;;; normally and once uncapped (with time running slower, so that many
;;; frames come between tics), recording the simulation after every tic.
;;; Interpolation moves things only for the duration of a frame's drawing,
;;; so every tic must match, while the uncapped run draws interpolated
;;; frames.
(import (chezscheme) (doom d_main) (doom doomstat) (doom i_system) (doom i_video)
        (doom trace) (doom r_interp))
(load "tests/lib.ss")

(define stop 4000)

;; One run (in this process): ((gametic . fields) ...), frames, interpolated.
(define (run args faketime)
  (let ([table '()] [frames 0])
    (set! headless #t)
    (set! i-fake-time faketime)
    (set! d-tic-hook
          (lambda ()
            (set! table (cons (cons gametic (trace-fields 0 0)) table))
            (when (>= gametic stop) (raise 'stop))))
    (set! d-frame-hook (lambda () (set! frames (+ frames 1))))
    (guard (c [(eq? c 'stop) (void)])
      (parameterize ([current-output-port (open-output-string)])
        (d-run args)))
    (list (reverse table) frames interp-frames)))

(define (field-index name)
  (let loop ([l trace-field-names] [i 0])
    (if (eq? (car l) name) i (loop (cdr l) (+ i 1)))))
;; The simulation's fields (not the frame, palette or menu random index).
(define (sim fields)
  (map (lambda (n) (list-ref fields (field-index n)))
       '(gametic gamestate leveltime prndindex px py pz pangle pmomx pmomy health armor
         nmobjs mobjhash sectorhash)))

(let ([which (getenv "UNCAPPED_CASE")])
  (if which
      (let ([r (if (string=? which "on")
                   ;; time slow enough that many frames see no new tic
                   (run '("-uncapped" "-nosound") 8)
                   (run '("-nosound") #t))])
        (with-output-to-file (format "build/test/uncapped-~a.txt" which)
          (lambda ()
            (write (list (map (lambda (e) (sim (cdr e))) (car r)) (cadr r) (caddr r))))
          'replace))
      (begin
        (system "mkdir -p build/test")
        (for-each (lambda (w)
                    (system (format "UNCAPPED_CASE=~a scheme --libdirs src --compile-imported-libraries --script tests/uncapped.ss" w)))
                  '("off" "on"))
        (let ([off (with-input-from-file "build/test/uncapped-off.txt" read)]
              [on (with-input-from-file "build/test/uncapped-on.txt" read)])
          (check (format "~a tics, the same with -uncapped" (length (car off)))
                 #f
                 (let loop ([a (car off)] [b (car on)])
                   (cond [(or (null? a) (null? b)) (and (not (= (length (car off)) (length (car on))))
                                                        (list 'lengths (length (car off)) (length (car on))))]
                         [(equal? (car a) (car b)) (loop (cdr a) (cdr b))]
                         [else (list 'first-difference (car a) (car b))])))
          (check (format "-uncapped drew interpolated frames (~a of ~a)" (caddr on) (cadr on))
                 #t (> (caddr on) 100))
          (finish)))))
