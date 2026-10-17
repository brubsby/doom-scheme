;;; The display options against the normal loop: the title loop (the three
;;; demos, with monsters, doors and lifts) to gametic 4000, with fake time,
;;; recording the simulation after every tic: normally, -uncapped (with
;;; time running slower, so that many frames come between tics), -hires 2,
;;; and -hires 4 with -uncapped. Neither option may touch the simulation
;;; (interpolation moves things only while a frame is drawn), so every tic
;;; must match; the uncapped runs must draw interpolated frames, and the
;;; hi-res ones run the renderer at 640x400 and 1280x800 through all
;;; three demos.
(import (chezscheme) (doom d_main) (doom doomstat) (doom i_system) (doom i_video)
        (doom trace) (doom r_interp))
(load "tests/lib.ss")

(define stop 4000)

;; The cases: name, arguments, fake time (tics per call), interpolated?
(define cases
  '(("normal" ("-nosound") #t #f)
    ("uncapped" ("-uncapped" "-nosound") 8 #t)
    ("hires2" ("-hires" "2" "-nosound") #t #f)
    ("hires4" ("-hires" "4" "-uncapped" "-nosound") 8 #t)))

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

(define (result-file name) (format "build/test/display-~a.txt" name))

(let ([which (getenv "DISPLAY_CASE")])
  (if which
      (let* ([c (assoc which cases)]
             [r (run (cadr c) (caddr c))])
        (with-output-to-file (result-file which)
          (lambda ()
            (write (list (map (lambda (e) (sim (cdr e))) (car r)) (cadr r) (caddr r))))
          'replace))
      (begin
        (system "mkdir -p build/test")
        (for-each (lambda (c)
                    (system (format "rm -f ~a; DISPLAY_CASE=~a scheme --libdirs src --compile-imported-libraries --script tests/display.ss"
                                    (result-file (car c)) (car c))))
                  cases)
        (let ([normal (with-input-from-file (result-file "normal") read)])
          (for-each
           (lambda (c)
             (let ([r (and (file-exists? (result-file (car c)))
                           (with-input-from-file (result-file (car c)) read))])
               (check (format "~a: ~a tics, the same as normal" (car c) (length (car normal)))
                      #f
                      (if (not r)
                          'no-result
                          (let loop ([a (car normal)] [b (car r)])
                            (cond [(or (null? a) (null? b))
                                   (and (not (= (length (car normal)) (length (car r))))
                                        (list 'lengths (length (car normal)) (length (car r))))]
                                  [(equal? (car a) (car b)) (loop (cdr a) (cdr b))]
                                  [else (list 'first-difference (car a) (car b))]))))
               (when (and r (cadddr c))
                 (check (format "~a: drew interpolated frames (~a of ~a)" (car c) (caddr r) (cadr r))
                        #t (> (caddr r) 100)))))
           (cdr cases))
          (finish)))))
