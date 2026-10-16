;;; G_BuildTiccmd against the oracle's own (build/oracle/ticcmd_probe):
;;; 3000 tics of scripted keyboard, mouse and joystick input, with keys held
;;; for runs of tics (for accelerative turning) and mouse buttons toggling
;;; (for double clicks), compared field by field.
(import (chezscheme) (doom d_main) (doom d_ticcmd) (doom doomdef))
(load "tests/lib.ss")

(define TICS 3000)

;; A small deterministic generator, so the script is the same every run.
(define seed 12345)
(define (rand n)
  (set! seed (mod (+ (* seed 1103515245) 12345) 2147483648))
  (mod (quotient seed 65536) n))

(define game-keys
  (append (list KEY_RIGHTARROW KEY_LEFTARROW KEY_UPARROW KEY_DOWNARROW KEY_RCTRL KEY_RALT
                KEY_RSHIFT (char->integer #\,) (char->integer #\.) (char->integer #\space))
          (map (lambda (c) (char->integer c)) (string->list "123456789"))))

;; Each line: keys held, mousex, mousey, mouse buttons, joy x, joy y, joy
;; buttons. Inputs change every few tics.
(define script
  (let loop ([t 0] [acc '()] [keys '()] [mb 0] [jx 0] [jy 0] [jb 0] [left 0])
    (if (= t TICS)
        (reverse acc)
        (let-values ([(keys mb jx jy jb left)
                      (if (> left 0)
                          (values keys mb jx jy jb (- left 1))
                          (values (filter (lambda (k) (< (rand 100) 15)) game-keys)
                                  (if (< (rand 100) 40) (rand 8) 0)
                                  (if (< (rand 100) 20) (- (rand 3) 1) 0)
                                  (if (< (rand 100) 20) (- (rand 3) 1) 0)
                                  (if (< (rand 100) 20) (rand 16) 0)
                                  (rand 12)))])
          (let ([mx (if (< (rand 100) 30) (- (rand 41) 20) 0)]
                [my (if (< (rand 100) 30) (- (rand 41) 20) 0)])
            (loop (+ t 1) (cons (list keys mx my mb jx jy jb) acc)
                  keys mb jx jy jb left))))))

(define (script-line l)
  (apply (lambda (keys mx my mb jx jy jb)
           (format "~a ~a ~a ~a ~a ~a ~a"
                   (if (null? keys) "-" (apply string-append
                                               (cons (number->string (car keys))
                                                     (map (lambda (k) (format ",~a" k)) (cdr keys)))))
                   mx my mb jx jy jb))
         l))

(unless (file-exists? "build/test") (mkdir "build/test"))
(call-with-output-file "build/test/ticcmd-script.txt"
  (lambda (p) (for-each (lambda (l) (put-string p (script-line l)) (newline p)) script))
  'replace)
(system "build/oracle/ticcmd_probe < build/test/ticcmd-script.txt > build/test/ticcmd-oracle.txt")
(define theirs
  (call-with-input-file "build/test/ticcmd-oracle.txt"
    (lambda (p)
      (let loop ([acc '()])
        (let ([line (get-line p)])
          (if (eof-object? line)
              (reverse acc)
              (loop (cons (with-input-from-string (string-append "(" line ")") read) acc))))))))

(m-load-defaults)
;; (in order: G_BuildTiccmd keeps state between tics, and map's order of
;; application is unspecified)
(define (map-in-order f l)
  (let loop ([l l] [acc '()])
    (if (null? l) (reverse acc) (loop (cdr l) (cons (f (car l)) acc)))))
(define ours
  (map-in-order (lambda (l)
         (apply (lambda (keys mx my mb jx jy jb)
                  (vector-fill! gamekeydown #f)
                  (for-each (lambda (k) (vector-set! gamekeydown k #t)) keys)
                  (set! mousex mx)
                  (set! mousey my)
                  (mousebuttons-set! 0 (logtest mb 1))
                  (mousebuttons-set! 1 (logtest mb 2))
                  (mousebuttons-set! 2 (logtest mb 4))
                  (set! joyxmove jx)
                  (set! joyymove jy)
                  (joybuttons-set! 0 (logtest jb 1))
                  (joybuttons-set! 1 (logtest jb 2))
                  (joybuttons-set! 2 (logtest jb 4))
                  (joybuttons-set! 3 (logtest jb 8))
                  (let ([cmd (make-ticcmd)])
                    (g-build-ticcmd cmd)
                    (list (ticcmd-forwardmove cmd) (ticcmd-sidemove cmd) (ticcmd-angleturn cmd)
                          (ticcmd-buttons cmd) (ticcmd-chatchar cmd))))
                l))
       script))

(check "oracle answered every tic" TICS (length theirs))
(check "G_BuildTiccmd matches on every tic"
       #f
       (let loop ([o ours] [t theirs] [i 1])
         (cond [(or (null? o) (null? t)) #f]
               [(equal? (car o) (car t)) (loop (cdr o) (cdr t) (+ i 1))]
               [else (list 'tic i 'ours (car o) 'oracle (car t))])))
(check "the script exercises turning, moving, buttons and weapon changes"
       '(#t #t #t #t #t)
       (list (exists (lambda (c) (not (= (caddr c) 0))) theirs)
             (exists (lambda (c) (not (= (car c) 0))) theirs)
             (exists (lambda (c) (logtest (cadddr c) 1)) theirs)
             (exists (lambda (c) (logtest (cadddr c) 2)) theirs)
             (exists (lambda (c) (logtest (cadddr c) 4)) theirs)))
(finish)
