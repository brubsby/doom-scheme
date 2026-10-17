;;; (doom opl3) against Chocolate Doom's Nuked OPL3 C code, sample for sample.
;;; Each scenario is a register script from a seeded LCG. The script goes to
;;; build/opl/NAME.txt, tools/oracle/probes/opl_probe.c (built here with the
;;; host cc if missing or stale) renders it to NAME.raw, and the Scheme port
;;; renders the same commands. The two s16le streams must be identical.
(import (chezscheme) (doom opl3))
(load "tests/lib.ss")

(define dir "build/opl")
(define probe (string-append dir "/opl_probe"))
(define probe-sources
  '("tools/oracle/probes/opl_probe.c" "reference/chocolate-doom/opl/opl3.c"
    "reference/chocolate-doom/opl/opl3.h"))

(define (newer? a b)
  (time>? (file-modification-time a) (file-modification-time b)))

(unless (file-directory? "build") (mkdir "build"))
(unless (file-directory? dir) (mkdir dir))
(when (or (not (file-exists? probe))
          (exists (lambda (src) (newer? src probe)) probe-sources))
  (unless (= 0 (system (format "cc -O2 -I reference/chocolate-doom/opl -o ~a ~a ~a"
                               probe (car probe-sources) (cadr probe-sources))))
    (printf "FAIL could not build ~a\n" probe)
    (exit 1)))

;;; Deterministic random numbers (glibc-style LCG, 31-bit state).
(define seed 1)
(define (rand n)
  (set! seed (logand (+ (* seed 1103515245) 12345) #x7fffffff))
  (mod (ash seed -8) n))
(define (chance p) (< (rand 1000) (* p 1000)))
(define (pick . xs) (list-ref xs (rand (length xs))))

;;; Script builder: commands accumulate in reverse.
(define cmds '())
(define (emit . c) (set! cmds (cons c cmds)))
(define write-cmd 'w)                   ; 'w direct, 'b buffered
(define (w reg v) (emit write-cmd reg v))
(define (gen n) (emit 'g n))

;; Channel 0-17 -> (bank offset, channel within bank, operator offset).
(define (bank ch) (if (< ch 9) 0 #x100))
(define (chn ch) (mod ch 9))
(define (op-off ch) (let ([c (chn ch)]) (+ (* 8 (quotient c 3)) (mod c 3))))

;; Random operator/channel parameters. stereo? sets the OPL3 C0 pan bits.
(define (instrument! ch stereo?)
  (for-each
   (lambda (op)
     (let ([r (+ (bank ch) op)])
       (w (+ #x20 r) (rand 256))                     ; AM VIB EGT KSR MULT
       (w (+ #x40 r) (if (= op (op-off ch))          ; KSL TL; carrier louder
                         (rand 256)
                         (logior (ash (rand 4) 6) (rand 24))))
       (w (+ #x60 r) (logior (ash (pick (rand 16) 15 (+ 8 (rand 8))) 4) (rand 16)))
       (w (+ #x80 r) (rand 256))                     ; SL RR
       (w (+ #xe0 r) (rand 8))))                     ; waveform
   (list (op-off ch) (+ (op-off ch) 3)))
  (w (+ #xc0 (bank ch) (chn ch))
     (logior (rand 16) (if stereo? (pick #x00 #x10 #x20 #x30 #x30) #x30))))

(define (note-on! ch)
  (let ([fnum (rand 1024)] [block (rand 8)])
    (w (+ #xa0 (bank ch) (chn ch)) (logand fnum #xff))
    (w (+ #xb0 (bank ch) (chn ch)) (logior #x20 (ash block 2) (ash fnum -8)))))
(define (note-off! ch)
  (w (+ #xb0 (bank ch) (chn ch)) (rand #x20)))

(define (depth-bits) (logand (rand 256) #xc0))

;;; Scenarios. Each returns the command list for one sample rate.
(define (script rate body)
  (set! cmds '())
  (set! write-cmd 'w)
  (emit 'r rate)
  (body)
  (reverse cmds))

;; OPL2 melodic: all waveforms, feedback/connection, KSL/TL, ADSR, AM/VIB with
;; both 0xBD depth settings, NTS, key-off and release.
(define (melodic rate)
  (script rate
    (lambda ()
      (w #x01 #x20)
      (do ([ch 0 (+ ch 1)]) ((= ch 9)) (instrument! ch #f))
      (do ([i 0 (+ i 1)]) ((= i 120))
        (let ([ch (rand 9)])
          (cond [(chance 0.45) (when (chance 0.3) (instrument! ch #f)) (note-on! ch)]
                [(chance 0.5) (note-off! ch)]
                [(chance 0.4) (w #xbd (depth-bits))]
                [(chance 0.3) (w #x08 (logand (rand 256) #x40))]
                [else (w (+ #xe0 (op-off ch) (pick 0 3)) (rand 8))]))
        (gen (+ 50 (rand 3000)))))))

;; Rhythm mode: drum key bits, drum channel instruments, switching rhythm off
;; and on with melodic notes on channels 6-8 in between.
(define (rhythm rate)
  (script rate
    (lambda ()
      (do ([ch 0 (+ ch 1)]) ((= ch 9)) (instrument! ch #f))
      (for-each note-on! '(6 7 8))
      (do ([i 0 (+ i 1)]) ((= i 150))
        (cond [(chance 0.6) (w #xbd (logior #x20 (depth-bits) (rand 32)))]
              [(chance 0.2) (instrument! (pick 6 7 8) #f)]
              [(chance 0.2) (let ([ch (pick 6 7 8)])
                              (w (+ #xa0 ch) (rand 256))
                              (w (+ #xb0 ch) (rand 64)))]
              [(chance 0.3) (w #xbd (depth-bits)) (note-on! (pick 6 7 8))]
              [else (let ([ch (rand 6)]) (if (chance 0.5) (note-on! ch) (note-off! ch)))])
        (gen (+ 50 (rand 2500)))))))

;; OPL3 mode: both banks, 4-op pairs (0x104), all 8 waveforms, pan bits.
(define (opl3 rate)
  (script rate
    (lambda ()
      (w #x105 1)
      (w #x104 (rand 64))
      (do ([ch 0 (+ ch 1)]) ((= ch 18)) (instrument! ch #t))
      (do ([i 0 (+ i 1)]) ((= i 150))
        (let ([ch (rand 18)])
          (cond [(chance 0.45) (when (chance 0.3) (instrument! ch #t)) (note-on! ch)]
                [(chance 0.45) (note-off! ch)]
                [(chance 0.3) (w #x104 (rand 64))]
                [(chance 0.3) (w (+ #xc0 (bank ch) (chn ch)) (rand 256))]
                [(chance 0.3) (w #xbd (logior (depth-bits) (if (chance 0.3) (+ 32 (rand 32)) 0)))]
                [else (w #x105 (if (chance 0.2) 0 1))]))
        (gen (+ 50 (rand 3000)))))))

;; Ten-plus seconds: everything mixed, notes every 1000-5000 samples, some
;; writes buffered.
(define (long rate)
  (script rate
    (lambda ()
      (w #x105 1)
      (do ([ch 0 (+ ch 1)]) ((= ch 18)) (instrument! ch #t))
      (let loop ([frames 0])
        (when (< frames (* 11 rate))
          (set! write-cmd (if (chance 0.2) 'b 'w))
          (let ([ch (rand 18)])
            (cond [(chance 0.5) (when (chance 0.25) (instrument! ch #t)) (note-on! ch)]
                  [(chance 0.6) (note-off! ch)]
                  [(chance 0.3) (w #xbd (logior (depth-bits) (if (chance 0.3) (+ 32 (rand 32)) 0)))]
                  [else (w #x104 (rand 64))]))
          (let ([n (+ 1000 (rand 4000))])
            (gen n)
            (loop (+ frames n))))))))

;; Buffered writes, including a burst that overflows the 1024-entry ring.
(define (buffered rate)
  (script rate
    (lambda ()
      (set! write-cmd 'b)
      (do ([ch 0 (+ ch 1)]) ((= ch 9)) (instrument! ch #f))
      (do ([i 0 (+ i 1)]) ((= i 80))
        (let ([ch (rand 9)])
          (if (chance 0.6) (note-on! ch) (note-off! ch)))
        (when (= i 40)
          (do ([j 0 (+ j 1)]) ((= j 1100)) (w (+ #xa0 (rand 9)) (rand 256))))
        (gen (+ 1 (rand (pick 4 50 3000))))))))

;; Random writes to every register, 0x000-0x1ff.
(define (fuzz rate)
  (script rate
    (lambda ()
      (do ([i 0 (+ i 1)]) ((= i 400))
        (do ([j (rand 20) (- j 1)]) ((= j 0))
          (w (rand #x200) (rand 256)))
        (gen (+ 1 (rand 600)))))))

;;; Run a command list through the Scheme port; returns the s16le stream.
(define (frames-of cmds)
  (fold-left (lambda (n c) (if (eq? (car c) 'g) (+ n (cadr c)) n)) 0 cmds))

(define (run-scheme cmds)
  (let ([bv (make-bytevector (* 4 (frames-of cmds)))] [chip (opl3-make)])
    (let loop ([cs cmds] [off 0])
      (unless (null? cs)
        (let ([c (car cs)])
          (case (car c)
            [(r) (opl3-reset! chip (cadr c)) (loop (cdr cs) off)]
            [(w) (opl3-write-reg! chip (cadr c) (caddr c)) (loop (cdr cs) off)]
            [(b) (opl3-write-reg-buffered! chip (cadr c) (caddr c)) (loop (cdr cs) off)]
            [(g) (opl3-generate-stream! chip bv off (cadr c))
                 (loop (cdr cs) (+ off (* 4 (cadr c))))]))))
    bv))

(define (run-c name cmds)
  (let ([txt (format "~a/~a.txt" dir name)] [raw (format "~a/~a.raw" dir name)])
    (call-with-output-file txt
      (lambda (p)
        (for-each (lambda (c)
                    (fprintf p "~a~{ ~a~}\n" (car c) (cdr c)))
                  cmds))
      'replace)
    (unless (= 0 (system (format "~a ~a ~a" probe txt raw)))
      (printf "FAIL ~a: probe failed\n" name)
      (exit 1))
    (call-with-port (open-file-input-port raw) get-bytevector-all)))

(define (first-diff a b)
  (let ([n (min (bytevector-length a) (bytevector-length b))])
    (let loop ([i 0])
      (cond [(>= i n) #f]
            [(= (bytevector-u8-ref a i) (bytevector-u8-ref b i)) (loop (+ i 1))]
            [else (quotient i 4)]))))

(define (frame bv f)
  (if (<= (* 4 (+ f 1)) (bytevector-length bv))
      (list (bytevector-s16-ref bv (* 4 f) (endianness little))
            (bytevector-s16-ref bv (+ 2 (* 4 f)) (endianness little)))
      'none))

(define (nonzero-frames bv)
  (do ([i 0 (+ i 4)]
       [n 0 (if (and (= 0 (bytevector-u16-ref bv i (endianness little)))
                     (= 0 (bytevector-u16-ref bv (+ i 2) (endianness little))))
                n (+ n 1))])
      ((>= i (bytevector-length bv)) n)))

(define (compare name gen-script rate)
  (set! seed (+ rate (string-length name)))
  (let* ([full (format "~a-~a" name rate)]
         [cmds (gen-script rate)]
         [expected (run-c full cmds)]
         [actual (run-scheme cmds)]
         [frames (quotient (bytevector-length expected) 4)])
    (if (bytevector=? expected actual)
        (check (format "~a: ~a frames (~,1fs, ~a non-silent) match C"
                       full frames (/ frames (exact->inexact rate))
                       (nonzero-frames expected))
               #t #t)
        (let ([f (first-diff expected actual)])
          (check (format "~a matches C" full)
                 (list 'frame f (frame expected f) 'length (bytevector-length expected))
                 (list 'frame f (frame actual f) 'length (bytevector-length actual)))))))

(for-each
 (lambda (rate)
   (compare "melodic" melodic rate)
   (compare "rhythm" rhythm rate)
   (compare "opl3" opl3 rate)
   (compare "buffered" buffered rate)
   (compare "fuzz" fuzz rate)
   (compare "long" long rate))
 '(49716 44100))

;;; Bytes [start, end) of bv.
(define (sub bv start end)
  (let ([r (make-bytevector (- end start))])
    (bytevector-copy! bv start r 0 (- end start))
    r))

;;; opl3-generate! (OPL3_Generate, no resampler): with no writes in between,
;;; the 49716 Hz resampled stream is the native one two frames late.
(let* ([setup (begin (set! seed 7)
                     (script 49716 (lambda ()
                                     (do ([ch 0 (+ ch 1)]) ((= ch 9))
                                       (instrument! ch #f)
                                       (note-on! ch)))))]
       [n 20000]
       [chip-a (opl3-make)] [chip-b (opl3-make)]
       [resampled (make-bytevector (* 4 (+ n 2)))]
       [native (make-bytevector (* 4 n))])
  (for-each (lambda (c)
              (when (eq? (car c) 'w)
                (opl3-write-reg! chip-a (cadr c) (caddr c))
                (opl3-write-reg! chip-b (cadr c) (caddr c))))
            setup)
  (opl3-generate-stream! chip-a resampled 0 (+ n 2))
  (do ([i 0 (+ i 1)]) ((= i n)) (opl3-generate! chip-b native (* 4 i)))
  (check "opl3-generate! is the 49716 Hz stream two frames early" #t
         (bytevector=? (sub resampled 8 (* 4 (+ n 2))) native)))

(define (rejects? thunk)
  (guard (e [(assertion-violation? e) #t]) (thunk) #f))
(check "bad arguments are rejected" '(#t #t #t #t)
       (list (rejects? (lambda () (opl3-write-reg! (make-fxvector 3) 1 1)))
             (rejects? (lambda () (opl3-write-reg! (opl3-make) 'x 1)))
             (rejects? (lambda () (opl3-generate-stream! (opl3-make) (make-bytevector 7) 0 2)))
             (rejects? (lambda () (opl3-reset! (opl3-make) 0)))))

;;; Timing (informational): one second at 49716 Hz.
(define (bench label setup!)
  (let ([chip (opl3-make 49716)] [bv (make-bytevector (* 4 49716))])
    (setup! chip)
    (collect)
    (let ([t0 (cpu-time)])
      (opl3-generate-stream! chip bv 0 49716)
      (printf "info ~a: 49716 frames in ~a ms CPU\n" label (- (cpu-time) t0)))))

(define (key-on-channels! chip opl3? chans)
  (when opl3? (opl3-write-reg! chip #x105 1))
  (for-each
   (lambda (ch)
     (let ([b (bank ch)] [o (op-off ch)] [c (chn ch)])
       (for-each (lambda (op)
                   (opl3-write-reg! chip (+ b #x20 op) #x01)
                   (opl3-write-reg! chip (+ b #x40 op) #x10)
                   (opl3-write-reg! chip (+ b #x60 op) #xf2)
                   (opl3-write-reg! chip (+ b #x80 op) #x24))
                 (list o (+ o 3)))
       (opl3-write-reg! chip (+ b #xc0 c) #x3e)
       (opl3-write-reg! chip (+ b #xa0 c) #x98)
       (opl3-write-reg! chip (+ b #xb0 c) (logior #x20 (ash 4 2) 1))))
   chans))

(bench "9 OPL2 channels on" (lambda (chip) (key-on-channels! chip #f (iota 9))))
(bench "18 OPL3 channels on" (lambda (chip) (key-on-channels! chip #t (iota 18))))
(bench "7 channels on, rest silent" (lambda (chip) (key-on-channels! chip #t (iota 7))))
(bench "silent" (lambda (chip) (void)))
(finish)
