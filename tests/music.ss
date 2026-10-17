;;; (doom i_oplmusic) against Chocolate Doom's OPL music player, byte for
;;; byte. tools/oracle/probes/music_probe.c (built here with the host cc
;;; into build/music/ if missing or stale) links the real i_oplmusic.c,
;;; mus2mid.c, midifile.c, memio.c, opl.c, opl_queue.c, opl_sdl.c and
;;; opl3.c, and renders a song with a script of pause/resume/volume/song
;;; changes to a raw s16le file. The Scheme port renders the same thing in
;;; the same mixing-callback buffers, and the two must be identical.
(import (chezscheme) (doom w_wad) (doom i_oplmusic))
(load "tests/lib.ss")

(define dir "build/music")
(define probe (string-append dir "/music_probe"))
(define C "reference/chocolate-doom")
(define probe-sources
  (append
   '("tools/oracle/probes/music_probe.c"
     "tools/oracle/probes/music/music_sdl.c"
     "tools/oracle/probes/music/stubs.c")
   (map (lambda (f) (string-append C "/" f))
        '("src/i_oplmusic.c" "src/mus2mid.c" "src/midifile.c" "src/memio.c"
          "opl/opl.c" "opl/opl_queue.c" "opl/opl3.c"))))
(define probe-deps
  (append probe-sources
          '("tools/oracle/probes/music/config.h" "tools/oracle/probes/music/SDL.h"
            "tools/oracle/probes/music/SDL_endian.h" "tools/oracle/probes/music/SDL_mixer.h")
          (map (lambda (f) (string-append C "/" f))
               '("opl/opl_sdl.c" "opl/opl3.h" "opl/opl.h" "opl/opl_queue.h"
                 "src/midifile.h" "src/mus2mid.h" "src/memio.h" "src/i_sound.h"))))

(define (newer? a b)
  (time>? (file-modification-time a) (file-modification-time b)))

(unless (file-directory? "build") (mkdir "build"))
(unless (file-directory? dir) (mkdir dir))
(when (or (not (file-exists? probe))
          (exists (lambda (src) (newer? src probe)) probe-deps))
  (unless (= 0 (system (format "cc -O2 -I tools/oracle/probes/music -I ~a/src -I ~a/opl -o ~a~{ ~a~}"
                               C C probe probe-sources)))
    (printf "FAIL could not build ~a\n" probe)
    (exit 1)))

(w-init-multiple-files (list wad-path))

;;; Synthetic songs, written to build/music/.

;; MIDI variable-length quantity.
(define (vlq n)
  (let loop ([n (ash n -7)] [acc (list (logand n #x7f))])
    (if (= n 0) acc (loop (ash n -7) (cons (logior #x80 (logand n #x7f)) acc)))))

(define (u16be n) (list (ash n -8) (logand n #xff)))
(define (u32be n) (list (ash n -24) (logand (ash n -16) #xff) (logand (ash n -8) #xff) (logand n #xff)))

;; A track from (delta byte ...) lists; the end of track is appended.
(define (mtrk events)
  (let ([body (append (apply append (map (lambda (e) (append (vlq (car e)) (cdr e))) events))
                      '(0 #xff #x2f 0))])
    (append (map char->integer (string->list "MTrk")) (u32be (length body)) body)))

(define (midi-file format division tracks)
  (u8-list->bytevector
   (append (map char->integer (string->list "MThd")) (u32be 6)
           (u16be format) (u16be (length tracks)) (u16be (logand division #xffff))
           (apply append (map mtrk tracks)))))

(define (tempo us) (list #xff #x51 3 (ash us -16) (logand (ash us -8) #xff) (logand us #xff)))

;; Type 1, two tracks: tempo changes (OPL_AdjustCallbacks), running status,
;; program changes past the main bank, pan, pitch bend, percussion and
;; out-of-range percussion keys, velocity-0 key off, SysEx, text, aftertouch,
;; all notes off, and MIDI channel 15 (DMX's channel 9).
(define tempo-midi
  (midi-file 1 96
   (list
    `((0 ,@(tempo 500000)) (0 #xff #x01 4 ,@(map char->integer (string->list "test")))
      (192 ,@(tempo 300000)) (100 #xff #x58 4 4 2 24 8) (284 ,@(tempo 750000))
      (300 ,@(tempo 420000)) (500 #xff #x01 0))
    `((0 #xc0 30) (0 #xb0 7 100) (0 #xb0 10 20) (0 #xc1 80) (0 #xb1 10 110)
      (0 #xcf 140) (0 #xb2 10 64)
      (0 #x90 60 100) (0 64 90) (0 #x91 48 110) (48 #x99 36 120) (0 38 100)
      (48 #x90 60 0) (0 #xe0 0 80) (24 #xe0 0 30) (0 #x99 30 100) (24 #x99 42 90)
      (0 #xf0 3 1 2 #xf7) (24 #x81 48 0) (0 #xa0 64 50) (0 #xd0 20)
      (48 #x9f 72 100) (0 #x92 55 127) (96 #xb0 #x7b 0) (0 #x9f 72 0)
      (48 #x90 67 80) (0 #x91 50 60) (96 #xb0 7 40) (96 #x80 67 0) (0 #x81 50 0)
      (48 #x99 81 100) (0 82 100) (96 #x89 81 0)
      (100 #x90 72 100) (200 #x80 72 0)))))

;; Format 0 with SMPTE division (-25 fps x 40, which midifile.c reads as
;; 24 x 40 ticks per beat).
(define smpte-midi
  (midi-file 0 (- #x10000 (* 25 256) (- 40))
   (list
    `((0 #xc0 0) (0 #x90 60 100) (0 #x90 64 100) (400 #x80 60 0) (0 64 0)
      (0 #xc0 19) (0 #x90 57 110) (800 #x80 57 0) (400 #x90 52 90) (300 #x80 52 0)))))

;; A MUS lump whose first key press has no velocity: mus2mid.c reuses the
;; channel's cached velocity, kept from the song converted before it. Also
;; a controller value over 127, a pitch wheel and a system event.
(define velocity-mus
  (let ([score '(#x40 0 5  #x40 3 200  #x90 60  70
                 #x80 60  10
                 #x90 #xc4 90  70
                 #x20 200  #xa0 44  30
                 #x80 68  5
                 #x10 62  #xb0 11  30
                 #x60)])
    (u8-list->bytevector
     (append '(#x4d #x55 #x53 #x1a) (list (length score) 0) '(16 0 1 0 0 0 0 0)
             '(0 0) score))))

;; Truncated MIDI: neither side can load it.
(define bad-midi (u8-list->bytevector '(#x4d #x54 #x68 #x64 0 0 0 6 0 0 0 1)))

(define (write-file name bv)
  (let ([path (format "~a/~a" dir name)])
    (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv)))
    path))

(define tempo-path (write-file "tempo.mid" tempo-midi))
(define smpte-path (write-file "smpte.mid" smpte-midi))
(define velocity-path (write-file "velocity.mus" velocity-mus))
(define bad-path (write-file "bad.mid" bad-midi))

;;; Running a case. A case is a song (a lump name, or a path with a '/'), a
;;; rate, a frame count, looping, a volume, a DMX option string, a driver
;;; version and a script of (frame command [arg [arg]]).

(define (song-bytes name)
  (if (memv #\/ (string->list name))
      (call-with-port (open-file-input-port name) get-bytevector-all)
      (w-cache-lump-name name)))

(define drivers '((doom1-1.666 . 0) (doom2-1.666 . 1) (doom-1.9 . 2)))

(define (run-c name song rate frames loop? volume dmxoption driver script)
  (let ([txt (format "~a/~a.txt" dir name)] [raw (format "~a/~a.raw" dir name)])
    (call-with-output-file txt
      (lambda (p) (for-each (lambda (c) (fprintf p "~{~a~^ ~}\n" c)) script))
      'replace)
    (unless (= 0 (system (format "env TMPDIR=~a DMXOPTION='~a' OPL_DRV_VER=~a ~a ~a ~a ~a ~a ~a ~a ~a ~a"
                                 dir dmxoption (cdr (assq driver drivers)) probe wad-path song
                                 rate frames (if loop? 1 0) volume raw txt)))
      (printf "FAIL ~a: probe failed\n" name)
      (exit 1))
    (call-with-port (open-file-input-port raw) get-bytevector-all)))

;; The Scheme side of music_probe.c's main loop.
(define (run-scheme song rate frames loop? volume dmxoption driver script)
  (let ([bv (make-bytevector (* 4 frames))] [chunk 4096])
    (define (start name looping)
      (let ([h (oplmusic-register-song (song-bytes name))])
        (oplmusic-play-song h looping)
        h))
    (parameterize ([oplmusic-dmxoption dmxoption] [oplmusic-driver-version driver])
      (unless (oplmusic-init rate) (printf "FAIL oplmusic-init\n") (exit 1)))
    (oplmusic-set-volume volume)
    (let loop ([handle (start song loop?)] [pos 0] [script script])
      (let ([at (if (null? script) frames (caar script))])
        (let render ([pos pos])
          (if (< pos at)
              (let ([k (min chunk (- at pos))])
                (oplmusic-generate! bv (* 4 pos) k)
                (render (+ pos k)))
              (if (null? script)
                  bv
                  (let ([c (car script)])
                    (case (cadr c)
                      [(pause) (oplmusic-pause-song)]
                      [(resume) (oplmusic-resume-song)]
                      [(stop) (oplmusic-stop-song)]
                      [(volume) (oplmusic-set-volume (caddr c))]
                      [(chunk) (set! chunk (caddr c))])
                    (if (eq? (cadr c) 'play)
                        (begin (oplmusic-stop-song)
                               (oplmusic-unregister-song handle)
                               (loop (start (caddr c) (= 1 (cadddr c))) pos (cdr script)))
                        (loop handle pos (cdr script)))))))))))

(define (first-diff a b)
  (let ([n (min (bytevector-length a) (bytevector-length b))])
    (let loop ([i 0])
      (cond [(>= i n) #f]
            [(= (bytevector-u8-ref a i) (bytevector-u8-ref b i)) (loop (+ i 1))]
            [else (quotient i 4)]))))

(define (frame bv f)
  (if (and f (<= (* 4 (+ f 1)) (bytevector-length bv)))
      (list (bytevector-s16-ref bv (* 4 f) (endianness little))
            (bytevector-s16-ref bv (+ 2 (* 4 f)) (endianness little)))
      'none))

(define (nonzero-frames bv)
  (do ([i 0 (+ i 4)]
       [n 0 (if (= 0 (bytevector-u32-ref bv i (endianness little))) n (+ n 1))])
      ((>= i (bytevector-length bv)) n)))

(define total-seconds 0)

(define (compare name song rate seconds loop? volume . opts)
  (let* ([dmxoption (if (pair? opts) (car opts) "")]
         [driver (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) 'doom-1.9)]
         [script (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts))) (caddr opts) '())]
         [frames (exact (round (* seconds rate)))]
         [expected (run-c name song rate frames loop? volume dmxoption driver script)]
         [actual (run-scheme song rate frames loop? volume dmxoption driver script)])
    (set! total-seconds (+ total-seconds seconds))
    (if (bytevector=? expected actual)
        (check (format "~a: ~a frames at ~a Hz (~as, ~a non-silent) match C"
                       name frames rate seconds (nonzero-frames expected))
               #t #t)
        (let ([f (first-diff expected actual)])
          (check (format "~a matches C" name)
                 (list 'frame f (frame expected f) 'length (bytevector-length expected))
                 (list 'frame f (frame actual f) 'length (bytevector-length actual)))))))

;;; The cases.

;; Plain playback, both rates.
(compare "e1m1" "D_E1M1" 44100 10 #t 100)
(compare "e1m2" "D_E1M2" 49716 10 #t 127)
(compare "e1m3" "D_E1M3" 44100 6 #t 64)
(compare "victor" "D_VICTOR" 49716 6 #t 100)
;; D_INTRO is 6.9 s: two loop wraps with looping on.
(compare "intro-loop" "D_INTRO" 44100 16 #t 100)
;; D_INTROA with looping off: the song ends and the notes decay.
(compare "introa-once" "D_INTROA" 49716 9 #f 100)
;; Music volume changes, including to 0 and back, and pause/resume.
(compare "inter-volume-pause" "D_INTER" 44100 14 #t 120 "" 'doom-1.9
         '((50000 volume 40) (90000 volume 0) (130000 volume 127)
           (200000 pause) (260000 resume) (300000 volume 70)
           (350000 pause) (352000 volume 100) (400000 resume)
           (500000 pause) (520000 resume)))
;; Level changes on the same chip: stop one song, start another, with
;; mixing buffers of other sizes, a stop with nothing playing after it, and
;; a pause carried into the next song (playing unpauses).
(compare "level-changes" "D_E1M4" 49716 15 #t 100 "" 'doom-1.9
         '((100000 play "D_E1M9" 1) (150000 chunk 1000) (230000 play "D_E1M5" 1)
           (260000 chunk 333) (300000 stop) (340000 chunk 4096) (360000 play "D_E1M6" 1)
           (500000 pause) (560000 play "D_E1M7" 1) (700000 play "D_E1M8" 0)))
;; OPL3 mode (18 voices, pan), and with -reverse.
(compare "e1m5-opl3" "D_E1M5" 44100 8 #t 100 "-opl3")
(compare "e1m6-opl3-reverse" "D_E1M6" 49716 8 #t 110 "-opl3 -reverse")
;; The older DMX voice allocation behaviours.
(compare "e1m7-doom1-1.666-opl3" "D_E1M7" 44100 6 #t 100 "-opl3" 'doom1-1.666)
(compare "e1m8-doom1-1.666" "D_E1M8" 49716 6 #t 100 "" 'doom1-1.666)
(compare "e1m9-doom2-1.666" "D_E1M9" 44100 6 #t 100 "" 'doom2-1.666)
;; MIDI files: tempo changes across loop restarts, and SMPTE division.
(compare "tempo-midi-opl3" tempo-path 44100 14 #t 100 "-opl3" 'doom-1.9
         '((300000 pause) (330000 resume)))
(compare "smpte-midi" smpte-path 49716 4 #f 90)
;; mus2mid's cached velocity, carried over from the song before.
(compare "velocity-carry" "D_E1M2" 44100 4 #t 100 "" 'doom-1.9
         '((60000 play "build/music/velocity.mus" 1)))
;; A file neither side can load: silence.
(compare "bad-midi" bad-path 44100 1 #t 100)

(printf "info ~as of audio compared\n" total-seconds)

;;; API behaviour.
(check "oplmusic-register-song rejects a bad file" #f
       (oplmusic-register-song bad-midi))
(check "a registered song plays until stopped" '(#t #f)
       (let ([h (oplmusic-register-song (w-cache-lump-name "D_E1M1"))])
         (oplmusic-play-song h #t)
         (let ([playing (oplmusic-playing?)])
           (oplmusic-stop-song)
           (list playing (oplmusic-playing?)))))
(define (rejects? thunk)
  (guard (e [(assertion-violation? e) #t]) (thunk) #f))
(check "bad arguments are rejected" '(#t #t #t)
       (list (rejects? (lambda () (oplmusic-generate! (make-bytevector 7) 0 2)))
             (rejects? (lambda () (oplmusic-set-volume 128)))
             (rejects? (lambda () (oplmusic-init 10)))))

;;; Timing (informational): 10 s of D_E1M1 at 44100 Hz in 4096-frame
;;; buffers, from the start of the song.
(let* ([rate 44100] [frames (* 10 rate)] [bv (make-bytevector (* 4 4096))])
  (oplmusic-init rate)
  (oplmusic-set-volume 100)
  (oplmusic-play-song (oplmusic-register-song (w-cache-lump-name "D_E1M1")) #t)
  (collect)
  (let ([t0 (cpu-time)] [b0 (sstats-bytes (statistics))])
    (let loop ([pos 0])
      (when (< pos frames)
        (let ([k (min 4096 (- frames pos))])
          (oplmusic-generate! bv 0 k)
          (loop (+ pos k)))))
    (printf "info D_E1M1, 10 s at 44100 Hz: ~a ms CPU, ~a bytes allocated\n"
            (- (cpu-time) t0) (- (sstats-bytes (statistics)) b0)))
  (oplmusic-stop-song))

(finish)
