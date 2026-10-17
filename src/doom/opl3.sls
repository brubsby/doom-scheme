;;; opl3: Nuked OPL3 1.8 (YMF262 emulator), from Chocolate Doom's
;;; opl/opl3.c and opl3.h. Bit-exact with the C; tests/opl.ss checks the
;;; output sample for sample against tools/oracle/probes/opl_probe.c.
;;;
;;; A chip is one fxvector, which avoids allocating per sample and keeps
;;; every field a fixnum. It holds the chip globals, 36 slot blocks, 18
;;; channel blocks, and the write buffer. C pointers become indices into that
;;; fxvector: a slot's `mod` and `trem` and a channel's `out[4]` hold the index
;;; of the cell they point at. G-ZERO is chip->zeromod, which is always 0.
;;; Every field holds the value its C type would hold. Where C truncates or
;;; wraps (Bit16u, Bit32u, int16 casts), the code masks explicitly.
;;;
;;; Output: opl3-generate-stream! and opl3-generate-resampled! write s16
;;; little-endian stereo frames (L then R, 4 bytes) into a bytevector, which is
;;; the format an audio device takes. An fxvector variant wouldn't be faster:
;;; one store per sample is noise next to the ~36 slot updates behind it.
;;;
;;; Speed: the fixnum and fxvector primitives below are compiled unsafe
;;; (#3%), which makes generation about 3x faster than safe code. Every
;;; exported procedure checks its arguments first, so the unchecked code only
;;; ever sees a well-formed chip, fixnum registers and in-range offsets.
#!chezscheme
(library (doom opl3)
  (export opl3-make opl3-reset! opl3-write-reg! opl3-write-reg-buffered!
          opl3-generate! opl3-generate-resampled! opl3-generate-stream!
          OPL3-NATIVE-RATE)
  (import (except (chezscheme)
            fx+ fx- fx* fx= fx< fx>
            fx<= fx>= fxmin fxmax fxquotient fxremainder
            fxsll fxsrl fxsra fxlogand fxlogor fxxor
            fxlognot fxfirst-bit-set fxvector-ref fxvector-set!))

  (define-syntax define-unsafe
    (syntax-rules ()
      [(_ (name prim) ...) (begin (define-syntax name (identifier-syntax prim)) ...)]))
  (define-unsafe
    (fx+ #3%fx+) (fx- #3%fx-) (fx* #3%fx*)
    (fx= #3%fx=) (fx< #3%fx<) (fx> #3%fx>)
    (fx<= #3%fx<=) (fx>= #3%fx>=) (fxmin #3%fxmin)
    (fxmax #3%fxmax) (fxquotient #3%fxquotient) (fxremainder #3%fxremainder)
    (fxsll #3%fxsll) (fxsrl #3%fxsrl) (fxsra #3%fxsra)
    (fxlogand #3%fxlogand) (fxlogor #3%fxlogor) (fxxor #3%fxxor)
    (fxlognot #3%fxlognot) (fxfirst-bit-set #3%fxfirst-bit-set) (fxvector-ref #3%fxvector-ref)
    (fxvector-set! #3%fxvector-set!))

  (define OPL3-NATIVE-RATE 49716)

  (define-syntax define-constants
    (syntax-rules ()
      [(_ (name val) ...) (begin (define-syntax name (identifier-syntax val)) ...)]))

  ;; Field access: (ref c i) is cell i, (ref c base field) is a block's field.
  (define-syntax ref
    (syntax-rules ()
      [(_ c i) (fxvector-ref c i)]
      [(_ c b f) (fxvector-ref c (fx+ b f))]))
  (define-syntax ref!
    (syntax-rules ()
      [(_ c i v) (fxvector-set! c i v)]
      [(_ c b f v) (fxvector-set! c (fx+ b f) v)]))

  ;; Chip globals.
  (define-constants
    (G-ZERO 0) (G-TREMOLO 1) (G-TIMER 2) (G-EG-TIMER 3) (G-EG-TIMERREM 4)
    (G-EG-STATE 5) (G-EG-ADD 6) (G-NEWM 7) (G-NTS 8) (G-RHY 9) (G-VIBPOS 10)
    (G-VIBSHIFT 11) (G-TREMOLOPOS 12) (G-TREMOLOSHIFT 13) (G-NOISE 14)
    (G-MIX0 15) (G-MIX1 16) (G-HH2 17) (G-HH3 18) (G-HH7 19) (G-HH8 20)
    (G-TC3 21) (G-TC5 22) (G-RATERATIO 23) (G-SAMPLECNT 24) (G-OLD0 25)
    (G-OLD1 26) (G-SAM0 27) (G-SAM1 28) (G-WB-SAMPLECNT 29) (G-WB-CUR 30)
    (G-WB-LAST 31) (G-WB-LASTTIME 32))

  ;; opl3_slot fields. S-CH is the slot's channel block index.
  (define-constants
    (S-OUT 0) (S-FBMOD 1) (S-MOD 2) (S-PROUT 3) (S-EG-ROUT 4) (S-EG-OUT 5)
    (S-EG-GEN 6) (S-EG-KSL 7) (S-TREM 8) (S-VIB 9) (S-TYPE 10) (S-KSR 11)
    (S-MULT 12) (S-KSL 13) (S-TL 14) (S-AR 15) (S-DR 16) (S-SL 17) (S-RR 18)
    (S-WF 19) (S-KEY 20) (S-PG-RESET 21) (S-PG-PHASE 22) (S-PHASE-OUT 23)
    (S-NUM 24) (S-CH 25)
    (SLOT-STRIDE 26))

  ;; opl3_channel fields. C-SLOT0/1 and C-PAIR are block indices (-1: no pair).
  (define-constants
    (C-SLOT0 0) (C-SLOT1 1) (C-PAIR 2) (C-OUT0 3) (C-OUT1 4) (C-OUT2 5)
    (C-OUT3 6) (C-TYPE 7) (C-FNUM 8) (C-BLOCK 9) (C-FB 10) (C-CON 11)
    (C-ALG 12) (C-KSV 13) (C-CHA 14) (C-CHB 15) (C-NUM 16)
    (CH-STRIDE 17))

  ;; Write buffer entries: time, reg, data.
  (define-constants
    (W-TIME 0) (W-REG 1) (W-DATA 2) (WB-STRIDE 3) (WB-SIZE 1024) (WB-DELAY 2))

  (define-constants
    (SLOT-BASE 40)
    (CH-BASE 976)                       ; SLOT-BASE + 36 * SLOT-STRIDE
    (WB-BASE 1282)                      ; CH-BASE + 18 * CH-STRIDE
    (CHIP-SIZE 4354))                   ; WB-BASE + WB-SIZE * WB-STRIDE

  (define-constants
    (ch-2op 0) (ch-4op 1) (ch-4op2 2) (ch-drum 3)
    (egk-norm 1) (egk-drum 2)
    (eg-attack 0) (eg-decay 1) (eg-sustain 2) (eg-release 3)
    (RSM-FRAC 10))

  (define-syntax slot-base
    (syntax-rules () [(_ n) (fx+ SLOT-BASE (fx* n SLOT-STRIDE))]))
  (define-syntax ch-base
    (syntax-rules () [(_ n) (fx+ CH-BASE (fx* n CH-STRIDE))]))

  ;; logsin table
  (define logsinrom
    (fxvector #x859 #x6c3 #x607 #x58b #x52e #x4e4 #x4a6 #x471
          #x443 #x41a #x3f5 #x3d3 #x3b5 #x398 #x37e #x365
          #x34e #x339 #x324 #x311 #x2ff #x2ed #x2dc #x2cd
          #x2bd #x2af #x2a0 #x293 #x286 #x279 #x26d #x261
          #x256 #x24b #x240 #x236 #x22c #x222 #x218 #x20f
          #x206 #x1fd #x1f5 #x1ec #x1e4 #x1dc #x1d4 #x1cd
          #x1c5 #x1be #x1b7 #x1b0 #x1a9 #x1a2 #x19b #x195
          #x18f #x188 #x182 #x17c #x177 #x171 #x16b #x166
          #x160 #x15b #x155 #x150 #x14b #x146 #x141 #x13c
          #x137 #x133 #x12e #x129 #x125 #x121 #x11c #x118
          #x114 #x10f #x10b #x107 #x103 #x0ff #x0fb #x0f8
          #x0f4 #x0f0 #x0ec #x0e9 #x0e5 #x0e2 #x0de #x0db
          #x0d7 #x0d4 #x0d1 #x0cd #x0ca #x0c7 #x0c4 #x0c1
          #x0be #x0bb #x0b8 #x0b5 #x0b2 #x0af #x0ac #x0a9
          #x0a7 #x0a4 #x0a1 #x09f #x09c #x099 #x097 #x094
          #x092 #x08f #x08d #x08a #x088 #x086 #x083 #x081
          #x07f #x07d #x07a #x078 #x076 #x074 #x072 #x070
          #x06e #x06c #x06a #x068 #x066 #x064 #x062 #x060
          #x05e #x05c #x05b #x059 #x057 #x055 #x053 #x052
          #x050 #x04e #x04d #x04b #x04a #x048 #x046 #x045
          #x043 #x042 #x040 #x03f #x03e #x03c #x03b #x039
          #x038 #x037 #x035 #x034 #x033 #x031 #x030 #x02f
          #x02e #x02d #x02b #x02a #x029 #x028 #x027 #x026
          #x025 #x024 #x023 #x022 #x021 #x020 #x01f #x01e
          #x01d #x01c #x01b #x01a #x019 #x018 #x017 #x017
          #x016 #x015 #x014 #x014 #x013 #x012 #x011 #x011
          #x010 #x00f #x00f #x00e #x00d #x00d #x00c #x00c
          #x00b #x00a #x00a #x009 #x009 #x008 #x008 #x007
          #x007 #x007 #x006 #x006 #x005 #x005 #x005 #x004
          #x004 #x004 #x003 #x003 #x003 #x002 #x002 #x002
          #x002 #x001 #x001 #x001 #x001 #x001 #x001 #x001
          #x000 #x000 #x000 #x000 #x000 #x000 #x000 #x000))

  ;; exp table
  (define exprom
    (fxvector #x7fa #x7f5 #x7ef #x7ea #x7e4 #x7df #x7da #x7d4
          #x7cf #x7c9 #x7c4 #x7bf #x7b9 #x7b4 #x7ae #x7a9
          #x7a4 #x79f #x799 #x794 #x78f #x78a #x784 #x77f
          #x77a #x775 #x770 #x76a #x765 #x760 #x75b #x756
          #x751 #x74c #x747 #x742 #x73d #x738 #x733 #x72e
          #x729 #x724 #x71f #x71a #x715 #x710 #x70b #x706
          #x702 #x6fd #x6f8 #x6f3 #x6ee #x6e9 #x6e5 #x6e0
          #x6db #x6d6 #x6d2 #x6cd #x6c8 #x6c4 #x6bf #x6ba
          #x6b5 #x6b1 #x6ac #x6a8 #x6a3 #x69e #x69a #x695
          #x691 #x68c #x688 #x683 #x67f #x67a #x676 #x671
          #x66d #x668 #x664 #x65f #x65b #x657 #x652 #x64e
          #x649 #x645 #x641 #x63c #x638 #x634 #x630 #x62b
          #x627 #x623 #x61e #x61a #x616 #x612 #x60e #x609
          #x605 #x601 #x5fd #x5f9 #x5f5 #x5f0 #x5ec #x5e8
          #x5e4 #x5e0 #x5dc #x5d8 #x5d4 #x5d0 #x5cc #x5c8
          #x5c4 #x5c0 #x5bc #x5b8 #x5b4 #x5b0 #x5ac #x5a8
          #x5a4 #x5a0 #x59c #x599 #x595 #x591 #x58d #x589
          #x585 #x581 #x57e #x57a #x576 #x572 #x56f #x56b
          #x567 #x563 #x560 #x55c #x558 #x554 #x551 #x54d
          #x549 #x546 #x542 #x53e #x53b #x537 #x534 #x530
          #x52c #x529 #x525 #x522 #x51e #x51b #x517 #x514
          #x510 #x50c #x509 #x506 #x502 #x4ff #x4fb #x4f8
          #x4f4 #x4f1 #x4ed #x4ea #x4e7 #x4e3 #x4e0 #x4dc
          #x4d9 #x4d6 #x4d2 #x4cf #x4cc #x4c8 #x4c5 #x4c2
          #x4be #x4bb #x4b8 #x4b5 #x4b1 #x4ae #x4ab #x4a8
          #x4a4 #x4a1 #x49e #x49b #x498 #x494 #x491 #x48e
          #x48b #x488 #x485 #x482 #x47e #x47b #x478 #x475
          #x472 #x46f #x46c #x469 #x466 #x463 #x460 #x45d
          #x45a #x457 #x454 #x451 #x44e #x44b #x448 #x445
          #x442 #x43f #x43c #x439 #x436 #x433 #x430 #x42d
          #x42a #x428 #x425 #x422 #x41f #x41c #x419 #x416
          #x414 #x411 #x40e #x40b #x408 #x406 #x403 #x400))

  ;; freq mult table multiplied by 2:
  ;; 1/2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 12, 12, 15, 15
  (define mt (fxvector 1 2 4 6 8 10 12 14 16 18 20 20 24 24 30 30))

  ;; ksl table
  (define kslrom (fxvector 0 32 40 45 48 51 53 55 56 58 59 60 61 62 63 64))
  (define kslshift (fxvector 8 1 2 0))

  ;; envelope generator constants: eg_incstep[4][4], row-major
  (define eg-incstep (fxvector 0 0 0 0  1 0 0 0  1 0 1 0  1 1 1 0))

  ;; address decoding
  (define ad-slot
    (fxvector 0 1 2 3 4 5 -1 -1 6 7 8 9 10 11 -1 -1
              12 13 14 15 16 17 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
  (define ch-slot (fxvector 0 1 2 6 7 8 12 13 14 18 19 20 24 25 26 30 31 32))

  ;; (Bit16s) cast.
  (define-syntax s16
    (syntax-rules ()
      [(_ x) (fx- (fxlogand (fx+ x #x8000) #xffff) #x8000)]))

  ;;
  ;; Envelope generator
  ;;

  ;; OPL3_EnvelopeCalcExp. level is never negative here.
  (define-syntax calc-exp
    (syntax-rules ()
      [(_ lv)
       (let ([level (fxmin lv #x1fff)])
         (fxsra (fxsll (fxvector-ref exprom (fxlogand level #xff)) 1)
                (fxsrl level 8)))]))

  ;; OPL3_EnvelopeCalcSin0..7 on phase (already & 0x3ff) and envelope.
  ;; `x ^ 0xffff` on a non-negative Bit16s, cast back to Bit16s, is fxlognot.
  (define-syntax logsin
    (syntax-rules () [(_ i) (fxvector-ref logsinrom i)]))
  (define (envelope-sin wf phase envelope)
    (let ([env (fxsll envelope 3)])
      (case wf
        [(0)
         (let ([v (calc-exp (fx+ (if (fx= 0 (fxlogand phase #x100))
                                     (logsin (fxlogand phase #xff))
                                     (logsin (fxxor (fxlogand phase #xff) #xff)))
                                 env))])
           (if (fx= 0 (fxlogand phase #x200)) v (fxlognot v)))]
        [(1)
         (calc-exp (fx+ (cond [(fx> (fxlogand phase #x200) 0) #x1000]
                              [(fx> (fxlogand phase #x100) 0)
                               (logsin (fxxor (fxlogand phase #xff) #xff))]
                              [else (logsin (fxlogand phase #xff))])
                        env))]
        [(2)
         (calc-exp (fx+ (if (fx= 0 (fxlogand phase #x100))
                            (logsin (fxlogand phase #xff))
                            (logsin (fxxor (fxlogand phase #xff) #xff)))
                        env))]
        [(3)
         (calc-exp (fx+ (if (fx= 0 (fxlogand phase #x100))
                            (logsin (fxlogand phase #xff))
                            #x1000)
                        env))]
        [(4)
         (let ([v (calc-exp
                   (fx+ (cond [(fx> (fxlogand phase #x200) 0) #x1000]
                              [(fx> (fxlogand phase #x80) 0)
                               (logsin (fxlogand (fxsll (fxxor phase #xff) 1) #xff))]
                              [else (logsin (fxlogand (fxsll phase 1) #xff))])
                        env))])
           (if (fx= (fxlogand phase #x300) #x100) (fxlognot v) v))]
        [(5)
         (calc-exp (fx+ (cond [(fx> (fxlogand phase #x200) 0) #x1000]
                              [(fx> (fxlogand phase #x80) 0)
                               (logsin (fxlogand (fxsll (fxxor phase #xff) 1) #xff))]
                              [else (logsin (fxlogand (fxsll phase 1) #xff))])
                        env))]
        [(6)
         (let ([v (calc-exp env)])
           (if (fx= 0 (fxlogand phase #x200)) v (fxlognot v)))]
        [else
         (if (fx= 0 (fxlogand phase #x200))
             (calc-exp (fx+ (fxsll phase 3) env))
             (fxlognot (calc-exp (fx+ (fxsll (fxxor (fxlogand phase #x1ff) #x1ff) 3)
                                      env))))])))

  ;; Output of a slot whose envelope is fully off (eg_out >= 0x1ff, so the exp
  ;; term is 0): the sign mask alone, 0 or -1, per waveform.
  (define-syntax silent-sin
    (syntax-rules ()
      [(_ wf phase)
       (case wf
         [(0 6 7) (if (fx= 0 (fxlogand phase #x200)) 0 -1)]
         [(4) (if (fx= (fxlogand phase #x300) #x100) -1 0)]
         [else 0])]))

  (define (envelope-update-ksl! c s)
    (let* ([ch (ref c s S-CH)]
           [ksl (fx- (fxsll (fxvector-ref kslrom (fxsrl (ref c ch C-FNUM) 6)) 2)
                     (fxsll (fx- 8 (ref c ch C-BLOCK)) 5))])
      (ref! c s S-EG-KSL (if (fx< ksl 0) 0 ksl))))

  ;; OPL3_EnvelopeCalc.
  (define (envelope-calc! c s ch)
    (let* ([rout (ref c s S-EG-ROUT)]
           [gen (ref c s S-EG-GEN)]
           [key (ref c s S-KEY)]
           [reset (and (fx> key 0) (fx= gen eg-release))]
           [reg-rate (cond
                       [reset (ref c s S-AR)]
                       [(fx= gen eg-attack) (ref c s S-AR)]
                       [(fx= gen eg-decay) (ref c s S-DR)]
                       [(fx= gen eg-sustain) (if (fx= (ref c s S-TYPE) 0) (ref c s S-RR) 0)]
                       [else (ref c s S-RR)])]
           [ks (fxsrl (ref c ch C-KSV) (fxsll (fxxor (ref c s S-KSR) 1) 1))]
           [rate (fx+ ks (fxsll reg-rate 2))]
           [rate-hi (let ([h (fxsrl rate 2)]) (if (fx= 0 (fxlogand h #x10)) h #x0f))]
           [rate-lo (fxlogand rate 3)]
           [eg-state (ref c G-EG-STATE)]
           [shift
            (cond
              [(fx= reg-rate 0) 0]
              [(fx< rate-hi 12)
               (if (fx= eg-state 0)
                   0
                   (let ([eg-shift (fx+ rate-hi (ref c G-EG-ADD))])
                     (cond [(fx= eg-shift 12) 1]
                           [(fx= eg-shift 13) (fxlogand (fxsrl rate-lo 1) 1)]
                           [(fx= eg-shift 14) (fxlogand rate-lo 1)]
                           [else 0])))]
              [else
               (let* ([sh (fx+ (fxlogand rate-hi 3)
                               (fxvector-ref eg-incstep
                                             (fx+ (fxsll rate-lo 2)
                                                  (fxlogand (ref c G-TIMER) 3))))]
                      [sh (if (fx= 0 (fxlogand sh 4)) sh 3)])
                 (if (fx= sh 0) eg-state sh))])]
           [eg-off (fx= (fxlogand rout #x1f8) #x1f8)]
           [base-rout (cond
                        [(and reset (fx= rate-hi #x0f)) 0]
                        [(and (not (fx= gen eg-attack)) (not reset) eg-off) #x1ff]
                        [else rout])])
      (ref! c s S-EG-OUT (fx+ rout
                            (fxsll (ref c s S-TL) 2)
                            (fxsrl (ref c s S-EG-KSL) (fxvector-ref kslshift (ref c s S-KSL)))
                            (ref c (ref c s S-TREM))))
      (ref! c s S-PG-RESET (if reset 1 0))
      ;; (gen2: the generator after this step; a set! of gen here would box
      ;; it, allocating on every call)
      (let ([inc
             (cond
               [(fx= gen eg-attack)
                (cond [(fx= rout 0) 0]
                      [(and (fx> key 0) (fx> shift 0) (not (fx= rate-hi #x0f)))
                       (fxsra (fxsll (fxlognot rout) shift) 4)]
                      [else 0])]
               [(fx= gen eg-decay)
                (cond [(fx= (fxsrl rout 4) (ref c s S-SL)) 0]
                      [(and (not eg-off) (not reset) (fx> shift 0))
                       (fxsll 1 (fx- shift 1))]
                      [else 0])]
               [(and (not eg-off) (not reset) (fx> shift 0)) (fxsll 1 (fx- shift 1))]
               [else 0])]
            [gen2 (cond
                    [(and (fx= gen eg-attack) (fx= rout 0)) eg-decay]
                    [(and (fx= gen eg-decay) (fx= (fxsrl rout 4) (ref c s S-SL))) eg-sustain]
                    [else gen])])
        (ref! c s S-EG-ROUT (fxlogand (fx+ base-rout inc) #x1ff))
        (ref! c s S-EG-GEN (cond [(fx= key 0) eg-release] [reset eg-attack] [else gen2])))))

  ;;
  ;; Phase generator
  ;;

  ;; OPL3_PhaseGenerate, including the rhythm-mode phase overrides and the
  ;; noise LFSR step that runs once per slot.
  (define (phase-generate! c s ch num)
    (let* ([fnum (ref c ch C-FNUM)]
           [fnum (if (fx= (ref c s S-VIB) 0)
                     fnum
                     (let* ([vibpos (ref c G-VIBPOS)]
                            [range (fxlogand (fxsrl fnum 7) 7)]
                            [range (cond [(fx= 0 (fxlogand vibpos 3)) 0]
                                         [(fx= 1 (fxlogand vibpos 1)) (fxsrl range 1)]
                                         [else range])]
                            [range (fxsra range (ref c G-VIBSHIFT))]
                            [range (if (fx= 0 (fxlogand vibpos 4)) range (fx- range))])
                       (fxlogand (fx+ fnum range) #xffff)))]
           [basefreq (fxsrl (fxsll fnum (ref c ch C-BLOCK)) 1)]
           [pg (ref c s S-PG-PHASE)]
           [phase (fxlogand (fxsrl pg 9) #xffff)]
           [pg (if (fx= (ref c s S-PG-RESET) 0) pg 0)]
           [noise (ref c G-NOISE)])
      (ref! c s S-PG-PHASE
          (fxlogand (fx+ pg (fxsrl (fx* basefreq (fxvector-ref mt (ref c s S-MULT))) 1))
                    #xffffffff))
      (ref! c s S-PHASE-OUT phase)
      (when (and (fx>= num 13) (fx<= num 17))
        (let ([rhy (fx> (fxlogand (ref c G-RHY) #x20) 0)])
          (when (fx= num 13)                       ; hh
            (ref! c G-HH2 (fxlogand (fxsrl phase 2) 1))
            (ref! c G-HH3 (fxlogand (fxsrl phase 3) 1))
            (ref! c G-HH7 (fxlogand (fxsrl phase 7) 1))
            (ref! c G-HH8 (fxlogand (fxsrl phase 8) 1)))
          (when (and rhy (fx= num 17))             ; tc
            (ref! c G-TC3 (fxlogand (fxsrl phase 3) 1))
            (ref! c G-TC5 (fxlogand (fxsrl phase 5) 1)))
          (when rhy
            (let ([rm-xor (fxlogor (fxxor (ref c G-HH2) (ref c G-HH7))
                                   (fxxor (ref c G-HH3) (ref c G-TC5))
                                   (fxxor (ref c G-TC3) (ref c G-TC5)))])
              (cond
                [(fx= num 13)
                 (ref! c s S-PHASE-OUT
                     (fxlogor (fxsll rm-xor 9)
                              (if (fx= 0 (fxxor rm-xor (fxlogand noise 1))) #x34 #xd0)))]
                [(fx= num 16)
                 (let ([hh8 (ref c G-HH8)])
                   (ref! c s S-PHASE-OUT
                       (fxlogor (fxsll hh8 9)
                                (fxsll (fxxor hh8 (fxlogand noise 1)) 8))))]
                [(fx= num 17)
                 (ref! c s S-PHASE-OUT (fxlogor (fxsll rm-xor 9) #x80))])))))
      (ref! c G-NOISE (fxlogor (fxsrl noise 1)
                             (fxsll (fxlogand (fxxor (fxsrl noise 14) noise) 1) 22)))))

  ;;
  ;; Slot
  ;;

  ;; One slot's update for one sample: OPL3_SlotCalcFB, OPL3_EnvelopeCalc,
  ;; OPL3_PhaseGenerate, OPL3_SlotGenerate.
  ;; A slot that is keyed off, in release, with eg_rout 0x1ff is dormant:
  ;; EnvelopeCalc leaves eg_rout, eg_gen and pg_reset (already 0) unchanged,
  ;; and since eg_out >= 0x1ff the exp term is 0 and only the waveform's sign
  ;; bit reaches `out`. Its eg_out isn't written, which is safe because nothing
  ;; reads eg_out before the next EnvelopeCalc recomputes it.
  (define (slot-tick! c s num)
    (let ([ch (ref c s S-CH)]
          [out (ref c s S-OUT)])
      (let ([fb (ref c ch C-FB)])
        (ref! c s S-FBMOD (if (fx= fb 0) 0 (fxsra (fx+ (ref c s S-PROUT) out) (fx- 9 fb)))))
      (ref! c s S-PROUT out)
      (let ([dormant (and (fx= (ref c s S-KEY) 0)
                          (fx= (ref c s S-EG-ROUT) #x1ff)
                          (fx= (ref c s S-EG-GEN) eg-release))])
        (unless dormant (envelope-calc! c s ch))
        (phase-generate! c s ch num)
        (let ([phase (fxlogand (fx+ (ref c s S-PHASE-OUT) (ref c (ref c s S-MOD))) #x3ff)]
              [wf (ref c s S-WF)])
          (ref! c s S-OUT (if dormant
                            (silent-sin wf phase)
                            (envelope-sin wf phase (ref c s S-EG-OUT))))))))

  (define (slot-write-20! c s data)
    (ref! c s S-TREM (if (fx> (fxlogand data #x80) 0) G-TREMOLO G-ZERO))
    (ref! c s S-VIB (fxlogand (fxsrl data 6) 1))
    (ref! c s S-TYPE (fxlogand (fxsrl data 5) 1))
    (ref! c s S-KSR (fxlogand (fxsrl data 4) 1))
    (ref! c s S-MULT (fxlogand data #x0f)))

  (define (slot-write-40! c s data)
    (ref! c s S-KSL (fxlogand (fxsrl data 6) 3))
    (ref! c s S-TL (fxlogand data #x3f))
    (envelope-update-ksl! c s))

  (define (slot-write-60! c s data)
    (ref! c s S-AR (fxlogand (fxsrl data 4) #x0f))
    (ref! c s S-DR (fxlogand data #x0f)))

  (define (slot-write-80! c s data)
    (let ([sl (fxlogand (fxsrl data 4) #x0f)])
      (ref! c s S-SL (if (fx= sl #x0f) #x1f sl)))
    (ref! c s S-RR (fxlogand data #x0f)))

  (define (slot-write-e0! c s data)
    (ref! c s S-WF (if (fx= (ref c G-NEWM) 0) (fxlogand data 3) (fxlogand data 7))))

  ;;
  ;; Channel
  ;;

  (define-syntax key-on!
    (syntax-rules ()
      [(_ c s type) (ref! c s S-KEY (fxlogor (ref c s S-KEY) type))]))
  (define-syntax key-off!
    (syntax-rules ()
      [(_ c s type) (ref! c s S-KEY (fxlogand (ref c s S-KEY) (fxlognot type)))]))

  (define (set-outs! c ch a b d e)
    (ref! c ch C-OUT0 a) (ref! c ch C-OUT1 b) (ref! c ch C-OUT2 d) (ref! c ch C-OUT3 e))

  (define (channel-update-rhythm! c data)
    (ref! c G-RHY (fxlogand data #x3f))
    (let ([ch6 (ch-base 6)] [ch7 (ch-base 7)] [ch8 (ch-base 8)])
      (cond
        [(fx> (fxlogand data #x20) 0)
         (let ([s60 (ref c ch6 C-SLOT0)] [s61 (ref c ch6 C-SLOT1)]
               [s70 (ref c ch7 C-SLOT0)] [s71 (ref c ch7 C-SLOT1)]
               [s80 (ref c ch8 C-SLOT0)] [s81 (ref c ch8 C-SLOT1)])
           (set-outs! c ch6 (fx+ s61 S-OUT) (fx+ s61 S-OUT) G-ZERO G-ZERO)
           (set-outs! c ch7 (fx+ s70 S-OUT) (fx+ s70 S-OUT) (fx+ s71 S-OUT) (fx+ s71 S-OUT))
           (set-outs! c ch8 (fx+ s80 S-OUT) (fx+ s80 S-OUT) (fx+ s81 S-OUT) (fx+ s81 S-OUT))
           (ref! c ch6 C-TYPE ch-drum) (ref! c ch7 C-TYPE ch-drum) (ref! c ch8 C-TYPE ch-drum)
           (channel-setup-alg! c ch6)
           (channel-setup-alg! c ch7)
           (channel-setup-alg! c ch8)
           (let-syntax ([drum (syntax-rules ()
                                [(_ bit s ...)
                                 (if (fx> (fxlogand data bit) 0)
                                     (begin (key-on! c s egk-drum) ...)
                                     (begin (key-off! c s egk-drum) ...))])])
             (drum #x01 s70)            ; hh
             (drum #x02 s81)            ; tc
             (drum #x04 s80)            ; tom
             (drum #x08 s71)            ; sd
             (drum #x10 s60 s61)))]     ; bd
        [else
         (do ([n 6 (fx+ n 1)]) ((fx= n 9))
           (let ([ch (ch-base n)])
             (ref! c ch C-TYPE ch-2op)
             (channel-setup-alg! c ch)
             (key-off! c (ref c ch C-SLOT0) egk-drum)
             (key-off! c (ref c ch C-SLOT1) egk-drum)))])))

  (define (update-ksv! c ch)
    (ref! c ch C-KSV (fxlogor (fxsll (ref c ch C-BLOCK) 1)
                            (fxlogand (fxsrl (ref c ch C-FNUM) (fx- 9 (ref c G-NTS))) 1))))

  (define (channel-write-a0! c ch data)
    (unless (and (fx> (ref c G-NEWM) 0) (fx= (ref c ch C-TYPE) ch-4op2))
      (ref! c ch C-FNUM (fxlogor (fxlogand (ref c ch C-FNUM) #x300) data))
      (update-ksv! c ch)
      (envelope-update-ksl! c (ref c ch C-SLOT0))
      (envelope-update-ksl! c (ref c ch C-SLOT1))
      (when (and (fx> (ref c G-NEWM) 0) (fx= (ref c ch C-TYPE) ch-4op))
        (let ([p (ref c ch C-PAIR)])
          (ref! c p C-FNUM (ref c ch C-FNUM))
          (ref! c p C-KSV (ref c ch C-KSV))
          (envelope-update-ksl! c (ref c p C-SLOT0))
          (envelope-update-ksl! c (ref c p C-SLOT1))))))

  (define (channel-write-b0! c ch data)
    (unless (and (fx> (ref c G-NEWM) 0) (fx= (ref c ch C-TYPE) ch-4op2))
      (ref! c ch C-FNUM (fxlogor (fxlogand (ref c ch C-FNUM) #xff)
                               (fxsll (fxlogand data 3) 8)))
      (ref! c ch C-BLOCK (fxlogand (fxsrl data 2) 7))
      (update-ksv! c ch)
      (envelope-update-ksl! c (ref c ch C-SLOT0))
      (envelope-update-ksl! c (ref c ch C-SLOT1))
      (when (and (fx> (ref c G-NEWM) 0) (fx= (ref c ch C-TYPE) ch-4op))
        (let ([p (ref c ch C-PAIR)])
          (ref! c p C-FNUM (ref c ch C-FNUM))
          (ref! c p C-BLOCK (ref c ch C-BLOCK))
          (ref! c p C-KSV (ref c ch C-KSV))
          (envelope-update-ksl! c (ref c p C-SLOT0))
          (envelope-update-ksl! c (ref c p C-SLOT1))))))

  ;; OPL3_ChannelSetupAlg.
  (define (channel-setup-alg! c ch)
    (let* ([alg (ref c ch C-ALG)]
           [s0 (ref c ch C-SLOT0)] [s1 (ref c ch C-SLOT1)]
           [s0-out (fx+ s0 S-OUT)] [s1-out (fx+ s1 S-OUT)]
           [s0-fb (fx+ s0 S-FBMOD)])
      (cond
        [(fx= (ref c ch C-TYPE) ch-drum)
         (let ([n (ref c ch C-NUM)])
           (cond
             [(or (fx= n 7) (fx= n 8))
              (ref! c s0 S-MOD G-ZERO)
              (ref! c s1 S-MOD G-ZERO)]
             [(fx= (fxlogand alg 1) 0)
              (ref! c s0 S-MOD s0-fb)
              (ref! c s1 S-MOD s0-out)]
             [else
              (ref! c s0 S-MOD s0-fb)
              (ref! c s1 S-MOD G-ZERO)]))]
        [(fx> (fxlogand alg #x08) 0) (void)]
        [(fx> (fxlogand alg #x04) 0)
         (let* ([p (ref c ch C-PAIR)]
                [p0 (ref c p C-SLOT0)] [p1 (ref c p C-SLOT1)]
                [p0-out (fx+ p0 S-OUT)] [p1-out (fx+ p1 S-OUT)])
           (set-outs! c p G-ZERO G-ZERO G-ZERO G-ZERO)
           (ref! c p0 S-MOD (fx+ p0 S-FBMOD))
           (case (fxlogand alg 3)
             [(0)
              (ref! c p1 S-MOD p0-out)
              (ref! c s0 S-MOD p1-out)
              (ref! c s1 S-MOD s0-out)
              (set-outs! c ch s1-out G-ZERO G-ZERO G-ZERO)]
             [(1)
              (ref! c p1 S-MOD p0-out)
              (ref! c s0 S-MOD G-ZERO)
              (ref! c s1 S-MOD s0-out)
              (set-outs! c ch p1-out s1-out G-ZERO G-ZERO)]
             [(2)
              (ref! c p1 S-MOD G-ZERO)
              (ref! c s0 S-MOD p1-out)
              (ref! c s1 S-MOD s0-out)
              (set-outs! c ch p0-out s1-out G-ZERO G-ZERO)]
             [else
              (ref! c p1 S-MOD G-ZERO)
              (ref! c s0 S-MOD p1-out)
              (ref! c s1 S-MOD G-ZERO)
              (set-outs! c ch p0-out s0-out s1-out G-ZERO)]))]
        [(fx= (fxlogand alg 1) 0)
         (ref! c s0 S-MOD s0-fb)
         (ref! c s1 S-MOD s0-out)
         (set-outs! c ch s1-out G-ZERO G-ZERO G-ZERO)]
        [else
         (ref! c s0 S-MOD s0-fb)
         (ref! c s1 S-MOD G-ZERO)
         (set-outs! c ch s0-out s1-out G-ZERO G-ZERO)])))

  (define (channel-write-c0! c ch data)
    (let ([con (fxlogand data 1)] [newm (fx> (ref c G-NEWM) 0)] [type (ref c ch C-TYPE)])
      (ref! c ch C-FB (fxsrl (fxlogand data #x0e) 1))
      (ref! c ch C-CON con)
      (ref! c ch C-ALG con)
      (cond
        [(and newm (fx= type ch-4op))
         (let ([p (ref c ch C-PAIR)])
           (ref! c p C-ALG (fxlogor 4 (fxsll con 1) (ref c p C-CON)))
           (ref! c ch C-ALG 8)
           (channel-setup-alg! c p))]
        [(and newm (fx= type ch-4op2))
         (let ([p (ref c ch C-PAIR)])
           (ref! c ch C-ALG (fxlogor 4 (fxsll (ref c p C-CON) 1) con))
           (ref! c p C-ALG 8)
           (channel-setup-alg! c ch))]
        [else (channel-setup-alg! c ch)])
      (if newm
          (begin
            (ref! c ch C-CHA (if (fx> (fxlogand data #x10) 0) #xffff 0))
            (ref! c ch C-CHB (if (fx> (fxlogand data #x20) 0) #xffff 0)))
          (begin
            (ref! c ch C-CHA #xffff)
            (ref! c ch C-CHB #xffff)))))

  (define (channel-key! c ch on?)
    (let ([type (ref c ch C-TYPE)])
      (let-syntax ([key (syntax-rules ()
                          [(_ s) (if on? (key-on! c s egk-norm) (key-off! c s egk-norm))])])
        (cond
          [(and (fx> (ref c G-NEWM) 0) (fx= type ch-4op))
           (let ([p (ref c ch C-PAIR)])
             (key (ref c ch C-SLOT0)) (key (ref c ch C-SLOT1))
             (key (ref c p C-SLOT0)) (key (ref c p C-SLOT1)))]
          [(or (fx= (ref c G-NEWM) 0) (fx= type ch-2op) (fx= type ch-drum))
           (key (ref c ch C-SLOT0)) (key (ref c ch C-SLOT1))]))))

  (define (channel-set-4op! c data)
    (do ([bit 0 (fx+ bit 1)]) ((fx= bit 6))
      (let ([n (if (fx>= bit 3) (fx+ bit 6) bit)])
        (if (fx> (fxlogand (fxsrl data bit) 1) 0)
            (begin (ref! c (ch-base n) C-TYPE ch-4op)
                   (ref! c (ch-base (fx+ n 3)) C-TYPE ch-4op2))
            (begin (ref! c (ch-base n) C-TYPE ch-2op)
                   (ref! c (ch-base (fx+ n 3)) C-TYPE ch-2op))))))

  ;;
  ;; Generation
  ;;

  (define-syntax clip-sample
    (syntax-rules () [(_ x) (fxmax -32768 (fxmin 32767 x))]))

  (define (tick-slots! c from to)
    (let loop ([n from] [s (slot-base from)])
      (unless (fx= n to)
        (slot-tick! c s n)
        (loop (fx+ n 1) (fx+ s SLOT-STRIDE)))))

  ;; Sum of all 18 channels' outputs, gated by C-CHA or C-CHB. Each channel's
  ;; four outputs sum within int16 (|out| <= 4085), so accm never wraps.
  (define (mix c side)
    (let loop ([ch CH-BASE] [n 0] [acc 0])
      (if (fx= n 18)
          acc
          (loop (fx+ ch CH-STRIDE) (fx+ n 1)
                (if (fx= (ref c ch side) 0)
                    acc
                    (fx+ acc (ref c (ref c ch C-OUT0)) (ref c (ref c ch C-OUT1))
                         (ref c (ref c ch C-OUT2)) (ref c (ref c ch C-OUT3))))))))

  ;; OPL3_Generate into the chip's samples[] (G-SAM0/1).
  (define (generate-native! c)
    (ref! c G-SAM1 (clip-sample (ref c G-MIX1)))
    (tick-slots! c 0 15)
    (ref! c G-MIX0 (mix c C-CHA))
    (tick-slots! c 15 18)
    (ref! c G-SAM0 (clip-sample (ref c G-MIX0)))
    (tick-slots! c 18 33)
    (ref! c G-MIX1 (mix c C-CHB))
    (tick-slots! c 33 36)
    (let ([timer (ref c G-TIMER)])
      (when (fx= (fxlogand timer #x3f) #x3f)
        (ref! c G-TREMOLOPOS (fxremainder (fx+ (ref c G-TREMOLOPOS) 1) 210)))
      (let ([pos (ref c G-TREMOLOPOS)])
        (ref! c G-TREMOLO (fxsrl (if (fx< pos 105) pos (fx- 210 pos)) (ref c G-TREMOLOSHIFT))))
      (when (fx= (fxlogand timer #x3ff) #x3ff)
        (ref! c G-VIBPOS (fxlogand (fx+ (ref c G-VIBPOS) 1) 7)))
      (ref! c G-TIMER (fxlogand (fx+ timer 1) #xffff)))
    ;; eg_timer < 2^36, so its lowest set bit is below 36.
    (let ([egt (ref c G-EG-TIMER)])
      (ref! c G-EG-ADD (if (fx= egt 0)
                         0
                         (let ([shift (fxfirst-bit-set egt)])
                           (if (fx> shift 12) 0 (fx+ shift 1)))))
      (when (or (fx> (ref c G-EG-TIMERREM) 0) (fx> (ref c G-EG-STATE) 0))
        (if (fx= egt #xfffffffff)
            (begin (ref! c G-EG-TIMER 0) (ref! c G-EG-TIMERREM 1))
            (begin (ref! c G-EG-TIMER (fx+ egt 1)) (ref! c G-EG-TIMERREM 0)))))
    (ref! c G-EG-STATE (fxxor (ref c G-EG-STATE) 1))
    (let loop ()
      (let ([e (fx+ WB-BASE (fx* (ref c G-WB-CUR) WB-STRIDE))])
        (when (and (fx<= (ref c e W-TIME) (ref c G-WB-SAMPLECNT))
                   (fx> (fxlogand (ref c e W-REG) #x200) 0))
          (ref! c e W-REG (fxlogand (ref c e W-REG) #x1ff))
          (write-reg! c (ref c e W-REG) (ref c e W-DATA))
          (ref! c G-WB-CUR (fxremainder (fx+ (ref c G-WB-CUR) 1) WB-SIZE))
          (loop))))
    (ref! c G-WB-SAMPLECNT (fx+ (ref c G-WB-SAMPLECNT) 1)))

  ;; OPL3_Generate into bv at off.
  (define (generate! c bv off)
    (generate-native! c)
    (bytevector-s16-set! bv off (ref c G-SAM0) (endianness little))
    (bytevector-s16-set! bv (fx+ off 2) (ref c G-SAM1) (endianness little)))

  ;; OPL3_GenerateResampled into bv at off.
  (define (resampled! c bv off)
    (let ([ratio (ref c G-RATERATIO)])
      (let loop ()
        (when (fx>= (ref c G-SAMPLECNT) ratio)
          (ref! c G-OLD0 (ref c G-SAM0))
          (ref! c G-OLD1 (ref c G-SAM1))
          (generate-native! c)
          (ref! c G-SAMPLECNT (fx- (ref c G-SAMPLECNT) ratio))
          (loop)))
      (let* ([cnt (ref c G-SAMPLECNT)] [rest (fx- ratio cnt)])
        (bytevector-s16-set! bv off
          (s16 (fxquotient (fx+ (fx* (ref c G-OLD0) rest) (fx* (ref c G-SAM0) cnt)) ratio))
          (endianness little))
        (bytevector-s16-set! bv (fx+ off 2)
          (s16 (fxquotient (fx+ (fx* (ref c G-OLD1) rest) (fx* (ref c G-SAM1) cnt)) ratio))
          (endianness little))
        (ref! c G-SAMPLECNT (fx+ cnt (fxsll 1 RSM-FRAC))))))

  ;; OPL3_GenerateStream into bv from off.
  (define (stream! c bv off n)
    (do ([i 0 (fx+ i 1)] [off off (fx+ off 4)]) ((fx= i n))
      (resampled! c bv off)))

  ;;
  ;; Reset and register writes
  ;;

  ;; OPL3_Reset.
  (define (reset! c samplerate)
    (fxvector-fill! c 0)
    (do ([n 0 (fx+ n 1)]) ((fx= n 36))
      (let ([s (slot-base n)])
        (ref! c s S-MOD G-ZERO)
        (ref! c s S-EG-ROUT #x1ff)
        (ref! c s S-EG-OUT #x1ff)
        (ref! c s S-EG-GEN eg-release)
        (ref! c s S-TREM G-ZERO)
        (ref! c s S-NUM n)))
    (do ([n 0 (fx+ n 1)]) ((fx= n 18))
      (let* ([ch (ch-base n)]
             [sn (fxvector-ref ch-slot n)]
             [s0 (slot-base sn)]
             [s1 (slot-base (fx+ sn 3))]
             [m (fxremainder n 9)])
        (ref! c ch C-SLOT0 s0)
        (ref! c ch C-SLOT1 s1)
        (ref! c s0 S-CH ch)
        (ref! c s1 S-CH ch)
        (ref! c ch C-PAIR (cond [(fx< m 3) (ch-base (fx+ n 3))]
                              [(fx< m 6) (ch-base (fx- n 3))]
                              [else -1]))
        (set-outs! c ch G-ZERO G-ZERO G-ZERO G-ZERO)
        (ref! c ch C-TYPE ch-2op)
        (ref! c ch C-CHA #xffff)
        (ref! c ch C-CHB #xffff)
        (ref! c ch C-NUM n)
        (channel-setup-alg! c ch)))
    (ref! c G-NOISE 1)
    (ref! c G-RATERATIO (quotient (logand (ash samplerate RSM-FRAC) #xffffffff) 49716))
    (ref! c G-TREMOLOSHIFT 4)
    (ref! c G-VIBSHIFT 1))

    (define-syntax with-slot
    (syntax-rules ()
      [(_ c high regm (s) body ...)
       (let ([a (fxvector-ref ad-slot (fxlogand regm #x1f))])
         (when (fx>= a 0)
           (let ([s (slot-base (fx+ (fx* 18 high) a))]) body ...)))]))

  ;; OPL3_WriteReg. reg and v are truncated to Bit16u and Bit8u as in C.
  (define (write-reg! c reg v)
    (let* ([reg (fxlogand reg #xffff)]
           [v (fxlogand v #xff)]
           [high (fxlogand (fxsrl reg 8) 1)]
           [regm (fxlogand reg #xff)]
           [lo (fxlogand regm #x0f)])
      (case (fxlogand regm #xf0)
        [(#x00)
         (if (fx= high 1)
             (case lo
               [(4) (channel-set-4op! c v)]
               [(5) (ref! c G-NEWM (fxlogand v 1))])
             (when (fx= lo 8) (ref! c G-NTS (fxlogand (fxsrl v 6) 1))))]
        [(#x20 #x30) (with-slot c high regm (s) (slot-write-20! c s v))]
        [(#x40 #x50) (with-slot c high regm (s) (slot-write-40! c s v))]
        [(#x60 #x70) (with-slot c high regm (s) (slot-write-60! c s v))]
        [(#x80 #x90) (with-slot c high regm (s) (slot-write-80! c s v))]
        [(#xe0 #xf0) (with-slot c high regm (s) (slot-write-e0! c s v))]
        [(#xa0)
         (when (fx< lo 9) (channel-write-a0! c (ch-base (fx+ (fx* 9 high) lo)) v))]
        [(#xb0)
         (cond
           [(and (fx= regm #xbd) (fx= high 0))
            (ref! c G-TREMOLOSHIFT (fx+ (fxsll (fxxor (fxsrl v 7) 1) 1) 2))
            (ref! c G-VIBSHIFT (fxxor (fxlogand (fxsrl v 6) 1) 1))
            (channel-update-rhythm! c v)]
           [(fx< lo 9)
            (let ([ch (ch-base (fx+ (fx* 9 high) lo))])
              (channel-write-b0! c ch v)
              (channel-key! c ch (fx> (fxlogand v #x20) 0)))])]
        [(#xc0)
         (when (fx< lo 9) (channel-write-c0! c (ch-base (fx+ (fx* 9 high) lo)) v))])))

  ;; OPL3_WriteRegBuffered.
  (define (write-reg-buffered! c reg v)
    (let* ([reg (fxlogand reg #xffff)]
           [v (fxlogand v #xff)]
           [last (ref c G-WB-LAST)]
           [e (fx+ WB-BASE (fx* last WB-STRIDE))])
      (when (fx> (fxlogand (ref c e W-REG) #x200) 0)
        (write-reg! c (fxlogand (ref c e W-REG) #x1ff) (ref c e W-DATA))
        (ref! c G-WB-CUR (fxremainder (fx+ last 1) WB-SIZE))
        (ref! c G-WB-SAMPLECNT (ref c e W-TIME)))
      (ref! c e W-REG (fxlogand (fxlogor reg #x200) #xffff))
      (ref! c e W-DATA v)
      (let ([time (fxmax (fx+ (ref c G-WB-LASTTIME) WB-DELAY) (ref c G-WB-SAMPLECNT))])
        (ref! c e W-TIME time)
        (ref! c G-WB-LASTTIME time))
      (ref! c G-WB-LAST (fxremainder (fx+ last 1) WB-SIZE))))

  ;;
  ;; Entry points: argument checks, then the unchecked code above.
  ;;

  (define (check-chip who c)
    (unless (and (fxvector? c) (eqv? (fxvector-length c) CHIP-SIZE))
      (assertion-violation who "not an opl3 chip" c)))

  (define (check-reg who reg v)
    (unless (and (fixnum? reg) (fixnum? v))
      (assertion-violation who "register and value must be fixnums" reg v)))

  (define (check-out who bv off n)
    (unless (and (bytevector? bv) (fixnum? off) (fixnum? n) (>= off 0) (>= n 0)
                 (<= (+ off (* 4 n)) (bytevector-length bv)))
      (assertion-violation who "frames don't fit in the bytevector" off n)))

  ;; (opl3-reset! chip samplerate): OPL3_Reset. The chip resamples its native
  ;; 49716 Hz output to samplerate, which must be at least 49 so the 22.10
  ;; rate ratio is nonzero (C divides by zero below that).
  (define (opl3-reset! c samplerate)
    (check-chip 'opl3-reset! c)
    (unless (and (fixnum? samplerate) (>= samplerate 49) (< samplerate #x400000))
      (assertion-violation 'opl3-reset! "unsupported sample rate" samplerate))
    (reset! c samplerate))

  ;; (opl3-make [samplerate]): a fresh chip, already reset (default 49716 Hz).
  (define opl3-make
    (case-lambda
      [() (opl3-make OPL3-NATIVE-RATE)]
      [(samplerate)
       (let ([c (make-fxvector CHIP-SIZE 0)])
         (opl3-reset! c samplerate)
         c)]))

  ;; (opl3-write-reg! chip reg v): OPL3_WriteReg. reg is 0x000-0x1ff, bank 1
  ;; at 0x100. reg and v are truncated to Bit16u and Bit8u as in C.
  (define (opl3-write-reg! c reg v)
    (check-chip 'opl3-write-reg! c)
    (check-reg 'opl3-write-reg! reg v)
    (write-reg! c reg v))

  ;; (opl3-write-reg-buffered! chip reg v): OPL3_WriteRegBuffered. The write
  ;; takes effect OPL_WRITEBUF_DELAY (2) native samples after the previous
  ;; buffered one. When the 1024-entry ring is full, its oldest entry is
  ;; applied immediately.
  (define (opl3-write-reg-buffered! c reg v)
    (check-chip 'opl3-write-reg-buffered! c)
    (check-reg 'opl3-write-reg-buffered! reg v)
    (write-reg-buffered! c reg v))

  ;; (opl3-generate! chip bv offset): OPL3_Generate. One frame at the native
  ;; 49716 Hz rate, bypassing the resampler, as s16le L R at byte offset.
  (define (opl3-generate! c bv off)
    (check-chip 'opl3-generate! c)
    (check-out 'opl3-generate! bv off 1)
    (generate! c bv off))

  ;; (opl3-generate-resampled! chip bv offset): OPL3_GenerateResampled. One
  ;; frame at the reset sample rate, as s16le L R (4 bytes) at byte offset.
  (define (opl3-generate-resampled! c bv off)
    (check-chip 'opl3-generate-resampled! c)
    (check-out 'opl3-generate-resampled! bv off 1)
    (resampled! c bv off))

  ;; (opl3-generate-stream! chip bv offset nframes): OPL3_GenerateStream.
  ;; nframes frames at the reset sample rate, as interleaved s16le L R from
  ;; byte offset (4 * nframes bytes).
  (define (opl3-generate-stream! c bv off n)
    (check-chip 'opl3-generate-stream! c)
    (check-out 'opl3-generate-stream! bv off n)
    (stream! c bv off n)))
