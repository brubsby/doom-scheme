;;; i_oplmusic: Chocolate Doom's OPL music player (src/i_oplmusic.c), which
;;; plays the D_* music lumps through an emulated OPL chip with the GENMIDI
;;; instrument bank, the way DMX did. linuxdoom has no music code, so this
;;; follows Chocolate Doom. tests/music.ss checks the output byte for byte
;;; against the C, built with tools/oracle/probes/music_probe.c.
;;;
;;; A song lump (MUS, converted by (doom mus2mid), or a MIDI file) becomes a
;;; (doom midifile) file. Playing it schedules one callback per track on
;;; (doom opl)'s queue for the track's next event; each callback processes
;;; the event (voice allocation, GENMIDI instrument load, volume, pan, pitch
;;; bend, tempo) as register writes, then schedules the next one. The audio
;;; side calls oplmusic-generate!, which runs those callbacks at their sample
;;; positions while it generates, as opl_sdl.c's mixing callback does.
;;;
;;; Defaults are Chocolate Doom's for Doom: opl_drv_ver = opl_doom_1_9,
;;; OPL2 mode with 9 voices unless the DMX option string has "-opl3" (then
;;; OPL3 with 18 voices and pan), DMX's reversed stereo unless it has
;;; "-reverse", and the music volume starts at 0 until set (s_sound sets it
;;; at startup).
;;;
;;; C pointers become indices: an instrument is its byte offset in the
;;; GENMIDI lump (-1 is NULL), a channel is 0-15 (-1 is NULL), and voices are
;;; 0-17 with their fields in fxvectors.
(library (doom i_oplmusic)
  (export oplmusic-init oplmusic-register-song oplmusic-unregister-song
          oplmusic-play-song oplmusic-stop-song oplmusic-pause-song
          oplmusic-resume-song oplmusic-set-volume oplmusic-playing?
          oplmusic-generate! oplmusic-dmxoption oplmusic-driver-version)
  (import (chezscheme) (doom w_wad) (doom opl) (doom mus2mid) (doom midifile))

  (define MAXMIDLENGTH (* 96 1024))
  (define GENMIDI-NUM-INSTRS 128)
  (define GENMIDI-NUM-PERCUSSION 47)
  (define GENMIDI-HEADER-LENGTH 8)      ; "#OPL_II#"
  (define GENMIDI-FLAG-FIXED #x0001)    ; fixed pitch
  (define GENMIDI-FLAG-2VOICE #x0004)   ; double voice (OPL3)
  (define OPL-NUM-VOICES 9)

  (define OPL-REGS-TREMOLO #x20)
  (define OPL-REGS-LEVEL #x40)
  (define OPL-REGS-ATTACK #x60)
  (define OPL-REGS-SUSTAIN #x80)
  (define OPL-REGS-WAVEFORM #xe0)
  (define OPL-REGS-FREQ-1 #xa0)
  (define OPL-REGS-FREQ-2 #xb0)
  (define OPL-REGS-FEEDBACK #xc0)

  ;; (oplmusic-dmxoption): the DMX option string, snd_dmxoption. Like C,
  ;; the DMXOPTION environment variable sets it when present. Read by
  ;; oplmusic-init: "-opl3" selects OPL3 mode, "-reverse" the corrected
  ;; stereo.
  (define oplmusic-dmxoption
    (make-parameter (or (getenv "DMXOPTION") "")
      (lambda (s) (unless (string? s) (assertion-violation 'oplmusic-dmxoption "not a string" s)) s)))

  ;; (oplmusic-driver-version): opl_drv_ver, the DMX version whose voice
  ;; allocation to emulate: 'doom-1.9 (Doom 1.9, the default), 'doom1-1.666
  ;; or 'doom2-1.666 (Doom 2 1.666, Heretic, Hexen). Read by oplmusic-init.
  (define oplmusic-driver-version
    (make-parameter 'doom-1.9
      (lambda (v)
        (unless (memq v '(doom1-1.666 doom2-1.666 doom-1.9))
          (assertion-violation 'oplmusic-driver-version "unknown driver version" v))
        v)))

  ;; opl_driver_ver_t, in C's order.
  (define opl-doom1-1-666 0)
  (define opl-doom2-1-666 1)
  (define opl-doom-1-9 2)
  (define opl-drv-ver opl-doom-1-9)

  ;; Frequency values to use for each note.
  (define frequency-curve
    (fxvector
     ;; -1
     #x133 #x133 #x134 #x134 #x135 #x136 #x136 #x137
     #x137 #x138 #x138 #x139 #x139 #x13a #x13b #x13b
     #x13c #x13c #x13d #x13d #x13e #x13f #x13f #x140
     #x140 #x141 #x142 #x142 #x143 #x143 #x144 #x144
     ;; -2
     #x145 #x146 #x146 #x147 #x147 #x148 #x149 #x149
     #x14a #x14a #x14b #x14c #x14c #x14d #x14d #x14e
     #x14f #x14f #x150 #x150 #x151 #x152 #x152 #x153
     #x153 #x154 #x155 #x155 #x156 #x157 #x157 #x158
     ;; 0: used for the first seven MIDI note values
     #x158 #x159 #x15a #x15a #x15b #x15b #x15c #x15d
     #x15d #x15e #x15f #x15f #x160 #x161 #x161 #x162
     #x162 #x163 #x164 #x164 #x165 #x166 #x166 #x167
     #x168 #x168 #x169 #x16a #x16a #x16b #x16c #x16c
     ;; 1
     #x16d #x16e #x16e #x16f #x170 #x170 #x171 #x172
     #x172 #x173 #x174 #x174 #x175 #x176 #x176 #x177
     #x178 #x178 #x179 #x17a #x17a #x17b #x17c #x17c
     #x17d #x17e #x17e #x17f #x180 #x181 #x181 #x182
     ;; 2
     #x183 #x183 #x184 #x185 #x185 #x186 #x187 #x188
     #x188 #x189 #x18a #x18a #x18b #x18c #x18d #x18d
     #x18e #x18f #x18f #x190 #x191 #x192 #x192 #x193
     #x194 #x194 #x195 #x196 #x197 #x197 #x198 #x199
     ;; 3
     #x19a #x19a #x19b #x19c #x19d #x19d #x19e #x19f
     #x1a0 #x1a0 #x1a1 #x1a2 #x1a3 #x1a3 #x1a4 #x1a5
     #x1a6 #x1a6 #x1a7 #x1a8 #x1a9 #x1a9 #x1aa #x1ab
     #x1ac #x1ad #x1ad #x1ae #x1af #x1b0 #x1b0 #x1b1
     ;; 4
     #x1b2 #x1b3 #x1b4 #x1b4 #x1b5 #x1b6 #x1b7 #x1b8
     #x1b8 #x1b9 #x1ba #x1bb #x1bc #x1bc #x1bd #x1be
     #x1bf #x1c0 #x1c0 #x1c1 #x1c2 #x1c3 #x1c4 #x1c4
     #x1c5 #x1c6 #x1c7 #x1c8 #x1c9 #x1c9 #x1ca #x1cb
     ;; 5
     #x1cc #x1cd #x1ce #x1ce #x1cf #x1d0 #x1d1 #x1d2
     #x1d3 #x1d3 #x1d4 #x1d5 #x1d6 #x1d7 #x1d8 #x1d8
     #x1d9 #x1da #x1db #x1dc #x1dd #x1de #x1de #x1df
     #x1e0 #x1e1 #x1e2 #x1e3 #x1e4 #x1e5 #x1e5 #x1e6
     ;; 6
     #x1e7 #x1e8 #x1e9 #x1ea #x1eb #x1ec #x1ed #x1ed
     #x1ee #x1ef #x1f0 #x1f1 #x1f2 #x1f3 #x1f4 #x1f5
     #x1f6 #x1f6 #x1f7 #x1f8 #x1f9 #x1fa #x1fb #x1fc
     #x1fd #x1fe #x1ff #x200 #x201 #x201 #x202 #x203
     ;; 7: first note of the looped range used for all octaves
     #x204 #x205 #x206 #x207 #x208 #x209 #x20a #x20b
     #x20c #x20d #x20e #x20f #x210 #x210 #x211 #x212
     #x213 #x214 #x215 #x216 #x217 #x218 #x219 #x21a
     #x21b #x21c #x21d #x21e #x21f #x220 #x221 #x222
     ;; 8
     #x223 #x224 #x225 #x226 #x227 #x228 #x229 #x22a
     #x22b #x22c #x22d #x22e #x22f #x230 #x231 #x232
     #x233 #x234 #x235 #x236 #x237 #x238 #x239 #x23a
     #x23b #x23c #x23d #x23e #x23f #x240 #x241 #x242
     ;; 9
     #x244 #x245 #x246 #x247 #x248 #x249 #x24a #x24b
     #x24c #x24d #x24e #x24f #x250 #x251 #x252 #x253
     #x254 #x256 #x257 #x258 #x259 #x25a #x25b #x25c
     #x25d #x25e #x25f #x260 #x262 #x263 #x264 #x265
     ;; 10
     #x266 #x267 #x268 #x269 #x26a #x26c #x26d #x26e
     #x26f #x270 #x271 #x272 #x273 #x275 #x276 #x277
     #x278 #x279 #x27a #x27b #x27d #x27e #x27f #x280
     #x281 #x282 #x284 #x285 #x286 #x287 #x288 #x289
     ;; 11
     #x28b #x28c #x28d #x28e #x28f #x290 #x292 #x293
     #x294 #x295 #x296 #x298 #x299 #x29a #x29b #x29c
     #x29e #x29f #x2a0 #x2a1 #x2a2 #x2a4 #x2a5 #x2a6
     #x2a7 #x2a9 #x2aa #x2ab #x2ac #x2ae #x2af #x2b0
     ;; 12
     #x2b1 #x2b2 #x2b4 #x2b5 #x2b6 #x2b7 #x2b9 #x2ba
     #x2bb #x2bd #x2be #x2bf #x2c0 #x2c2 #x2c3 #x2c4
     #x2c5 #x2c7 #x2c8 #x2c9 #x2cb #x2cc #x2cd #x2ce
     #x2d0 #x2d1 #x2d2 #x2d4 #x2d5 #x2d6 #x2d8 #x2d9
     ;; 13
     #x2da #x2dc #x2dd #x2de #x2e0 #x2e1 #x2e2 #x2e4
     #x2e5 #x2e6 #x2e8 #x2e9 #x2ea #x2ec #x2ed #x2ee
     #x2f0 #x2f1 #x2f2 #x2f4 #x2f5 #x2f6 #x2f8 #x2f9
     #x2fb #x2fc #x2fd #x2ff #x300 #x302 #x303 #x304
     ;; 14
     #x306 #x307 #x309 #x30a #x30b #x30d #x30e #x310
     #x311 #x312 #x314 #x315 #x317 #x318 #x31a #x31b
     #x31c #x31e #x31f #x321 #x322 #x324 #x325 #x327
     #x328 #x329 #x32b #x32c #x32e #x32f #x331 #x332
     ;; 15
     #x334 #x335 #x337 #x338 #x33a #x33b #x33d #x33e
     #x340 #x341 #x343 #x344 #x346 #x347 #x349 #x34a
     #x34c #x34d #x34f #x350 #x352 #x353 #x355 #x357
     #x358 #x35a #x35b #x35d #x35e #x360 #x361 #x363
     ;; 16
     #x365 #x366 #x368 #x369 #x36b #x36c #x36e #x370
     #x371 #x373 #x374 #x376 #x378 #x379 #x37b #x37c
     #x37e #x380 #x381 #x383 #x384 #x386 #x388 #x389
     #x38b #x38d #x38e #x390 #x392 #x393 #x395 #x397
     ;; 17
     #x398 #x39a #x39c #x39d #x39f #x3a1 #x3a2 #x3a4
     #x3a6 #x3a7 #x3a9 #x3ab #x3ac #x3ae #x3b0 #x3b1
     #x3b3 #x3b5 #x3b7 #x3b8 #x3ba #x3bc #x3bd #x3bf
     #x3c1 #x3c3 #x3c4 #x3c6 #x3c8 #x3ca #x3cb #x3cd
     ;; 18: incomplete; the last value is a DMX buffer overrun
     #x3cf #x3d1 #x3d2 #x3d4 #x3d6 #x3d8 #x3da #x3db
     #x3dd #x3df #x3e1 #x3e3 #x3e4 #x3e6 #x3e8 #x3ea
     #x3ec #x3ed #x3ef #x3f1 #x3f3 #x3f5 #x3f6 #x3f8
     #x3fa #x3fc #x3fe #x36c))

  ;; Mapping from MIDI volume level to OPL level value.
  (define volume-mapping-table
    (fxvector
     0 1 3 5 6 8 10 11
     13 14 16 17 19 20 22 23
     25 26 27 29 30 32 33 34
     36 37 39 41 43 45 47 49
     50 52 54 55 57 59 60 61
     63 64 66 67 68 69 71 72
     73 74 75 76 77 79 80 81
     82 83 84 84 85 86 87 88
     89 90 91 92 92 93 94 95
     96 96 97 98 99 99 100 101
     101 102 103 103 104 105 105 106
     107 107 108 109 109 110 110 111
     112 112 113 113 114 114 115 115
     116 117 117 118 118 119 119 120
     120 121 121 122 122 123 123 123
     124 124 125 125 126 126 127 127))

  ;; A MIDI file's data bytes can be up to 255 where DMX expects 0-127 (MUS
  ;; never has them): C then reads past the end of the table. Clamp instead.
  (define (volume-mapping v) (fxvector-ref volume-mapping-table (fxmin v 127)))

  (define music-initialized #f)
  (define start-music-volume 0)
  (define current-music-volume 0)

  ;; GENMIDI lump instrument data: the lump, and the offsets of the main and
  ;; percussion instrument tables.
  (define genmidi (make-bytevector 0))
  (define main-instrs GENMIDI-HEADER-LENGTH)
  (define percussion-instrs (+ GENMIDI-HEADER-LENGTH (* 36 GENMIDI-NUM-INSTRS)))
  ;; A program change can name instrument 0-255 (main_instrs[255] in C);
  ;; the lump is padded so that every such instrument can be read.
  (define GENMIDI-MIN-LENGTH (+ GENMIDI-HEADER-LENGTH (* 36 256)))

  (define (g8 off) (bytevector-u8-ref genmidi off))
  ;; genmidi_instr_t: flags, fine_tuning, fixed_note, voices[2].
  (define (instr-flags instr) (bytevector-u16-ref genmidi instr (endianness little)))
  (define (instr-fine-tuning instr) (g8 (fx+ instr 2)))
  (define (instr-fixed-note instr) (g8 (fx+ instr 3)))
  ;; genmidi_voice_t: modulator op at 0, feedback at 6, carrier op at 7,
  ;; base_note_offset at 14. genmidi_op_t: tremolo, attack, sustain,
  ;; waveform, scale, level.
  (define (instr-voice instr v) (fx+ instr 4 (fx* 16 v)))
  (define (gv-modulator gv) gv)
  (define (gv-feedback gv) (g8 (fx+ gv 6)))
  (define (gv-carrier gv) (fx+ gv 7))
  (define (gv-base-note-offset gv) (bytevector-s16-ref genmidi (fx+ gv 14) (endianness little)))
  (define (op-tremolo op) (g8 op))
  (define (op-attack op) (g8 (fx+ op 1)))
  (define (op-sustain op) (g8 (fx+ op 2)))
  (define (op-waveform op) (g8 (fx+ op 3)))
  (define (op-scale op) (g8 (fx+ op 4)))
  (define (op-level op) (g8 (fx+ op 5)))

  ;; Voices (opl_voice_t), 18 for OPL3, of which OPL2 uses 9.
  (define NV (* 2 OPL-NUM-VOICES))
  (define v-index (make-fxvector NV 0))         ; index of this voice
  (define v-op1 (make-fxvector NV 0))           ; the operators used by it
  (define v-op2 (make-fxvector NV 0))
  (define v-array (make-fxvector NV 0))         ; array used by voice
  (define v-instr (make-fxvector NV -1))        ; currently-loaded instrument
  (define v-instr-voice (make-fxvector NV 0))   ; voice number in the instrument
  (define v-channel (make-fxvector NV -1))      ; channel using this voice
  (define v-key (make-fxvector NV 0))           ; MIDI key it is playing
  (define v-note (make-fxvector NV 0))          ; the note (fixed-pitch: not the key)
  (define v-freq (make-fxvector NV 0))          ; frequency value being used
  (define v-note-volume (make-fxvector NV 0))   ; volume of the note
  (define v-car-volume (make-fxvector NV 0))    ; level register values set
  (define v-mod-volume (make-fxvector NV 0))
  (define v-reg-pan (make-fxvector NV 0))
  (define v-priority (make-fxvector NV 0))

  (define voice-free-list (make-fxvector NV 0))
  (define voice-alloced-list (make-fxvector NV 0))
  (define voice-free-num 0)
  (define voice-alloced-num 0)
  (define opl-opl3mode #f)
  (define num-opl-voices OPL-NUM-VOICES)

  ;; Data for each channel (opl_channel_data_t).
  (define ch-instrument (make-fxvector MIDI-CHANNELS-PER-TRACK -1))
  (define ch-volume (make-fxvector MIDI-CHANNELS-PER-TRACK 0))
  (define ch-volume-base (make-fxvector MIDI-CHANNELS-PER-TRACK 0))
  (define ch-pan (make-fxvector MIDI-CHANNELS-PER-TRACK 0))
  (define ch-bend (make-fxvector MIDI-CHANNELS-PER-TRACK 0))

  ;; Track data for playing tracks: their iterators.
  (define tracks (vector))
  (define num-tracks 0)
  (define running-tracks 0)             ; unsigned int
  (define song-looping #f)

  ;; Tempo control variables
  (define ticks-per-beat 0)
  (define us-per-beat 0)

  ;; If true, OPL sound channels are reversed to their correct arrangement
  ;; (as intended by the MIDI standard) rather than the backwards one used
  ;; by DMX due to a bug.
  (define opl-stereo-correct #f)

  (define (write-register reg value) (opl-write-register reg value))

  ;; Load instrument table from GENMIDI lump. DMX does not check the header.
  (define (load-instrument-table! lump)
    (let* ([data (w-cache-lump-num lump)]
           [n (bytevector-length data)]
           [bv (make-bytevector (max n GENMIDI-MIN-LENGTH) 0)])
      (bytevector-copy! data 0 bv 0 n)
      (set! genmidi bv)))

  ;; Get the next available voice from the freelist, or #f.
  (define (get-free-voice)
    (and (fx> voice-free-num 0)
         (let ([result (fxvector-ref voice-free-list 0)])
           ;; Remove from free list
           (set! voice-free-num (fx- voice-free-num 1))
           (do ([i 0 (fx+ i 1)]) ((fx= i voice-free-num))
             (fxvector-set! voice-free-list i (fxvector-ref voice-free-list (fx+ i 1))))
           ;; Add to allocated list
           (fxvector-set! voice-alloced-list voice-alloced-num result)
           (set! voice-alloced-num (fx+ voice-alloced-num 1))
           result)))

  ;; Release a voice back to the freelist.
  (define (release-voice! index)
    (if (fx>= index voice-alloced-num)
        ;; Doom 2 1.666 OPL crash emulation.
        (begin (set! voice-alloced-num 0) (set! voice-free-num 0))
        (let* ([voice (fxvector-ref voice-alloced-list index)]
               [double-voice (not (fx= 0 (fxvector-ref v-instr-voice voice)))])
          (voice-key-off! voice)
          (fxvector-set! v-channel voice -1)
          (fxvector-set! v-note voice 0)
          ;; Remove from alloced list.
          (set! voice-alloced-num (fx- voice-alloced-num 1))
          (do ([i index (fx+ i 1)]) ((fx>= i voice-alloced-num))
            (fxvector-set! voice-alloced-list i (fxvector-ref voice-alloced-list (fx+ i 1))))
          ;; Search to the end of the freelist (This is how Doom behaves!)
          (fxvector-set! voice-free-list voice-free-num voice)
          (set! voice-free-num (fx+ voice-free-num 1))
          (when (and double-voice (fx< opl-drv-ver opl-doom-1-9))
            (release-voice! index)))))

  ;; Load data to the specified operator; returns the level register value.
  ;; For the carrier wave we always set the maximum level.
  (define (load-operator-data! operator op max-level)
    (let ([level (fxlogor (op-scale op) (if max-level #x3f (op-level op)))])
      (write-register (fx+ OPL-REGS-LEVEL operator) level)
      (write-register (fx+ OPL-REGS-TREMOLO operator) (op-tremolo op))
      (write-register (fx+ OPL-REGS-ATTACK operator) (op-attack op))
      (write-register (fx+ OPL-REGS-SUSTAIN operator) (op-sustain op))
      (write-register (fx+ OPL-REGS-WAVEFORM operator) (op-waveform op))
      level))

  ;; Set the instrument for a particular voice.
  (define (set-voice-instrument! voice instr instr-voice-num)
    ;; Instrument already set for this channel?
    (unless (and (fx= (fxvector-ref v-instr voice) instr)
                 (fx= (fxvector-ref v-instr-voice voice) instr-voice-num))
      (fxvector-set! v-instr voice instr)
      (fxvector-set! v-instr-voice voice instr-voice-num)
      (let* ([data (instr-voice instr instr-voice-num)]
             [array (fxvector-ref v-array voice)]
             ;; Are we using modulated feedback mode?
             [modulating (fx= 0 (fxlogand (gv-feedback data) 1))])
        ;; Doom loads the second operator first, then the first. The
        ;; carrier is set to minimum volume until the voice volume is set in
        ;; set-voice-volume! (below). If we are not using modulating mode,
        ;; we must set both to minimum volume.
        (fxvector-set! v-car-volume voice
          (load-operator-data! (fxlogor (fxvector-ref v-op2 voice) array) (gv-carrier data) #t))
        (fxvector-set! v-mod-volume voice
          (load-operator-data! (fxlogor (fxvector-ref v-op1 voice) array) (gv-modulator data)
                               (not modulating)))
        ;; Set feedback register that control the connection between the two
        ;; operators. Turn on bits in the upper nybble; I think this is for
        ;; OPL3, where it turns on channel A/B.
        (write-register (fxlogor (fx+ OPL-REGS-FEEDBACK (fxvector-ref v-index voice)) array)
                        (fxlogor (gv-feedback data) (fxvector-ref v-reg-pan voice)))
        ;; Calculate voice priority.
        (let ([carrier (gv-carrier data)])
          (fxvector-set! v-priority voice
            (fx+ (fx- #x0f (fxsra (op-attack carrier) 4))
                 (fx- #x0f (fxlogand (op-sustain carrier) #x0f))))))))

  (define (set-voice-volume! voice volume)
    (fxvector-set! v-note-volume voice volume)
    (let* ([opl-voice (instr-voice (fxvector-ref v-instr voice) (fxvector-ref v-instr-voice voice))]
           [array (fxvector-ref v-array voice)]
           ;; Multiply note volume and channel volume to get the actual
           ;; volume.
           [midi-volume (fx* 2 (fx+ (volume-mapping (fxvector-ref ch-volume (fxvector-ref v-channel voice))) 1))]
           [full-volume (fxsra (fx* (volume-mapping volume) midi-volume) 9)]
           ;; The volume value to use in the register:
           [car-volume (fx- #x3f full-volume)])
      ;; Update the volume register(s) if necessary.
      (unless (fx= car-volume (fxlogand (fxvector-ref v-car-volume voice) #x3f))
        (fxvector-set! v-car-volume voice
          (fxlogor car-volume (fxlogand (fxvector-ref v-car-volume voice) #xc0)))
        (write-register (fxlogor (fx+ OPL-REGS-LEVEL (fxvector-ref v-op2 voice)) array)
                        (fxvector-ref v-car-volume voice))
        ;; If we are using non-modulated feedback mode, we must set the
        ;; volume for both voices.
        (let ([modulator (gv-modulator opl-voice)])
          (when (and (not (fx= 0 (fxlogand (gv-feedback opl-voice) 1)))
                     (not (fx= (op-level modulator) #x3f)))
            (let* ([mod-volume (fxmax (op-level modulator) car-volume)]
                   [mod-volume (fxlogor mod-volume (fxlogand (fxvector-ref v-mod-volume voice) #xc0))])
              (unless (fx= mod-volume (fxvector-ref v-mod-volume voice))
                (fxvector-set! v-mod-volume voice mod-volume)
                (write-register (fxlogor (fx+ OPL-REGS-LEVEL (fxvector-ref v-op1 voice)) array)
                                (fxlogor mod-volume (fxlogand (op-scale modulator) #xc0))))))))))

  (define (set-voice-pan! voice pan)
    (fxvector-set! v-reg-pan voice pan)
    (let ([opl-voice (instr-voice (fxvector-ref v-instr voice) (fxvector-ref v-instr-voice voice))])
      (write-register (fxlogor (fx+ OPL-REGS-FEEDBACK (fxvector-ref v-index voice))
                               (fxvector-ref v-array voice))
                      (fxlogor (gv-feedback opl-voice) pan))))

  ;; Operators used by the different voices.
  (define voice-operators-1 (fxvector #x00 #x01 #x02 #x08 #x09 #x0a #x10 #x11 #x12))
  (define voice-operators-2 (fxvector #x03 #x04 #x05 #x0b #x0c #x0d #x13 #x14 #x15))

  ;; Initialize the voice table and freelist
  (define (init-voices!)
    ;; Start with an empty free list.
    (set! voice-free-num num-opl-voices)
    (set! voice-alloced-num 0)
    ;; Initialize each voice.
    (do ([i 0 (fx+ i 1)]) ((fx= i num-opl-voices))
      (let ([n (fxremainder i OPL-NUM-VOICES)])
        (fxvector-set! v-index i n)
        (fxvector-set! v-op1 i (fxvector-ref voice-operators-1 n))
        (fxvector-set! v-op2 i (fxvector-ref voice-operators-2 n))
        (fxvector-set! v-array i (fxsll (fxquotient i OPL-NUM-VOICES) 8))
        (fxvector-set! v-instr i -1)
        ;; Add this voice to the freelist.
        (fxvector-set! voice-free-list i i))))

  ;; Set music volume (0 - 127): I_OPL_SetMusicVolume.
  (define (set-music-volume! volume)
    (unless (fx= current-music-volume volume)
      ;; Internal state variable.
      (set! current-music-volume volume)
      ;; Update the volume of all voices.
      (do ([i 0 (fx+ i 1)]) ((fx= i MIDI-CHANNELS-PER-TRACK))
        (if (fx= i 15)
            (set-channel-volume! i volume #f)
            (set-channel-volume! i (fxvector-ref ch-volume-base i) #f)))))

  (define (voice-key-off! voice)
    (write-register (fxlogor (fx+ OPL-REGS-FREQ-2 (fxvector-ref v-index voice))
                             (fxvector-ref v-array voice))
                    (fxsrl (fxvector-ref v-freq voice) 8)))

  ;; MIDI uses track #9 for percussion, but for MUS it's track #15 instead.
  ;; Because DMX works on MUS data internally, we need to swap back to the
  ;; MUS version of the channel number.
  (define (track-channel-for-event event)
    (let ([channel-num (midi-event-channel event)])
      (case channel-num [(9) 15] [(15) 9] [else channel-num])))

  (define (key-off-event! event)
    (let ([channel (track-channel-for-event event)]
          [key (midi-event-param1 event)])
      ;; Turn off voices being used to play this key. If it is a double
      ;; voice instrument there will be two.
      (let loop ([i 0])
        (when (fx< i voice-alloced-num)
          (let ([voice (fxvector-ref voice-alloced-list i)])
            (if (and (fx= (fxvector-ref v-channel voice) channel)
                     (fx= (fxvector-ref v-key voice) key))
                ;; Finished with this voice now.
                (begin (release-voice! i) (loop i))
                (loop (fx+ i 1))))))))

  ;; When all voices are in use, we must discard an existing voice to play
  ;; a new note. A voice playing the second voice of an instrument goes
  ;; first; otherwise the last allocated on the highest-numbered channel.
  (define (alloced-channel i) (fxvector-ref v-channel (fxvector-ref voice-alloced-list i)))
  (define (replace-existing-voice!)
    (let loop ([i 0] [result 0])
      (if (fx< i voice-alloced-num)
          (loop (fx+ i 1)
                (if (or (not (fx= 0 (fxvector-ref v-instr-voice (fxvector-ref voice-alloced-list i))))
                        (fx>= (alloced-channel i) (alloced-channel result)))
                    i result))
          (release-voice! result))))

  ;; Alternate versions of replace-existing-voice! used when emulating old
  ;; versions of the DMX library used in Doom 1.666, Heretic and Hexen.
  (define (replace-existing-voice-doom1!)
    (let loop ([i 0] [result 0])
      (if (fx< i voice-alloced-num)
          (loop (fx+ i 1) (if (fx> (alloced-channel i) (alloced-channel result)) i result))
          (release-voice! result))))

  (define (replace-existing-voice-doom2! channel)
    (let loop ([i 0] [result 0] [priority #x8000])
      (if (fx< i (fx- voice-alloced-num 3))
          (let ([voice (fxvector-ref voice-alloced-list i)])
            (if (and (fx< (fxvector-ref v-priority voice) priority)
                     (fx>= (fxvector-ref v-channel voice) channel))
                (loop (fx+ i 1) i (fxvector-ref v-priority voice))
                (loop (fx+ i 1) result priority)))
          (release-voice! result))))

  ;; Get the frequency that we should be using for a voice.
  (define (frequency-for-voice voice)
    (let* ([instr (fxvector-ref v-instr voice)]
           [iv (fxvector-ref v-instr-voice voice)]
           [gm-voice (instr-voice instr iv)]
           [note (fxvector-ref v-note voice)]
           ;; Apply note offset. Don't apply offset if the instrument is a
           ;; fixed note instrument.
           [note (if (fx= 0 (fxlogand (instr-flags instr) GENMIDI-FLAG-FIXED))
                     (fx+ note (gv-base-note-offset gm-voice))
                     note)]
           ;; Avoid possible overflow due to base note offset:
           [note (let loop ([note note]) (if (fx< note 0) (loop (fx+ note 12)) note))]
           [note (let loop ([note note]) (if (fx> note 95) (loop (fx- note 12)) note))]
           [freq-index (fx+ 64 (fx* 32 note) (fxvector-ref ch-bend (fxvector-ref v-channel voice)))]
           ;; If this is the second voice of a double voice instrument, the
           ;; frequency index can be adjusted by the fine tuning field.
           [freq-index (if (fx= iv 0)
                           freq-index
                           (fx+ freq-index (fx- (fxquotient (instr-fine-tuning instr) 2) 64)))]
           [freq-index (fxmax freq-index 0)])
      ;; The first 7 notes use the start of the table, while consecutive
      ;; notes loop around the latter part.
      (if (fx< freq-index 284)
          (fxvector-ref frequency-curve freq-index)
          (let ([sub-index (fxremainder (fx- freq-index 284) (* 12 32))]
                ;; Once the seventh octave is reached, things break down:
                ;; octave 7 is the most the register can hold.
                [octave (fxmin 7 (fxquotient (fx- freq-index 284) (* 12 32)))])
            ;; Calculate the resulting register value to use for the
            ;; frequency.
            (fxlogor (fxvector-ref frequency-curve (fx+ sub-index 284)) (fxsll octave 10))))))

  ;; Update the frequency that a voice is programmed to use.
  (define (update-voice-frequency! voice)
    (let ([freq (frequency-for-voice voice)]
          [index (fxvector-ref v-index voice)]
          [array (fxvector-ref v-array voice)])
      (unless (fx= (fxvector-ref v-freq voice) freq)
        (write-register (fxlogor (fx+ OPL-REGS-FREQ-1 index) array) (fxlogand freq #xff))
        (write-register (fxlogor (fx+ OPL-REGS-FREQ-2 index) array) (fxlogor (fxsrl freq 8) #x20))
        (fxvector-set! v-freq voice freq))))

  ;; Program a single voice for an instrument. For a double voice
  ;; instrument (GENMIDI_FLAG_2VOICE), this is called twice for each key on
  ;; event.
  (define (voice-key-on! channel instrument instrument-voice note key volume)
    (let ([instrument-voice (if (and (not opl-opl3mode) (fx= opl-drv-ver opl-doom1-1-666))
                                0
                                instrument-voice)]
          ;; Find a voice to use for this new note.
          [voice (get-free-voice)])
      (when voice
        (fxvector-set! v-channel voice channel)
        (fxvector-set! v-key voice key)
        ;; Work out the note to use. This is normally the same as the key,
        ;; unless it is a fixed pitch instrument.
        (fxvector-set! v-note voice
          (if (fx= 0 (fxlogand (instr-flags instrument) GENMIDI-FLAG-FIXED))
              note
              (instr-fixed-note instrument)))
        (fxvector-set! v-reg-pan voice (fxvector-ref ch-pan channel))
        ;; Program the voice with the instrument data:
        (set-voice-instrument! voice instrument instrument-voice)
        ;; Set the volume level.
        (set-voice-volume! voice volume)
        ;; Write the frequency value to turn the note on.
        (fxvector-set! v-freq voice 0)
        (update-voice-frequency! voice))))

  (define (key-on-event! event)
    (let ([key (midi-event-param1 event)]
          [volume (midi-event-param2 event)])
      ;; A volume of zero means key off. Some MIDI tracks, eg. the ones in
      ;; AV.wad, use a second key on with a volume of zero to mean key off.
      (if (fx= volume 0)
          (key-off-event! event)
          (let ([channel (track-channel-for-event event)])
            (let-values ([(instrument note)
                          ;; Percussion channel is treated differently.
                          (if (fx= (midi-event-channel event) 9)
                              (values (and (fx<= 35 key 81)
                                           (fx+ percussion-instrs (fx* 36 (fx- key 35))))
                                      60)
                              (values (fxvector-ref ch-instrument channel) key))])
              (when instrument
                (let ([double-voice (not (fx= 0 (fxlogand (instr-flags instrument) GENMIDI-FLAG-2VOICE)))])
                  (cond
                    [(fx= opl-drv-ver opl-doom1-1-666)
                     (let ([voicenum (if opl-opl3mode (if double-voice 2 1) 1)])
                       (let loop ()
                         (when (fx> voice-alloced-num (fx- num-opl-voices voicenum))
                           (replace-existing-voice-doom1!)
                           (loop))))
                     ;; Find and program a voice for this instrument. If this
                     ;; is a double voice instrument, we must do this twice.
                     (when double-voice
                       (voice-key-on! channel instrument 1 note key volume))
                     (voice-key-on! channel instrument 0 note key volume)]
                    [(fx= opl-drv-ver opl-doom2-1-666)
                     (when (fx= voice-alloced-num num-opl-voices)
                       (replace-existing-voice-doom2! channel))
                     (when (and (fx= voice-alloced-num (fx- num-opl-voices 1)) double-voice)
                       (replace-existing-voice-doom2! channel))
                     (when double-voice
                       (voice-key-on! channel instrument 1 note key volume))
                     (voice-key-on! channel instrument 0 note key volume)]
                    [else
                     (when (fx= voice-free-num 0)
                       (replace-existing-voice!))
                     (voice-key-on! channel instrument 0 note key volume)
                     (when double-voice
                       (voice-key-on! channel instrument 1 note key volume))]))))))))

  (define (program-change-event! event)
    ;; Set the instrument used on this channel.
    (fxvector-set! ch-instrument (track-channel-for-event event)
                   (fx+ main-instrs (fx* 36 (midi-event-param1 event)))))

  (define (set-channel-volume! channel volume clip-start)
    (fxvector-set! ch-volume-base channel volume)
    (let* ([volume (fxmin volume current-music-volume)]
           [volume (if clip-start (fxmin volume start-music-volume) volume)])
      (fxvector-set! ch-volume channel volume)
      ;; Update all voices that this channel is using.
      (do ([i 0 (fx+ i 1)]) ((fx= i num-opl-voices))
        (when (fx= (fxvector-ref v-channel i) channel)
          (set-voice-volume! i (fxvector-ref v-note-volume i))))))

  (define (set-channel-pan! channel pan)
    ;; The DMX library has the stereo channels backwards, maybe because Paul
    ;; Radek had a Soundblaster card with the channels reversed, or perhaps
    ;; it was just a bug in the OPL3 support that was never finished. By
    ;; default we preserve this bug, but we also provide a secret DMXOPTION
    ;; to fix it. (pan is unsigned: 144 - pan wraps.)
    (let ([pan (if opl-stereo-correct (logand (- 144 pan) #xffffffff) pan)])
      (when opl-opl3mode
        (let ([reg-pan (cond [(>= pan 96) #x10] [(<= pan 48) #x20] [else #x30])])
          (unless (fx= (fxvector-ref ch-pan channel) reg-pan)
            (fxvector-set! ch-pan channel reg-pan)
            (do ([i 0 (fx+ i 1)]) ((fx= i num-opl-voices))
              (when (fx= (fxvector-ref v-channel i) channel)
                (set-voice-pan! i reg-pan))))))))

  ;; Handler for the MIDI_CONTROLLER_ALL_NOTES_OFF channel event.
  (define (all-notes-off! channel)
    (let loop ([i 0])
      (when (fx< i voice-alloced-num)
        (if (fx= (alloced-channel i) channel)
            ;; Finished with this voice now.
            (begin (release-voice! i) (loop i))
            (loop (fx+ i 1))))))

  (define (controller-event! event)
    (let ([channel (track-channel-for-event event)]
          [param (midi-event-param2 event)])
      (let ([controller (midi-event-param1 event)])
        (cond
          [(fx= controller MIDI-CONTROLLER-VOLUME-MSB) (set-channel-volume! channel param #t)]
          [(fx= controller MIDI-CONTROLLER-PAN) (set-channel-pan! channel param)]
          [(fx= controller MIDI-CONTROLLER-ALL-NOTES-OFF) (all-notes-off! channel)]))))

  ;; Process a pitch bend event. Only the MSB of the pitch bend value is
  ;; considered: this is what Doom does. The channel's voices move to the
  ;; end of the allocated list.
  (define voice-updated-list (make-fxvector NV 0))
  (define voice-not-updated-list (make-fxvector NV 0))
  (define (pitch-bend-event! event)
    (let ([channel (track-channel-for-event event)])
      (fxvector-set! ch-bend channel (fx- (midi-event-param2 event) 64))
      ;; Update all voices for this channel.
      (let loop ([i 0] [nu 0] [nn 0])
        (if (fx< i voice-alloced-num)
            (let ([voice (fxvector-ref voice-alloced-list i)])
              (if (fx= (fxvector-ref v-channel voice) channel)
                  (begin (update-voice-frequency! voice)
                         (fxvector-set! voice-updated-list nu voice)
                         (loop (fx+ i 1) (fx+ nu 1) nn))
                  (begin (fxvector-set! voice-not-updated-list nn voice)
                         (loop (fx+ i 1) nu (fx+ nn 1)))))
            (begin
              (do ([i 0 (fx+ i 1)]) ((fx= i nn))
                (fxvector-set! voice-alloced-list i (fxvector-ref voice-not-updated-list i)))
              (do ([i 0 (fx+ i 1)]) ((fx= i nu))
                (fxvector-set! voice-alloced-list (fx+ i nn) (fxvector-ref voice-updated-list i))))))))

  ;; (float) us_per_beat / tempo, in single precision.
  (define f32-cell (make-bytevector 4))
  (define (f32 x)
    (bytevector-ieee-single-native-set! f32-cell 0 x)
    (bytevector-ieee-single-native-ref f32-cell 0))

  (define (meta-set-tempo! tempo)
    (opl-adjust-callbacks (f32 (/ (f32 (inexact us-per-beat)) (f32 (inexact tempo)))))
    (set! us-per-beat tempo))

  ;; Process a meta event: only the tempo matters. The end of track is
  ;; handled when we run out of events in the track (track-timer-callback).
  (define (meta-event! event)
    (let ([data (midi-event-data event)])
      (when (and (fx= (midi-event-meta-type event) MIDI-META-SET-TEMPO)
                 (fx= (bytevector-length data) 3))
        (meta-set-tempo! (fxlogor (fxsll (bytevector-u8-ref data 0) 16)
                                  (fxsll (bytevector-u8-ref data 1) 8)
                                  (bytevector-u8-ref data 2))))))

  ;; Process a MIDI event from a track. SysEx and aftertouch are ignored.
  (define (process-event! event)
    (let ([type (midi-event-type event)])
      (cond
        [(fx= type MIDI-EVENT-NOTE-OFF) (key-off-event! event)]
        [(fx= type MIDI-EVENT-NOTE-ON) (key-on-event! event)]
        [(fx= type MIDI-EVENT-CONTROLLER) (controller-event! event)]
        [(fx= type MIDI-EVENT-PROGRAM-CHANGE) (program-change-event! event)]
        [(fx= type MIDI-EVENT-PITCH-BEND) (pitch-bend-event! event)]
        [(fx= type MIDI-EVENT-META) (meta-event! event)])))

  ;; Restart a song from the beginning.
  (define (restart-song _)
    (set! running-tracks num-tracks)
    (set! start-music-volume current-music-volume)
    (do ([i 0 (fx+ i 1)]) ((fx= i num-tracks))
      (midi-restart-iterator! (vector-ref tracks i))
      (schedule-track! (vector-ref tracks i)))
    (do ([i 0 (fx+ i 1)]) ((fx= i MIDI-CHANNELS-PER-TRACK))
      (init-channel! i)))

  ;; Callback function invoked when another event needs to be read from a
  ;; track.
  (define (track-timer-callback track)
    ;; Get the next event and process it.
    (let ([event (midi-next-event track)])
      (when event
        (process-event! event)
        ;; End of track?
        (if (and (fx= (midi-event-type event) MIDI-EVENT-META)
                 (fx= (midi-event-meta-type event) MIDI-META-END-OF-TRACK))
            (begin
              (set! running-tracks (logand (- running-tracks 1) #xffffffff))
              ;; When all tracks have finished, restart the song. Don't
              ;; restart the song immediately, but wait for 5ms before
              ;; triggering a restart. Otherwise it is possible to construct
              ;; an empty MIDI file that causes the game to lock up in an
              ;; infinite loop. (5ms should be short enough not to be
              ;; noticeable by the listener).
              (when (and (= running-tracks 0) song-looping)
                (opl-set-callback 5000 restart-song #f)))
            ;; Reschedule the callback for the next event in the track.
            (schedule-track! track)))))

  (define (schedule-track! track)
    ;; Get the number of microseconds until the next event, and set a timer
    ;; to be invoked when the next event is ready to play.
    (let* ([nticks (midi-delta-time track)]
           [us (quotient (* nticks us-per-beat) ticks-per-beat)])
      (opl-set-callback us track-timer-callback track)))

  ;; Initialize a channel.
  (define (init-channel! channel)
    (fxvector-set! ch-instrument channel main-instrs)
    (fxvector-set! ch-volume channel (fxmin current-music-volume 100))
    (fxvector-set! ch-volume-base channel 100)
    (fxvector-set! ch-pan channel #x30)
    (fxvector-set! ch-bend channel 0))

  ;;
  ;; The music module interface.
  ;;

  ;; (oplmusic-init samplerate): I_OPL_InitMusic, with OPL output at
  ;; samplerate Hz (snd_samplerate; the chip resamples to it). Loads GENMIDI
  ;; from the WAD, which w_wad must already have open: returns #f if there is
  ;; none, else #t. Reads oplmusic-dmxoption and oplmusic-driver-version.
  ;; Calling it again shuts down first (stopping the song), then starts over
  ;; with a fresh chip.
  (define (oplmusic-init samplerate)
    (unless (and (fixnum? samplerate) (>= samplerate 49) (< samplerate #x400000))
      (assertion-violation 'oplmusic-init "unsupported sample rate" samplerate))
    (let ([lump (w-check-num-for-name "GENMIDI")])
      (and (>= lump 0)
           (let ([dmxoption (oplmusic-dmxoption)])
             (define (has? opt)
               (let ([n (string-length dmxoption)] [k (string-length opt)])
                 (let loop ([i 0])
                   (and (<= (+ i k) n)
                        (or (string=? (substring dmxoption i (+ i k)) opt)
                            (loop (+ i 1)))))))
             ;; Starting over: I_OPL_ShutdownMusic first stops the song,
             ;; which frees every voice.
             (oplmusic-stop-song)
             (set! opl-drv-ver
               (case (oplmusic-driver-version)
                 [(doom1-1.666) opl-doom1-1-666]
                 [(doom2-1.666) opl-doom2-1-666]
                 [else opl-doom-1-9]))
             (opl-init samplerate)
             ;; The DMXOPTION variable must be set to enable OPL3 support.
             ;; The emulated chip always detects as an OPL3.
             (set! opl-opl3mode (has? "-opl3"))
             (set! num-opl-voices (if opl-opl3mode (* 2 OPL-NUM-VOICES) OPL-NUM-VOICES))
             ;; Secret, undocumented DMXOPTION that reverses the stereo
             ;; channels into their correct orientation.
             (set! opl-stereo-correct (has? "-reverse"))
             ;; Initialize all registers.
             (opl-init-registers opl-opl3mode)
             ;; Load instruments from GENMIDI lump:
             (load-instrument-table! lump)
             (init-voices!)
             (set! tracks (vector))
             (set! num-tracks 0)
             (set! music-initialized #t)
             #t))))

  ;; (oplmusic-set-volume v): I_OPL_SetMusicVolume, v from 0 to 127.
  (define (oplmusic-set-volume volume)
    (unless (and (fixnum? volume) (fx<= 0 volume 127))
      (assertion-violation 'oplmusic-set-volume "volume must be 0-127" volume))
    (set-music-volume! volume))

  ;; Determine whether memory block is a .mid file
  (define (mid? bv)
    (and (> (bytevector-length bv) 4)
         (= (bytevector-u32-ref bv 0 (endianness big)) #x4d546864)))  ; "MThd"

  ;; (oplmusic-register-song bv): I_OPL_RegisterSong. bv holds a MUS lump
  ;; or a MIDI file (under 96 KB; anything else is taken for MUS). Returns a
  ;; song handle, or #f if it can't be read or music isn't initialized. A
  ;; file whose time division works out to 0 is refused too: C would divide
  ;; by zero when it plays.
  (define (oplmusic-register-song bv)
    (unless (bytevector? bv)
      (assertion-violation 'oplmusic-register-song "not a bytevector" bv))
    (and music-initialized
         (let* ([midi (if (and (mid? bv) (< (bytevector-length bv) MAXMIDLENGTH))
                          bv
                          ;; Assume a MUS file and try to convert
                          (mus2mid bv))]
                [file (and midi (midi-load midi))])
           (unless file
             (display "I_OPL_RegisterSong: Failed to load MID.\n" (current-error-port)))
           (and file (> (midi-time-division file) 0) file))))

  ;; (oplmusic-unregister-song handle): I_OPL_UnRegisterSong. The handle is
  ;; garbage collected; this only exists for symmetry with C.
  (define (oplmusic-unregister-song handle) (void))

  ;; (oplmusic-play-song handle looping?): I_OPL_PlaySong. Starts handle's
  ;; tracks from the current sample position; looping? restarts the song
  ;; 5 ms after its last track ends. Also unpauses, as DMX does. Does nothing
  ;; if handle is #f. Like C it doesn't stop a song already playing: callers
  ;; stop first (s_sound does).
  (define (oplmusic-play-song file looping)
    (when (and music-initialized file)
      (let ([n (midi-num-tracks file)])
        ;; Allocate track data.
        (set! tracks (make-vector n #f))
        (set! num-tracks n)
        (set! running-tracks n)
        (set! song-looping (and looping #t))
        (set! ticks-per-beat (midi-time-division file))
        ;; Default is 120 bpm.
        (set! us-per-beat (* 500 1000))
        (set! start-music-volume current-music-volume)
        ;; Start each track playing: schedule its first event.
        (do ([i 0 (fx+ i 1)]) ((fx= i n))
          (vector-set! tracks i (midi-iterate-track file i))
          (schedule-track! (vector-ref tracks i)))
        (do ([i 0 (fx+ i 1)]) ((fx= i MIDI-CHANNELS-PER-TRACK))
          (init-channel! i))
        ;; If the music was previously paused, it needs to be unpaused;
        ;; playing a new song implies that we turn off pause. This matches
        ;; vanilla behavior of the DMX library, and some of the higher-level
        ;; code in s_sound.c relies on this.
        (opl-set-paused #f))))

  ;; (oplmusic-pause-song): I_OPL_PauseSong. Callbacks stop, and the main
  ;; instrument voices (not percussion) are keyed off, as vanilla does.
  (define (oplmusic-pause-song)
    (when music-initialized
      (opl-set-paused #t)
      (do ([i 0 (fx+ i 1)]) ((fx= i num-opl-voices))
        (when (and (not (fx= (fxvector-ref v-channel i) -1))
                   (fx< (fxvector-ref v-instr i) percussion-instrs))
          (voice-key-off! i)))))

  ;; (oplmusic-resume-song): I_OPL_ResumeSong.
  (define (oplmusic-resume-song)
    (when music-initialized
      (opl-set-paused #f)))

  ;; (oplmusic-stop-song): I_OPL_StopSong. Clears the callbacks and frees
  ;; all voices; the chip keeps running (released notes decay).
  (define (oplmusic-stop-song)
    (when music-initialized
      ;; Stop all playback.
      (opl-clear-callbacks)
      ;; Free all voices.
      (do ([i 0 (fx+ i 1)]) ((fx= i MIDI-CHANNELS-PER-TRACK))
        (all-notes-off! i))
      ;; Free all track data.
      (set! tracks (vector))
      (set! num-tracks 0)))

  ;; (oplmusic-playing?): I_OPL_MusicIsPlaying.
  (define (oplmusic-playing?)
    (and music-initialized (> num-tracks 0)))

  ;; (oplmusic-generate! bv offset nframes): render the next nframes
  ;; stereo s16le frames (4 bytes each) into bv from byte offset, advancing
  ;; the song: opl_sdl.c's mixing callback for a buffer of nframes. Before
  ;; oplmusic-init the frames are silence.
  (define (oplmusic-generate! bv offset nframes)
    (unless (and (bytevector? bv) (fixnum? offset) (fixnum? nframes)
                 (>= offset 0) (>= nframes 0)
                 (<= (+ offset (* 4 nframes)) (bytevector-length bv)))
      (assertion-violation 'oplmusic-generate! "frames don't fit in the bytevector" offset nframes))
    (if music-initialized
        (opl-mix! bv offset nframes)
        (bytevector-fill-range! bv offset (* 4 nframes))))

  (define (bytevector-fill-range! bv start n)
    (do ([i 0 (fx+ i 1)]) ((fx= i n)) (bytevector-u8-set! bv (fx+ start i) 0))))
