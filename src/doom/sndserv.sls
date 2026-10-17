;;; sndserv: linuxdoom's sound server (sndserv/soundsrv.c and wadread.c's
;;; getsfx), which mixes the sound effects.
;;;
;;; linuxdoom 1.10 ships with SNDSERV defined: the game pipes
;;; "p<id><pitch><vol><sep>" commands to this separate process, which loops
;;; reading whatever commands are waiting, then mixing one 512-sample buffer
;;; (11025 Hz, 16-bit stereo) and writing it to /dev/dsp. That blocking write
;;; paces the loop. Here the server runs in-process: i_sound queues the
;;; commands and calls sndserv-loop once per server iteration (one per frame
;;; in lockstep with the oracle's patched server, or as many as the audio
;;; device needs). Every mixed buffer goes to the output procedure.
(library (doom sndserv)
  (export sndserv-init sndserv-command sndserv-loop sndserv-quit sndserv-active?
          SAMPLECOUNT SPEED)
  (import (chezscheme) (doom sounds) (doom w_wad))

  (define SAMPLECOUNT 512)
  (define MIXBUFFERSIZE (* SAMPLECOUNT 2 2))    ; bytes
  (define SPEED 11025)

  ;; an internal time keeper
  (define mytime 0)

  ;; Sound data: the padded samples (8 header bytes first, as in the lump),
  ;; and the lengths of all sound effects.
  (define sfxdata (make-vector NUMSFX #f))
  (define lengths (make-fxvector NUMSFX 0))

  ;; mixing buffer
  (define mixbuffer (make-bytevector MIXBUFFERSIZE 0))

  ;; The channels. A channel's data pointer is the sound's bytevector plus
  ;; an index into it; #f is C's null pointer (a free channel).
  (define channels (make-vector 8 #f))
  (define channelpos (make-fxvector 8 0))
  ;; the channel step amount
  (define channelstep (make-fxvector 8 0))
  ;; 0.16 bit remainder of last step
  (define channelstepremainder (make-fxvector 8 0))
  ;; the channel data end pointers
  (define channelsend (make-fxvector 8 0))
  ;; time that the channel started playing
  (define channelstart (make-fxvector 8 0))
  ;; the channel handles
  (define channelhandles (make-fxvector 8 0))
  ;; the channel left and right volume lookups (offsets into vol_lookup)
  (define channelleftvol_lookup (make-fxvector 8 0))
  (define channelrightvol_lookup (make-fxvector 8 0))
  ;; sfx id of the playing sound effect
  (define channelids (make-fxvector 8 0))

  (define steptable (make-fxvector 256 0))
  (define vol_lookup (make-fxvector (* 128 256) 0))

  (define (derror msg)
    (errorf 'sndserv "~a" msg))

  ;; mix: one buffer from the eight channels.
  (define (mix)
    (do ([i 0 (fx+ i 4)]) ((fx= i MIXBUFFERSIZE))
      (let loop ([chan 0] [dl 0] [dr 0])
        (if (fx< chan 8)
            (let ([data (vector-ref channels chan)])
              (if data
                  (let* ([pos (fxvector-ref channelpos chan)]
                         [sample (bytevector-u8-ref data pos)]
                         [dl (fx+ dl (fxvector-ref vol_lookup (fx+ (fxvector-ref channelleftvol_lookup chan) sample)))]
                         [dr (fx+ dr (fxvector-ref vol_lookup (fx+ (fxvector-ref channelrightvol_lookup chan) sample)))]
                         [rem (fx+ (fxvector-ref channelstepremainder chan) (fxvector-ref channelstep chan))]
                         [pos (fx+ pos (fxsrl rem 16))])
                    (fxvector-set! channelstepremainder chan (fxand rem #xffff))
                    (fxvector-set! channelpos chan pos)
                    (when (fx>= pos (fxvector-ref channelsend chan))
                      (vector-set! channels chan #f))
                    (loop (fx+ chan 1) dl dr))
                  (loop (fx+ chan 1) dl dr)))
            (begin
              (bytevector-s16-set! mixbuffer i (fxmax -32768 (fxmin 32767 dl)) (endianness little))
              (bytevector-s16-set! mixbuffer (fx+ i 2) (fxmax -32768 (fxmin 32767 dr)) (endianness little)))))))

  ;; getsfx (wadread.c): the sound's lump, padded with silence (128) to a
  ;; whole number of mix buffers. Returns (values data length); the data's
  ;; samples start at index 8. The server reads the IWAD itself with its
  ;; own reader; we use the game's lumps. A missing lump (DOOM II sounds in
  ;; doom1.wad) leaves the C's size uninitialized; here it's empty.
  (define (getsfx sfxname)
    (let* ([lump (w-check-num-for-name (string-append "ds" sfxname))]
           [sfx (if (fx< lump 0) (make-bytevector 8 128) (w-read-lump lump))]
           [size (bytevector-length sfx)]
           ;; pad the sound effect out to the mixing buffer size
           [paddedsize (fx* (fxquotient (fx+ (fx- size 8) (fx- SAMPLECOUNT 1)) SAMPLECOUNT)
                            SAMPLECOUNT)]
           [padded (make-bytevector (fx+ paddedsize 8) 128)])
      (bytevector-copy! sfx 0 padded 0 (fxmin size (fx+ paddedsize 8)))
      (values padded paddedsize)))

  ;; grabdata: load every sound. A link (the chaingun, to the pistol) shares
  ;; the data, but its length is lengths[(link - S_sfx)/sizeof(sfxinfo_t)]:
  ;; the pointer difference is already an index, so dividing by the
  ;; struct's size again (36 bytes on the 32-bit original) gives
  ;; lengths[0], which is 0. The chaingun plays one sample.
  (define (grabdata)
    (do ([i 1 (fx+ i 1)]) ((fx= i NUMSFX))
      (let* ([sfx (vector-ref S_sfx i)] [link (sfxinfo-link sfx)])
        (if (not link)
            (let-values ([(data len) (getsfx (sfxinfo-name sfx))])
              (vector-set! sfxdata i data)
              (fxvector-set! lengths i len))
            (begin
              (vector-set! sfxdata i (vector-ref sfxdata link))
              (fxvector-set! lengths i (fxvector-ref lengths (fxquotient link 36))))))))

  (define handlenums 0)                 ; static unsigned short in addsfx

  (define (addsfx sfxid volume step seperation)
    (let ([oldest mytime] [oldestnum 0])
      ;; play these sound effects only one at a time
      (when (memv sfxid (list sfx_sawup sfx_sawidl sfx_sawful sfx_sawhit sfx_stnmov sfx_pistol))
        (let loop ([i 0])
          (when (fx< i 8)
            (if (and (vector-ref channels i) (fx= (fxvector-ref channelids i) sfxid))
                (vector-set! channels i #f)
                (loop (fx+ i 1))))))
      (let* ([i (let loop ([i 0])
                  (if (and (fx< i 8) (vector-ref channels i))
                      (begin
                        (when (fx< (fxvector-ref channelstart i) oldest)
                          (set! oldestnum i)
                          (set! oldest (fxvector-ref channelstart i)))
                        (loop (fx+ i 1)))
                      i))]
             [slot (if (fx= i 8) oldestnum i)])
        (vector-set! channels slot (vector-ref sfxdata sfxid))
        (fxvector-set! channelpos slot 8)
        (fxvector-set! channelsend slot (fx+ 8 (fxvector-ref lengths sfxid)))
        (when (fx= handlenums 0) (set! handlenums 100))
        (let ([rc handlenums])
          (set! handlenums (fxand (fx+ handlenums 1) #xffff))
          (fxvector-set! channelhandles slot rc)
          (fxvector-set! channelstep slot step)
          (fxvector-set! channelstepremainder slot 0)
          (fxvector-set! channelstart slot mytime)
          ;; (range: 1 - 256)
          (let* ([seperation (fx+ seperation 1)]
                 ;; (x^2 seperation)
                 [leftvol (fx- volume (fxquotient (fx* volume (fx* seperation seperation)) (* 256 256)))]
                 [seperation (fx- seperation 257)]
                 [rightvol (fx- volume (fxquotient (fx* volume (fx* seperation seperation)) (* 256 256)))])
            ;; sanity check
            (when (or (fx< rightvol 0) (fx> rightvol 127)) (derror "rightvol out of bounds"))
            (when (or (fx< leftvol 0) (fx> leftvol 127)) (derror "leftvol out of bounds"))
            ;; get the proper lookup table piece for this volume level
            (fxvector-set! channelleftvol_lookup slot (fx* leftvol 256))
            (fxvector-set! channelrightvol_lookup slot (fx* rightvol 256))
            (fxvector-set! channelids slot sfxid))
          rc))))

  (define (initdata)
    (vector-fill! channels #f)
    (do ([i -128 (fx+ i 1)]) ((fx= i 128))
      (fxvector-set! steptable (fx+ i 128)
                     (flonum->fixnum (fl* (expt 2.0 (fl/ (fixnum->flonum i) 64.0)) 65536.0))))
    ;; generates volume lookup tables which also turn the unsigned samples
    ;; into signed samples
    (do ([i 0 (fx+ i 1)]) ((fx= i 128))
      (do ([j 0 (fx+ j 1)]) ((fx= j 256))
        (fxvector-set! vol_lookup (fx+ (fx* i 256) j)
                       (fxquotient (fx* i (fx* (fx- j 128) 256)) 127)))))

  ;; The pipe: commands the game has sent and the server hasn't read.
  (define pending '())                  ; newest first
  (define active #f)
  (define (sndserv-active?) active)

  ;; main's startup: get sound data, init any data.
  (define (sndserv-init)
    (grabdata)
    (initdata)
    (set! mytime 0)
    (set! pending '())
    (set! active #t))

  ;; The game's I_StartSound: fprintf(sndserver, "p%2.2x%2.2x%2.2x%2.2x\n",
  ;; id, pitch, vol, sep). Each value is two hex digits on the wire.
  (define (sndserv-command id pitch vol sep)
    (when active
      (set! pending (cons (vector (fxand id #xff) (fxand pitch #xff) (fxand vol #xff) (fxand sep #xff))
                          pending))))

  ;; One iteration of main's loop: read the waiting commands, then mix a
  ;; buffer and hand it (a bytevector of SAMPLECOUNT s16le stereo frames,
  ;; reused by the next call) to output.
  (define (sndserv-loop output)
    (set! mytime (fx+ mytime 1))
    (for-each (lambda (c)
                ;; p<snd#><step><vol><sep>
                (addsfx (vector-ref c 0) (vector-ref c 2)
                        (fxvector-ref steptable (vector-ref c 1)) (vector-ref c 3)))
              (reverse pending))
    (set! pending '())
    (mix)
    (output mixbuffer))

  ;; The game's "q" (I_ShutdownSound): the server stops reading commands
  ;; and mixes until every channel has finished, then exits.
  (define (sndserv-quit output)
    (when active
      (let loop ()
        (sndserv-loop output)
        (when (let any ([i 0]) (and (fx< i 8) (or (vector-ref channels i) (any (fx+ i 1)))))
          (loop)))
      (set! active #f))))
