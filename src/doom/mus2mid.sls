;;; mus2mid: MUS to MIDI conversion, from Chocolate Doom's src/mus2mid.c
;;; (Ben Ryves 2006), which i_oplmusic uses to turn a D_* lump into a type 0
;;; MIDI file. The MEMFILE streams of src/memio.c become a read position in
;;; the input bytevector and a growable output buffer.
;;;
;;; As in C, two pieces of state outlive a conversion: the cached velocity
;;; of each MIDI channel (a key press that carries no velocity reuses the last
;;; one, even from an earlier song) and the queued delay (only nonzero after a
;;; conversion failed part way). Bytes from a MUS lump are unsigned; unsigned
;;; int arithmetic is masked to 32 bits where it can overflow.
(library (doom mus2mid)
  (export mus2mid)
  (import (chezscheme))

  (define NUM-CHANNELS 16)
  (define MIDI-PERCUSSION-CHAN 9)
  (define MUS-PERCUSSION-CHAN 15)

  ;; MUS event codes
  (define mus-releasekey #x00)
  (define mus-presskey #x10)
  (define mus-pitchwheel #x20)
  (define mus-systemevent #x30)
  (define mus-changecontroller #x40)
  (define mus-scoreend #x60)

  ;; MIDI event codes
  (define midi-releasekey #x80)
  (define midi-presskey #x90)
  (define midi-changecontroller #xb0)
  (define midi-changepatch #xc0)
  (define midi-pitchwheel #xe0)

  ;; Standard MIDI type 0 header + track header
  (define midiheader
    #vu8(#x4d #x54 #x68 #x64            ; Main header "MThd"
         #x00 #x00 #x00 #x06            ; Header size
         #x00 #x00                      ; MIDI type (0)
         #x00 #x01                      ; Number of tracks
         #x00 #x46                      ; Resolution
         #x4d #x54 #x72 #x6b            ; Start of track "MTrk"
         #x00 #x00 #x00 #x00))          ; Placeholder for track length

  ;; Cached channel velocities
  (define channelvelocities (make-bytevector NUM-CHANNELS 127))

  ;; Timestamps between sequences of MUS events
  (define queuedtime 0)

  ;; Counter for the length of the track
  (define tracksize 0)

  (define controller-map
    #vu8(#x00 #x20 #x01 #x07 #x0A #x0B #x5B #x5D
         #x40 #x43 #x78 #x7B #x7E #x7F #x79))

  (define channel-map (make-fxvector NUM-CHANNELS -1))

  (define (u32 x) (logand x #xffffffff))

  ;; The output stream: a buffer that doubles as it fills, the write
  ;; position and the length written so far.
  (define out (make-bytevector 1024))
  (define out-pos 0)
  (define out-len 0)

  (define (out-byte! b)
    (when (fx= out-pos (bytevector-length out))
      (let ([new (make-bytevector (fx* 2 (bytevector-length out)))])
        (bytevector-copy! out 0 new 0 out-pos)
        (set! out new)))
    (bytevector-u8-set! out out-pos (fxlogand b #xff))
    (set! out-pos (fx+ out-pos 1))
    (when (fx> out-pos out-len) (set! out-len out-pos)))

  ;; Write timestamp to a MIDI file.
  (define (write-time time)
    (let loop ([buffer (fxlogand time #x7f)] [time (fxsrl time 7)])
      (if (not (fx= time 0))
          (loop (u32 (logor (ash buffer 8) (fxlogor (fxlogand time #x7f) #x80)))
                (fxsrl time 7))
          (let write ([buffer buffer])
            (out-byte! (logand buffer #xff))
            (set! tracksize (u32 (+ tracksize 1)))
            (if (not (= 0 (logand buffer #x80)))
                (write (ash buffer -8))
                (set! queuedtime 0))))))

  ;; Write the end of track marker
  (define (write-end-track)
    (write-time queuedtime)
    (out-byte! #xff) (out-byte! #x2f) (out-byte! #x00)
    (set! tracksize (u32 (+ tracksize 3))))

  ;; Write a key press event
  (define (write-press-key channel key velocity)
    (write-time queuedtime)
    (out-byte! (fxlogor midi-presskey channel))
    (out-byte! (fxlogand key #x7f))
    (out-byte! (fxlogand velocity #x7f))
    (set! tracksize (u32 (+ tracksize 3))))

  ;; Write a key release event
  (define (write-release-key channel key)
    (write-time queuedtime)
    (out-byte! (fxlogor midi-releasekey channel))
    (out-byte! (fxlogand key #x7f))
    (out-byte! 0)
    (set! tracksize (u32 (+ tracksize 3))))

  ;; Write a pitch wheel/bend event
  (define (write-pitch-wheel channel wheel)
    (write-time queuedtime)
    (out-byte! (fxlogor midi-pitchwheel channel))
    (out-byte! (fxlogand wheel #x7f))
    (out-byte! (fxlogand (fxsra wheel 7) #x7f))
    (set! tracksize (u32 (+ tracksize 3))))

  ;; Write a patch change event
  (define (write-change-patch channel patch)
    (write-time queuedtime)
    (out-byte! (fxlogor midi-changepatch channel))
    (out-byte! (fxlogand patch #x7f))
    (set! tracksize (u32 (+ tracksize 2))))

  ;; Write a valued controller change event. MUS controller values should
  ;; be 7-bit; vanilla passes 8, and mus2mid.c clamps those to 0x7f.
  (define (write-change-controller-valued channel control value)
    (write-time queuedtime)
    (out-byte! (fxlogor midi-changecontroller channel))
    (out-byte! (fxlogand control #x7f))
    (out-byte! (if (fx= 0 (fxlogand value #x80)) value #x7f))
    (set! tracksize (u32 (+ tracksize 3))))

  ;; Write a valueless controller change event
  (define (write-change-controller-valueless channel control)
    (write-change-controller-valued channel control 0))

  ;; Allocate a free MIDI channel: the one after the highest allocated,
  ;; skipping the MIDI percussion channel.
  (define (allocate-midi-channel)
    (let* ([max (let loop ([i 0] [max -1])
                  (if (fx= i NUM-CHANNELS)
                      max
                      (loop (fx+ i 1) (fxmax max (fxvector-ref channel-map i)))))]
           [result (fx+ max 1)])
      (if (fx= result MIDI-PERCUSSION-CHAN) (fx+ result 1) result)))

  ;; Given a MUS channel number, get the MIDI channel number to use in the
  ;; outputted file.
  (define (get-midi-channel mus-channel)
    (cond
      [(fx= mus-channel MUS-PERCUSSION-CHAN) MIDI-PERCUSSION-CHAN]
      [else
       (when (fx= (fxvector-ref channel-map mus-channel) -1)
         (fxvector-set! channel-map mus-channel (allocate-midi-channel))
         ;; First time using the channel, send an "all notes off" event.
         ;; This fixes "The D_DDTBLU disease".
         (write-change-controller-valueless (fxvector-ref channel-map mus-channel) #x7b))
       (fxvector-ref channel-map mus-channel)]))

  ;; (mus2mid bv): the MIDI file for the MUS data in bv, as a fresh
  ;; bytevector, or #f where mus2mid.c fails. The header's id is not checked
  ;; (CHECK_MUS_HEADER is off).
  (define (mus2mid in)
    (define len (bytevector-length in))
    (define pos 0)
    ;; mem_fread of one byte: the byte, or #f at the end.
    (define (read-byte!)
      (and (fx< pos len)
           (let ([b (bytevector-u8-ref in pos)]) (set! pos (fx+ pos 1)) b)))
    (call/cc
     (lambda (return)
       (define (fail) (return #f))
       (do ([c 0 (fx+ c 1)]) ((fx= c NUM-CHANNELS)) (fxvector-set! channel-map c -1))
       ;; Grab the header: id, then five shorts, read as six mem_freads.
       (unless (fx>= len 14) (fail))
       (let ([scorestart (bytevector-u16-ref in 6 (endianness little))])
         ;; Seek to where the data is held
         (unless (fx< scorestart len) (fail))
         (set! pos scorestart))
       (set! out (make-bytevector 1024))
       (set! out-pos 0)
       (set! out-len 0)
       (do ([i 0 (fx+ i 1)]) ((fx= i (bytevector-length midiheader)))
         (out-byte! (bytevector-u8-ref midiheader i)))
       (set! tracksize 0)
       ;; Now, process the MUS file:
       (let loop ()
         ;; Handle a block of events. Returns #t at the score end.
         (let ([end?
                (let events ()
                  (let* ([eventdescriptor (or (read-byte!) (fail))]
                         [channel (get-midi-channel (fxlogand eventdescriptor #x0f))]
                         [event (fxlogand eventdescriptor #x70)]
                         [end?
                          (cond
                            [(fx= event mus-releasekey)
                             (write-release-key channel (or (read-byte!) (fail)))
                             #f]
                            [(fx= event mus-presskey)
                             (let ([key (or (read-byte!) (fail))])
                               (unless (fx= 0 (fxlogand key #x80))
                                 (bytevector-u8-set! channelvelocities channel
                                   (fxlogand (or (read-byte!) (fail)) #x7f)))
                               (write-press-key channel key
                                 (bytevector-u8-ref channelvelocities channel)))
                             #f]
                            [(fx= event mus-pitchwheel)
                             ;; A failed read only breaks out of the switch.
                             (let ([key (read-byte!)])
                               (when key (write-pitch-wheel channel (fx* key 64))))
                             #f]
                            [(fx= event mus-systemevent)
                             (let ([n (or (read-byte!) (fail))])
                               (when (or (fx< n 10) (fx> n 14)) (fail))
                               (write-change-controller-valueless
                                channel (bytevector-u8-ref controller-map n)))
                             #f]
                            [(fx= event mus-changecontroller)
                             (let* ([n (or (read-byte!) (fail))]
                                    [value (or (read-byte!) (fail))])
                               (cond
                                 [(fx= n 0) (write-change-patch channel value)]
                                 [(or (fx< n 1) (fx> n 9)) (fail)]
                                 [else (write-change-controller-valued
                                        channel (bytevector-u8-ref controller-map n) value)]))
                             #f]
                            [(fx= event mus-scoreend) #t]
                            [else (fail)])])
                    (cond [end? #t]
                          [(not (fx= 0 (fxlogand eventdescriptor #x80))) #f]
                          [else (events)])))])
           ;; Now we need to read the time code:
           (unless end?
             (let time ([timedelay 0])
               (let* ([working (or (read-byte!) (fail))]
                      [timedelay (u32 (+ (* timedelay 128) (fxlogand working #x7f)))])
                 (if (fx= 0 (fxlogand working #x80))
                     (set! queuedtime (u32 (+ queuedtime timedelay)))
                     (time timedelay))))
             (loop))))
       ;; End of track
       (write-end-track)
       ;; Write the track size into the stream
       (set! out-pos 18)
       (out-byte! (ash tracksize -24))
       (out-byte! (ash tracksize -16))
       (out-byte! (ash tracksize -8))
       (out-byte! tracksize)
       (let ([result (make-bytevector out-len)])
         (bytevector-copy! out 0 result 0 out-len)
         result)))))
