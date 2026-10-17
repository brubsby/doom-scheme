;;; midifile: reading of MIDI files, from Chocolate Doom's src/midifile.c.
;;; The parser reads a bytevector where C reads a FILE, with the same checks
;;; in the same order, so a file C rejects is rejected here too. Like C it
;;; ignores the track chunk lengths and reads events up to each track's end
;;; of track meta event.
;;;
;;; A track is a vector of events. An iterator is a mutable record of the
;;; track and a position; midi-next-event returns the event and advances.
(library (doom midifile)
  (export midi-load midi-num-tracks midi-time-division
          midi-iterate-track midi-restart-iterator! midi-delta-time midi-next-event
          midi-event? midi-event-delta midi-event-type midi-event-channel
          midi-event-param1 midi-event-param2 midi-event-meta-type midi-event-data
          MIDI-EVENT-NOTE-OFF MIDI-EVENT-NOTE-ON MIDI-EVENT-AFTERTOUCH
          MIDI-EVENT-CONTROLLER MIDI-EVENT-PROGRAM-CHANGE MIDI-EVENT-CHAN-AFTERTOUCH
          MIDI-EVENT-PITCH-BEND MIDI-EVENT-SYSEX MIDI-EVENT-SYSEX-SPLIT MIDI-EVENT-META
          MIDI-META-END-OF-TRACK MIDI-META-SET-TEMPO
          MIDI-CONTROLLER-VOLUME-MSB MIDI-CONTROLLER-PAN MIDI-CONTROLLER-ALL-NOTES-OFF
          MIDI-CHANNELS-PER-TRACK)
  (import (chezscheme))

  (define MIDI-CHANNELS-PER-TRACK 16)

  ;; midi_event_type_t
  (define MIDI-EVENT-NOTE-OFF #x80)
  (define MIDI-EVENT-NOTE-ON #x90)
  (define MIDI-EVENT-AFTERTOUCH #xa0)
  (define MIDI-EVENT-CONTROLLER #xb0)
  (define MIDI-EVENT-PROGRAM-CHANGE #xc0)
  (define MIDI-EVENT-CHAN-AFTERTOUCH #xd0)
  (define MIDI-EVENT-PITCH-BEND #xe0)
  (define MIDI-EVENT-SYSEX #xf0)
  (define MIDI-EVENT-SYSEX-SPLIT #xf7)
  (define MIDI-EVENT-META #xff)

  ;; The controllers and meta events i_oplmusic handles.
  (define MIDI-CONTROLLER-VOLUME-MSB #x07)
  (define MIDI-CONTROLLER-PAN #x0a)
  (define MIDI-CONTROLLER-ALL-NOTES-OFF #x7b)
  (define MIDI-META-END-OF-TRACK #x2f)
  (define MIDI-META-SET-TEMPO #x51)

  ;; midi_event_t. type is the event type with the channel masked off;
  ;; channel, param1 and param2 belong to channel events (param2 is 0 for
  ;; one-parameter events, where C leaves it uninitialized and nothing reads
  ;; it); meta-type and data (a bytevector) to meta and SysEx events.
  (define-record-type (midi-event make-midi-event midi-event?)
    (fields (immutable delta midi-event-delta)
            (immutable type midi-event-type)
            (immutable channel midi-event-channel)
            (immutable param1 midi-event-param1)
            (immutable param2 midi-event-param2)
            (immutable meta-type midi-event-meta-type)
            (immutable data midi-event-data)))

  ;; midi_file_t: the header's time division and the tracks.
  (define-record-type (midi-file make-midi-file midi-file?)
    (fields (immutable time-division midi-file-time-division)
            (immutable tracks midi-file-tracks)))

  ;; midi_track_iter_t
  (define-record-type (midi-iter make-midi-iter midi-iter?)
    (fields (immutable track midi-iter-track)
            (mutable position midi-iter-position midi-iter-position-set!)))

  ;; (midi-load bv): MIDI_LoadFile over the bytes in bv; a midi file or #f.
  (define (midi-load bv)
    (define len (bytevector-length bv))
    (define pos 0)
    (call/cc
     (lambda (return)
       (define (fail) (return #f))
       ;; Read a single byte.
       (define (read-byte!)
         (unless (fx< pos len) (fail))
         (let ([b (bytevector-u8-ref bv pos)]) (set! pos (fx+ pos 1)) b))
       ;; Read a variable-length value: at most four bytes.
       (define (read-variable-length!)
         (let loop ([i 0] [result 0])
           (if (fx= i 4)
               (fail)
               (let* ([b (read-byte!)]
                      [result (fxlogor (fxsll result 7) (fxlogand b #x7f))])
                 (if (fx= 0 (fxlogand b #x80)) result (loop (fx+ i 1) result))))))
       ;; Read a byte sequence.
       (define (read-byte-sequence! n)
         (unless (<= n (fx- len pos)) (fail))
         (let ([r (make-bytevector n)])
           (bytevector-copy! bv pos r 0 n)
           (set! pos (fx+ pos n))
           r))
       ;; A chunk header: 4-byte id, then a big-endian length.
       (define (chunk-header! id)
         (unless (fx<= 8 (fx- len pos)) (fail))
         (unless (bytevector=? (string->utf8 id) (bytevector-slice bv pos 4)) (fail))
         (let ([size (bytevector-u32-ref bv (fx+ pos 4) (endianness big))])
           (set! pos (fx+ pos 8))
           size))
       ;; Read one event; last is the running status.
       (define (read-event! last)
         (let* ([delta (read-variable-length!)]
                [b (read-byte!)]
                [event-type (if (fx= 0 (fxlogand b #x80))
                                (begin (set! pos (fx- pos 1)) last)
                                b)]
                [last (if (fx= 0 (fxlogand b #x80)) last b)])
           (define (channel-event two-param?)
             (let* ([p1 (read-byte!)] [p2 (if two-param? (read-byte!) 0)])
               (make-midi-event delta (fxlogand event-type #xf0) (fxlogand event-type #x0f)
                                p1 p2 0 #f)))
           (values
            (case (fxlogand event-type #xf0)
              [(#x80 #x90 #xa0 #xb0 #xe0) (channel-event #t)]
              [(#xc0 #xd0) (channel-event #f)]
              [else
               (case event-type
                 [(#xf0 #xf7)
                  (let* ([n (read-variable-length!)] [data (read-byte-sequence! n)])
                    (make-midi-event delta event-type 0 0 0 0 data))]
                 [(#xff)
                  (let* ([type (read-byte!)] [n (read-variable-length!)]
                         [data (read-byte-sequence! n)])
                    (make-midi-event delta event-type 0 0 0 type data))]
                 [else (fail)])])
            last)))
       ;; Read a track: events up to and including the end of track.
       (define (read-track!)
         (chunk-header! "MTrk")
         (let loop ([last 0] [acc '()])
           (let-values ([(e last) (read-event! last)])
             (if (and (fx= (midi-event-type e) MIDI-EVENT-META)
                      (fx= (midi-event-meta-type e) MIDI-META-END-OF-TRACK))
                 (list->vector (reverse (cons e acc)))
                 (loop last (cons e acc))))))
       ;; Read and check the header chunk.
       (unless (fx<= 14 len) (fail))
       (unless (fx= 6 (chunk-header! "MThd")) (fail))
       (let ([format-type (bytevector-u16-ref bv 8 (endianness big))]
             [num-tracks (bytevector-u16-ref bv 10 (endianness big))]
             [division (bytevector-s16-ref bv 12 (endianness big))])
         (set! pos 14)
         (unless (and (or (fx= format-type 0) (fx= format-type 1)) (fx>= num-tracks 1))
           (fail))
         (let ([tracks (make-vector num-tracks)])
           (do ([i 0 (fx+ i 1)]) ((fx= i num-tracks))
             (vector-set! tracks i (read-track!)))
           (make-midi-file division tracks))))))

  (define (bytevector-slice bv start n)
    (let ([r (make-bytevector n)]) (bytevector-copy! bv start r 0 n) r))

  ;; Get the number of tracks in a MIDI file.
  (define (midi-num-tracks file) (vector-length (midi-file-tracks file)))

  ;; MIDI_GetFileTimeDivision: ticks per beat. A negative division is SMPTE
  ;; time, frames per second times ticks per frame (C's int division).
  (define (midi-time-division file)
    (let ([result (midi-file-time-division file)])
      (if (fx< result 0)
          (fx* (fx- (fxquotient result 256)) (fxlogand result #xff))
          result)))

  ;; Start iterating over the events in a track.
  (define (midi-iterate-track file track)
    (make-midi-iter (vector-ref (midi-file-tracks file) track) 0))

  ;; Reset an iterator to the beginning of a track.
  (define (midi-restart-iterator! iter) (midi-iter-position-set! iter 0))

  ;; Get the time until the next MIDI event in a track (0 at the end).
  (define (midi-delta-time iter)
    (let ([events (midi-iter-track iter)] [i (midi-iter-position iter)])
      (if (fx< i (vector-length events))
          (midi-event-delta (vector-ref events i))
          0)))

  ;; Get the next MIDI event and advance, or #f at the end of the track.
  (define (midi-next-event iter)
    (let ([events (midi-iter-track iter)] [i (midi-iter-position iter)])
      (and (fx< i (vector-length events))
           (begin (midi-iter-position-set! iter (fx+ i 1))
                  (vector-ref events i))))))
