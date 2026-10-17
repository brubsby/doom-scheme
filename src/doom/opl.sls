;;; opl: Chocolate Doom's OPL library as the music player uses it with the
;;; software emulator: opl/opl.c (register writes, chip detection, register
;;; init), opl/opl_queue.c (the callback heap) and opl/opl_sdl.c (the
;;; sample-driven clock and mixing callback), over (doom opl3).
;;;
;;; There is no audio thread: opl-mix! is opl_sdl.c's OPL_Mix_Callback, which
;;; the caller runs whenever it wants more samples. It generates up to the
;;; next queued callback, advances the clock by the samples generated, runs
;;; every callback now due, and repeats until the buffer is full. The clock
;;; is opl_sdl.c's: microseconds, advanced by nsamples * 10^6 / rate rounded
;;; down per step, so where a buffer is split matters, as it does in C.
;;; Nothing reads the wall clock.
;;;
;;; Times are C's uint64_t, kept as exact integers reduced mod 2^64 wherever
;;; C would wrap (they stay fixnums in practice). OPL_AdjustCallbacks does
;;; its arithmetic in single precision, which is emulated by rounding each
;;; result to a float.
;;;
;;; Locking (OPL_Lock, the queue mutexes) has nothing to do on one thread.
;;; The OPL timers are not emulated: their registers (2-4) never reach the
;;; chip, and only OPL_Detect's status reads would see them, which always
;;; find an OPL3 under opl_sdl.
(library (doom opl)
  (export opl-init opl-write-register opl-init-registers
          opl-set-callback opl-clear-callbacks opl-set-paused opl-adjust-callbacks
          opl-mix!)
  (import (chezscheme) (doom opl3))

  (define OPL-SECOND 1000000)
  (define OPL-MS 1000)
  (define U64 (expt 2 64))
  (define (u64 x) (modulo x U64))

  (define OPL-NUM-OPERATORS 21)
  (define OPL-REG-WAVEFORM-ENABLE #x01)
  (define OPL-REG-TIMER1 #x02)
  (define OPL-REG-TIMER2 #x03)
  (define OPL-REG-TIMER-CTRL #x04)
  (define OPL-REG-FM-MODE #x08)
  (define OPL-REG-NEW #x105)
  (define OPL-REGS-LEVEL #x40)
  (define OPL-REGS-ATTACK #x60)
  (define OPL-REGS-WAVEFORM #xe0)

  ;;
  ;; opl_queue.c: a binary min heap of (time, callback, data).
  ;;

  (define MAX-OPL-QUEUE 64)
  (define q-time (make-vector MAX-OPL-QUEUE 0))
  (define q-callback (make-vector MAX-OPL-QUEUE #f))
  (define q-data (make-vector MAX-OPL-QUEUE #f))
  (define q-num 0)

  (define (queue-set! i time callback data)
    (vector-set! q-time i time)
    (vector-set! q-callback i callback)
    (vector-set! q-data i data))

  (define (queue-move! to from)
    (queue-set! to (vector-ref q-time from) (vector-ref q-callback from)
                (vector-ref q-data from)))

  (define (queue-push! callback data time)
    (if (fx>= q-num MAX-OPL-QUEUE)
        (display "OPL_Queue_Push: Exceeded maximum callbacks\n" (current-error-port))
        (let ([entry-id q-num])
          (set! q-num (fx+ q-num 1))
          (let shift ([entry-id entry-id])
            ;; Shift existing entries down in the heap.
            (if (fx> entry-id 0)
                (let ([parent-id (fxquotient (fx- entry-id 1) 2)])
                  (if (>= time (vector-ref q-time parent-id))
                      (queue-set! entry-id time callback data)
                      (begin (queue-move! entry-id parent-id)
                             (shift parent-id))))
                (queue-set! entry-id time callback data))))))

  ;; Pop the first entry; returns its callback and data.
  (define (queue-pop!)
    (let ([callback (vector-ref q-callback 0)] [data (vector-ref q-data 0)])
      ;; Decrease the heap size, and percolate the last entry down from the
      ;; top.
      (set! q-num (fx- q-num 1))
      (let* ([last q-num]
             [time (vector-ref q-time last)]
             [last-callback (vector-ref q-callback last)]
             [last-data (vector-ref q-data last)])
        (let loop ([i 0])
          (let* ([child1 (fx+ (fx* i 2) 1)]
                 [child2 (fx+ (fx* i 2) 2)]
                 [next-i
                  (cond
                    [(and (fx< child1 q-num) (< (vector-ref q-time child1) time))
                     ;; Left child is less than entry. Use the minimum of
                     ;; left and right children.
                     (if (and (fx< child2 q-num)
                              (< (vector-ref q-time child2) (vector-ref q-time child1)))
                         child2
                         child1)]
                    [(and (fx< child2 q-num) (< (vector-ref q-time child2) time))
                     child2]
                    [else #f])])
            (if next-i
                (begin (queue-move! i next-i) (loop next-i))
                (queue-set! i time last-callback last-data)))))
      (values callback data)))

  (define (queue-peek) (if (fx> q-num 0) (vector-ref q-time 0) 0))

  ;; (uint64_t) of a float, as gcc compiles it for x86-64: truncation for
  ;; values in range, and the results of cvttss2si's "indefinite" value
  ;; otherwise (NaN and -inf give 2^63, +inf gives 0).
  (define (float->u64 x)
    (cond
      [(nan? x) (expt 2 63)]
      [(< x (expt 2.0 63))
       (if (>= x (- (expt 2.0 63))) (u64 (exact (truncate x))) (expt 2 63))]
      [(< (- x (expt 2.0 63)) (expt 2.0 63))
       (+ (exact (truncate (- x (expt 2.0 63)))) (expt 2 63))]
      [else 0]))

  (define f32-cell (make-bytevector 4))
  (define (f32 x)
    (bytevector-ieee-single-native-set! f32-cell 0 x)
    (bytevector-ieee-single-native-ref f32-cell 0))

  ;; OPL_Queue_AdjustCallbacks: rescale every entry's distance from time by
  ;; 1/factor, in single precision. The heap is not reordered (nor in C).
  (define (queue-adjust! time factor)
    (do ([i 0 (fx+ i 1)]) ((fx= i q-num))
      (let* ([d (u64 (- (vector-ref q-time i) time))]
             [offset (if (>= d (expt 2 63)) (- d U64) d)])
        (vector-set! q-time i
          (u64 (+ time (float->u64 (f32 (/ (f32 (inexact offset)) factor)))))))))

  ;;
  ;; opl_sdl.c
  ;;

  (define chip (opl3-make))
  (define mixing-freq 49716)
  ;; Current time (current_time), in us since startup.
  (define clock 0)
  ;; If true, playback is currently paused.
  (define paused #f)
  ;; Time offset (in us) due to the fact that callbacks were previously
  ;; paused.
  (define pause-offset 0)
  ;; The driver's copy of the OPL3 mode bit (register 0x105).
  (define opl3mode 0)

  ;; Advance time by the specified number of samples, invoking any
  ;; callback functions as appropriate.
  (define (advance-time! nsamples)
    (let ([us (quotient (* nsamples OPL-SECOND) mixing-freq)])
      (set! clock (u64 (+ clock us)))
      (when paused (set! pause-offset (u64 (+ pause-offset us))))
      ;; Are there callbacks to invoke now? Keep invoking them until there
      ;; are no more left.
      (let loop ()
        (when (and (fx> q-num 0)
                   (>= clock (u64 (+ (queue-peek) pause-offset))))
          (let-values ([(callback data) (queue-pop!)])
            (callback data))
          (loop)))))

  ;; (opl-mix! bv offset nframes): OPL_Mix_Callback. Fill nframes stereo
  ;; s16le frames at byte offset with the chip's output (overwriting what
  ;; is there: SDL_mixer's add onto a silent stream), running callbacks at
  ;; their sample positions.
  (define (opl-mix! bv offset nframes)
    (let loop ([filled 0])
      (when (fx< filled nframes)
        (let* ([left (fx- nframes filled)]
               [nsamples
                (if (or paused (fx= q-num 0))
                    left
                    ;; The time until the next callback, in samples, rounded
                    ;; up.
                    (let* ([next (u64 (+ (queue-peek) pause-offset))]
                           [n (u64 (* (u64 (- next clock)) mixing-freq))]
                           [n (quotient (u64 (+ n (- OPL-SECOND 1))) OPL-SECOND)])
                      (if (> n left) left n)))])
          (opl3-generate-stream! chip bv (fx+ offset (fx* 4 filled)) nsamples)
          (advance-time! nsamples)
          (loop (fx+ filled nsamples))))))

  ;; OPL_SDL_SetCallback
  (define (opl-set-callback us callback data)
    (queue-push! callback data (u64 (+ (- clock pause-offset) us))))

  (define (opl-clear-callbacks) (set! q-num 0))

  (define (opl-set-paused p) (set! paused p))

  ;; OPL_AdjustCallbacks: factor is a single-precision float.
  (define (opl-adjust-callbacks factor) (queue-adjust! clock factor))

  ;; WriteRegister: the timer registers stay in the driver; everything else
  ;; goes through the chip's write buffer.
  (define (opl-write-register reg value)
    (cond
      [(or (fx= reg OPL-REG-TIMER1) (fx= reg OPL-REG-TIMER2) (fx= reg OPL-REG-TIMER-CTRL))
       (void)]
      [else
       (when (fx= reg OPL-REG-NEW) (set! opl3mode (fxlogand value 1)))
       (opl3-write-reg-buffered! chip reg value)]))

  ;;
  ;; opl.c
  ;;

  ;; OPL_Delay: queue a callback and let the "audio thread" run until it
  ;; fires. Here that is the mixing callback over exactly the frames up to
  ;; the callback, discarded.
  (define (delay! us)
    (let ([finished #f])
      (opl-set-callback us (lambda (_) (set! finished #t)) #f)
      (let ([n (let* ([next (u64 (+ (queue-peek) pause-offset))]
                      [n (u64 (* (u64 (- next clock)) mixing-freq))])
                 (quotient (u64 (+ n (- OPL-SECOND 1))) OPL-SECOND))])
        (opl-mix! (make-bytevector (* 4 n)) 0 n))
      (unless finished (error 'opl-init "OPL_Delay callback did not fire"))))

  ;; OPL_Detect, minus the timer writes and status reads that never reach
  ;; the chip: what remains is the 1 ms delay.
  (define (detect!)
    (delay! (* 1 OPL-MS)))

  ;; (opl-init rate): OPL_Init with the SDL driver mixing at rate Hz:
  ;; OPL_SDL_Init (clock, queue and chip reset), then InitDriver's two chip
  ;; detections.
  (define (opl-init rate)
    (set! mixing-freq rate)
    (set! paused #f)
    (set! pause-offset 0)
    (set! q-num 0)
    (set! clock 0)
    (opl3-reset! chip rate)
    (set! opl3mode 0)
    (detect!)
    (detect!))

  ;; Write an OPL register value. OPL_WriteRegister's port reads, which pace
  ;; a real chip, have no effect here.
  (define (write-register reg value) (opl-write-register reg value))

  ;; OPL_InitRegisters. These loops write to registers that don't exist,
  ;; and the <= is intentional: this is what Doom does.
  (define (opl-init-registers opl3?)
    (define (init-bank bank)
      ;; Initialize level registers
      (do ([r OPL-REGS-LEVEL (fx+ r 1)]) ((fx> r (fx+ OPL-REGS-LEVEL OPL-NUM-OPERATORS)))
        (write-register (fxlogor r bank) #x3f))
      ;; Initialize other registers
      (do ([r OPL-REGS-ATTACK (fx+ r 1)]) ((fx> r (fx+ OPL-REGS-WAVEFORM OPL-NUM-OPERATORS)))
        (write-register (fxlogor r bank) #x00))
      ;; More registers ...
      (do ([r 1 (fx+ r 1)]) ((fx= r OPL-REGS-LEVEL))
        (write-register (fxlogor r bank) #x00)))
    (init-bank 0)
    ;; Re-initialize the low registers: reset both timers and enable
    ;; interrupts.
    (write-register OPL-REG-TIMER-CTRL #x60)
    (write-register OPL-REG-TIMER-CTRL #x80)
    ;; "Allow FM chips to control the waveform of each operator":
    (write-register OPL-REG-WAVEFORM-ENABLE #x20)
    (when opl3?
      (write-register OPL-REG-NEW #x01)
      (init-bank #x100))
    ;; Keyboard split point on (?)
    (write-register OPL-REG-FM-MODE #x40)
    (when opl3?
      (write-register OPL-REG-NEW #x01))))
