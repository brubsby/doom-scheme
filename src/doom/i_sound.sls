;;; i_sound: the system interface for sound, as linuxdoom builds it (with
;;; SNDSERV): sound effects go to the sound server (sndserv), which here
;;; runs in-process, and its mixed buffers go to an SDL audio device.
;;;
;;; Under SNDSERV the game knows nothing about what the server plays:
;;; I_StartSound returns the sfx id as the handle, and I_SoundIsPlaying is
;;; "gametic < handle", so after the first ~100 tics every s_sound channel
;;; is freed the next frame. linuxdoom has no music (its music calls are
;;; dummies).
(library (doom i_sound)
  (export i-init-sound i-shutdown-sound i-close-sound i-update-sound i-submit-sound
          i-set-channels i-get-sfx-lump-num i-start-sound i-stop-sound
          i-sound-is-playing i-update-sound-params
          i-init-music i-shutdown-music i-set-music-volume i-pause-song i-resume-song
          i-register-song i-play-song i-stop-song i-unregister-song
          sound-hook)
  (import (chezscheme) (doom global) (doom doomstat) (doom sounds) (doom w_wad)
          (doom m_argv) (doom i_sdl) (doom i_video) (doom i_system) (doom sndserv))

  ;; Called with each buffer the server mixes (a reused bytevector of
  ;; SAMPLECOUNT s16le stereo frames), before it is played: the tests hash
  ;; these against the oracle's server.
  (define-global sound-hook #f)

  ;; The SDL device (0: none, so the server runs in lockstep, one buffer
  ;; per frame, as in the oracle).
  (define device 0)
  ;; linuxdoom's sound volumes are 0-15 out of the mixer's 0-127 (its
  ;; S_Init and M_SfxVol have the "*8" commented out), so a mix peaks at
  ;; about an eighth of full scale. The device gets the mix amplified by
  ;; this, clamped: an output level, like the OSS mixer setting, after the
  ;; hashed mix.
  (define output-gain 4)
  (define outbuf (make-bytevector (* SAMPLECOUNT 4) 0))
  ;; Keep about two buffers queued (about 93 ms), as linuxdoom's two OSS
  ;; fragments of 2048 bytes did.
  (define queue-target (* 2 SAMPLECOUNT 4))

  (define (i-init-sound)
    (if (fx> (m-check-parm "-nosound") 0)
        (printf "I_InitSound: sound disabled (-nosound)\n")
        (begin
          ;; start sound process
          (sndserv-init)
          (unless headless
            (if (< (sdl-init-sub-system SDL_INIT_AUDIO) 0)
                (printf "I_InitSound: no audio (~a)\n" (sdl-get-error))
                (begin
                  (set! device (sdl-open-audio-device SPEED 2 SAMPLECOUNT))
                  (if (eqv? device 0)
                      (printf "I_InitSound: could not open audio device (~a)\n" (sdl-get-error))
                      (begin
                        (sdl-pause-audio-device device 0)
                        (set! i-wait-hook i-submit-sound)))))))))

  ;; For I_Error, which exits without I_ShutdownSound: the server just
  ;; loses its pipe (no "q", so nothing more is played out).
  (define (i-close-sound)
    (unless (eqv? device 0)
      (set! i-wait-hook #f)
      (sdl-close-audio-device device)
      (set! device 0)))

  ;; I_ShutdownSound: send the server "q": it plays out what's playing.
  ;; The device then gets up to a few seconds to drain.
  (define (i-shutdown-sound)
    (if (eqv? device 0)
        (sndserv-quit (lambda (buf) (when sound-hook (sound-hook buf))))
        (begin
          (sndserv-quit play)
          (let loop ([n 0])
            (when (and (fx< n 300) (> (sdl-get-queued-audio-size device) 0))
              (sdl-delay 10)
              (loop (fx+ n 1))))
          (set! i-wait-hook #f)
          (sdl-close-audio-device device)
          (set! device 0))))

  ;; Under SNDSERV the game mixes nothing (D_DoomLoop skips this).
  (define (i-update-sound) (void))

  ;; I_SubmitSound: linuxdoom (SNDSERV) writes its unused, all-zero
  ;; mixbuffer to fd 0 here. Here it runs the server: one loop iteration in
  ;; lockstep without a device, otherwise as many as keep the device's
  ;; queue full (the original server was paced by its blocking writes).
  (define (i-submit-sound)
    (when (sndserv-active?)
      (if (eqv? device 0)
          (sndserv-loop (lambda (buf) (when sound-hook (sound-hook buf))))
          (let loop ()
            (when (< (sdl-get-queued-audio-size device) queue-target)
              (sndserv-loop play)
              (loop))))))

  (define (play buf)
    (when sound-hook (sound-hook buf))
    (do ([i 0 (fx+ i 2)]) ((fx= i (bytevector-length buf)))
      (bytevector-s16-set! outbuf i
                           (fxmax -32768 (fxmin 32767 (fx* output-gain (bytevector-s16-ref buf i (endianness little)))))
                           (endianness little)))
    (sdl-queue-audio device outbuf))

  ;; I_SetChannels: sets up the in-process mixer's tables, unused under
  ;; SNDSERV.
  (define (i-set-channels) (void))

  (define (i-get-sfx-lump-num sfx)
    (w-get-num-for-name (string-append "ds" (sfxinfo-name sfx))))

  ;; I_StartSound: send the server "p<id><pitch><vol><sep>"; the handle is
  ;; the id.
  (define (i-start-sound id vol sep pitch priority)
    (sndserv-command id pitch vol sep)
    id)

  (define (i-stop-sound handle) (void))

  (define (i-sound-is-playing handle)
    ;; Ouch.
    (fx< gametic handle))

  (define (i-update-sound-params handle vol sep pitch) (void))

  ;; MUSIC API. Still no music done. Remains. Dummies.
  (define (i-init-music) (void))
  (define (i-shutdown-music) (void))
  (define (i-set-music-volume volume) (void))
  (define (i-pause-song handle) (void))
  (define (i-resume-song handle) (void))
  (define (i-register-song data) 1)
  (define (i-play-song handle looping) (void))
  (define (i-stop-song handle) (void))
  (define (i-unregister-song handle) (void)))
