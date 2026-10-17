;;; i_sound: the system interface for sound, as linuxdoom builds it (with
;;; SNDSERV): sound effects go to the sound server (sndserv), which here
;;; runs in-process, and its mixed buffers go to an SDL audio device.
;;;
;;; Under SNDSERV the game knows nothing about what the server plays:
;;; I_StartSound returns the sfx id as the handle, and I_SoundIsPlaying is
;;; "gametic < handle", so after the first ~100 tics every s_sound channel
;;; is freed the next frame.
;;;
;;; linuxdoom has no music (its music calls are dummies, and the DOS
;;; version's went to DMX). Music here is Chocolate Doom's OPL player
;;; (i_oplmusic), on its own SDL device at 44100 Hz, kept full the same way.
;;; -nosound turns off both, -nomusic the music.
(library (doom i_sound)
  (export i-init-sound i-shutdown-sound i-close-sound i-update-sound i-submit-sound
          i-set-channels i-get-sfx-lump-num i-start-sound i-stop-sound
          i-sound-is-playing i-update-sound-params
          i-init-music i-shutdown-music i-set-music-volume i-pause-song i-resume-song
          i-register-song i-play-song i-stop-song i-unregister-song
          sound-hook)
  (import (chezscheme) (doom global) (doom doomstat) (doom sounds) (doom w_wad)
          (doom m_argv) (doom i_sdl) (doom i_video) (doom i_system) (doom sndserv) (doom i_oplmusic))

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

  ;; The music device, and its buffer: about 93 ms queued, in chunks of
  ;; 1024 frames.
  (define MUSICRATE 44100)
  (define MUSICCHUNK 1024)
  (define music-device 0)
  (define musicbuf (make-bytevector (* MUSICCHUNK 4) 0))
  (define music-queue-target (* 4 MUSICCHUNK 4))

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
                      (sdl-pause-audio-device device 0))
                  (unless (or (fx> (m-check-parm "-nomusic") 0)
                              (not (oplmusic-init MUSICRATE)))
                    (set! music-device (sdl-open-audio-device MUSICRATE 2 MUSICCHUNK))
                    (if (eqv? music-device 0)
                        (printf "I_InitMusic: could not open audio device (~a)\n" (sdl-get-error))
                        (sdl-pause-audio-device music-device 0)))
                  (unless (and (eqv? device 0) (eqv? music-device 0))
                    (set! i-wait-hook i-submit-sound))))))))

  (define (close-music)
    (unless (eqv? music-device 0)
      (sdl-close-audio-device music-device)
      (set! music-device 0)))

  ;; For I_Error, which exits without I_ShutdownSound: the server just
  ;; loses its pipe (no "q", so nothing more is played out).
  (define (i-close-sound)
    (close-music)
    (set! i-wait-hook #f)
    (unless (eqv? device 0)
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
          (set! device 0)))
    (close-music))

  ;; Under SNDSERV the game mixes nothing (D_DoomLoop skips this).
  (define (i-update-sound) (void))

  ;; I_SubmitSound: linuxdoom (SNDSERV) writes its unused, all-zero
  ;; mixbuffer to fd 0 here. Here it runs the server: one loop iteration in
  ;; lockstep without a device, otherwise as many as keep the device's
  ;; queue full (the original server was paced by its blocking writes).
  (define (i-submit-sound)
    (submit-music)
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

  ;; MUSIC API: Chocolate Doom's OPL player. (Without a music device the
  ;; calls still go to the player, which then just isn't run.)
  (define (submit-music)
    (unless (eqv? music-device 0)
      (let loop ()
        (when (< (sdl-get-queued-audio-size music-device) music-queue-target)
          (oplmusic-generate! musicbuf 0 MUSICCHUNK)
          (sdl-queue-audio music-device musicbuf)
          (loop)))))

  (define (i-init-music) (void))        ; (done by I_InitSound)
  (define (i-shutdown-music) (close-music))
  ;; Volumes are 0-15 (linuxdoom's S_SetMusicVolume passes the menu's
  ;; value); the player's are 0-127, as Chocolate's S_SetMusicVolume(v*8).
  (define (i-set-music-volume volume)
    (oplmusic-set-volume (fxmin 127 (fx* volume 8))))
  (define (i-pause-song handle) (oplmusic-pause-song))
  (define (i-resume-song handle) (oplmusic-resume-song))
  (define (i-register-song data) (oplmusic-register-song data))
  (define (i-play-song handle looping) (when handle (oplmusic-play-song handle looping)))
  (define (i-stop-song handle) (oplmusic-stop-song))
  (define (i-unregister-song handle) (when handle (oplmusic-unregister-song handle))))
