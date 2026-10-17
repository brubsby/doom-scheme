;;; s_sound: the sound system's interface to the game: channels, priority,
;;; distance and stereo, and music selection.
;;;
;;; The game logic is ported in full, because it touches the simulation's
;;; neighbours: S_StartSound calls M_Random (pitch variation) only for
;;; sounds that are audible and get a channel, and the status bar face and
;;; the screen wipe read the same M_Random stream. Nothing is played yet
;;; (milestone 8): the i_sound calls are the oracle's null layer's, where
;;; I_SoundIsPlaying is always false, so S_UpdateSounds frees every channel
;;; each frame, exactly as in the oracle.
(library (doom s_sound)
  (export s-init s-start s-start-sound s-start-sound-at-volume s-stop-sound s-start-music
          s-change-music s-stop-music s-pause-sound s-resume-sound s-update-sounds
          s-set-music-volume s-set-sfx-volume snd_SfxVolume snd_MusicVolume numChannels)
  (import (chezscheme) (doom global) (doom doomdef) (doom doomstat) (doom d_player)
          (doom sounds) (doom m_fixed) (doom m_random) (doom tables) (doom w_wad)
          (doom i_system) (doom r_defs) (doom p_mobj.h) (doom r_local))

  (define S_CLIPPING_DIST (* 1200 #x10000)) ; when to clip out sounds
  ;; Distance to origin when sounds should be maxed out.
  (define S_CLOSE_DIST (* 160 #x10000))
  (define S_ATTENUATOR (fxsra (fx- S_CLIPPING_DIST S_CLOSE_DIST) FRACBITS))
  (define NORM_PITCH 128)
  (define NORM_PRIORITY 64)
  (define NORM_SEP 128)
  (define S_STEREO_SWING (* 96 #x10000))

  ;; Adjustable by menu (the defaults are m_misc's).
  (define-global snd_SfxVolume 15)
  (define-global snd_MusicVolume 15)
  ;; number of channels available (set by the defaults code in m_misc)
  (define-global numChannels 3)

  ;; channel_t: sfxinfo (#f if the channel is free), origin, handle.
  (define-record-type channel
    (fields (mutable sfxinfo) (mutable origin) (mutable handle)))
  (define (make-channels n)
    (let ([v (make-vector n)])
      (do ([i 0 (fx+ i 1)]) ((fx= i n) v)
        (vector-set! v i (make-channel #f #f 0)))))
  ;; (allocated by S_Init; these serve tests that run the playsim alone)
  (define channels (make-channels numChannels))

  (define mus_paused #f)                ; whether songs are mus_paused
  (define mus_playing #f)               ; music currently being played (an index)
  ;; music lump numbers, looked up on first use
  (define music-lumpnum (make-vector NUMMUSIC #f))

  ;; The i_sound layer, as the oracle's null one (milestone 8 plays sound).
  (define next-handle 0)
  (define (i-start-sound id vol sep pitch priority)
    (let ([h next-handle]) (set! next-handle (fx+ h 1)) h))
  (define (i-sound-is-playing handle) #f)
  (define (i-get-sfx-lump-num sfx)
    (w-get-num-for-name (string-append "ds" (sfxinfo-name sfx))))

  ;; S_Init: initializes sound stuff, including volume. Sets channels, SFX
  ;; and music volume, allocates channel buffer, sets S_sfx lookup.
  (define (s-init sfx-volume music-volume)
    (s-set-sfx-volume sfx-volume)
    (s-set-music-volume music-volume)
    ;; Allocating the internal channels for mixing; free all channels for
    ;; use.
    (set! channels (make-channels numChannels))
    ;; no sounds are playing, and they are not mus_paused
    (set! mus_paused #f)
    ;; Note that sounds have not been cached (yet).
    (do ([i 1 (fx+ i 1)]) ((fx= i NUMSFX))
      (let ([sfx (vector-ref S_sfx i)])
        (sfxinfo-lumpnum-set! sfx -1)
        (sfxinfo-usefulness-set! sfx -1))))

  (define spmus
    ;; Song - Who? - Where?
    (vector mus_e3m4    ; American     e4m1
            mus_e3m2    ; Romero       e4m2
            mus_e3m3    ; Shawn        e4m3
            mus_e1m5    ; American     e4m4
            mus_e2m7    ; Tim          e4m5
            mus_e2m4    ; Romero       e4m6
            mus_e2m6    ; J.Anderson   e4m7 CHIRON.WAD
            mus_e2m5    ; Shawn        e4m8
            mus_e1m9))  ; Tim          e4m9

  ;; S_Start: per level startup code. Kills playing sounds at start of
  ;; level, determines music if any, changes music.
  (define (s-start)
    ;; kill all playing sounds at start of level (trust me - a good idea)
    (do ([cnum 0 (fx+ cnum 1)]) ((fx= cnum numChannels))
      (when (channel-sfxinfo (vector-ref channels cnum))
        (s-stop-channel cnum)))
    ;; start new music for the level
    (set! mus_paused #f)
    (s-change-music (cond [(fx= gamemode commercial) (fx+ mus_runnin gamemap -1)]
                          [(fx< gameepisode 4) (fx+ mus_e1m1 (fx* (fx- gameepisode 1) 9) gamemap -1)]
                          [else (vector-ref spmus (fx- gamemap 1))])
                    #t))

  (define (clamp-pitch p) (fxmax 0 (fxmin 255 p)))

  ;; S_StartSoundAtVolume. origin is an mobj, a sector's sound origin (a
  ;; degenmobj), or #f.
  (define (s-start-sound-at-volume origin sfx-id volume)
    ;; check for bogus sound #
    (when (or (fx< sfx-id 1) (fx> sfx-id NUMSFX))
      (i-error "Bad sfx #: ~a" sfx-id))
    (let* ([sfx (vector-ref S_sfx sfx-id)]
           [pitch (if (sfxinfo-link sfx) (sfxinfo-pitch sfx) NORM_PITCH)]
           [priority (if (sfxinfo-link sfx) (sfxinfo-priority sfx) NORM_PRIORITY)]
           [volume (if (sfxinfo-link sfx) (fx+ volume (sfxinfo-volume sfx)) volume)]
           [sep NORM_SEP])
      (call/1cc
       (lambda (return)
         ;; initialize sound parameters
         (when (sfxinfo-link sfx)
           (when (fx< volume 1) (return (void)))
           (when (fx> volume snd_SfxVolume) (set! volume snd_SfxVolume)))
         ;; Check to see if it is audible, and if not, modify the params
         (let ([listener (player-mo (vector-ref players consoleplayer))])
           (when (and origin (not (eq? origin listener)))
             (let-values ([(rc v s p) (s-adjust-sound-params listener origin volume sep pitch)])
               (set! volume v) (set! sep s) (set! pitch p)
               (when (and (fx= (degenmobj-x origin) (degenmobj-x listener))
                          (fx= (degenmobj-y origin) (degenmobj-y listener)))
                 (set! sep NORM_SEP))
               (unless rc (return (void))))))
         ;; hacks to vary the sfx pitches
         (cond
           [(and (fx>= sfx-id sfx_sawup) (fx<= sfx-id sfx_sawhit))
            (set! pitch (clamp-pitch (fx+ pitch (fx- 8 (fxand (m-random) 15)))))]
           [(not (or (fx= sfx-id sfx_itemup) (fx= sfx-id sfx_tink)))
            (set! pitch (clamp-pitch (fx+ pitch (fx- 16 (fxand (m-random) 31)))))])
         ;; kill old sound
         (s-stop-sound origin)
         ;; try to find a channel
         (let ([cnum (s-get-channel origin sfx)])
           (when (fx< cnum 0) (return (void)))
           ;; get lumpnum if necessary
           (when (fx< (sfxinfo-lumpnum sfx) 0)
             (sfxinfo-lumpnum-set! sfx (i-get-sfx-lump-num sfx)))
           ;; increase the usefulness
           (let ([u (sfxinfo-usefulness sfx)])
             (sfxinfo-usefulness-set! sfx (if (fx< u 0) 1 (fx+ u 1))))
           ;; Assigns the handle to one of the channels in the mix/output
           ;; buffer.
           (channel-handle-set! (vector-ref channels cnum)
                                (i-start-sound sfx-id volume sep pitch priority)))))))

  (define (s-start-sound origin sfx-id)
    (s-start-sound-at-volume origin sfx-id snd_SfxVolume))

  (define (s-stop-sound origin)
    (let loop ([cnum 0])
      (when (fx< cnum numChannels)
        (let ([c (vector-ref channels cnum)])
          (if (and (channel-sfxinfo c) (eq? (channel-origin c) origin))
              (s-stop-channel cnum)
              (loop (fx+ cnum 1)))))))

  ;; Stop and resume music, during game PAUSE.
  (define (s-pause-sound)
    (when (and mus_playing (not mus_paused))
      (set! mus_paused #t)))

  (define (s-resume-sound)
    (when (and mus_playing mus_paused)
      (set! mus_paused #f)))

  ;; S_UpdateSounds: updates music & sounds.
  (define (s-update-sounds listener)
    (do ([cnum 0 (fx+ cnum 1)]) ((fx= cnum numChannels))
      (let* ([c (vector-ref channels cnum)]
             [sfx (channel-sfxinfo c)])
        (when sfx
          (if (i-sound-is-playing (channel-handle c))
              ;; initialize parameters
              (let ([volume (if (sfxinfo-link sfx) (fx+ snd_SfxVolume (sfxinfo-volume sfx)) snd_SfxVolume)]
                    [pitch (if (sfxinfo-link sfx) (sfxinfo-pitch sfx) NORM_PITCH)])
                (if (fx< volume 1)
                    (s-stop-channel cnum)
                    ;; check non-local sounds for distance clipping or
                    ;; modify their params
                    (let ([volume (fxmin volume snd_SfxVolume)])
                      (when (and (channel-origin c) (not (eq? listener (channel-origin c))))
                        (let-values ([(audible v s p)
                                      (s-adjust-sound-params listener (channel-origin c) volume NORM_SEP pitch)])
                          (unless audible
                            (s-stop-channel cnum)))))))
              ;; if channel is allocated but sound has stopped, free it
              (s-stop-channel cnum))))))

  (define (s-set-music-volume volume)
    (when (or (fx< volume 0) (fx> volume 127))
      (i-error "Attempt to set music volume at ~a" volume))
    (set! snd_MusicVolume volume))

  (define (s-set-sfx-volume volume)
    (when (or (fx< volume 0) (fx> volume 127))
      (i-error "Attempt to set sfx volume at ~a" volume))
    (set! snd_SfxVolume volume))

  ;; Starts some music with the music id found in sounds.h.
  (define (s-start-music m-id)
    (s-change-music m-id #f))

  (define (s-change-music musicnum looping)
    (when (or (fx<= musicnum mus_None) (fx>= musicnum NUMMUSIC))
      (i-error "Bad music number ~a" musicnum))
    (unless (eqv? mus_playing musicnum)
      ;; shutdown old music
      (s-stop-music)
      ;; get lumpnum if neccessary
      (unless (vector-ref music-lumpnum musicnum)
        (vector-set! music-lumpnum musicnum
                     (w-get-num-for-name (string-append "d_" (vector-ref S_music musicnum)))))
      ;; (load, register and play it: milestone 8)
      (set! mus_playing musicnum)))

  (define (s-stop-music)
    (when mus_playing
      (set! mus_paused #f)
      (set! mus_playing #f)))

  (define (s-stop-channel cnum)
    (let* ([c (vector-ref channels cnum)]
           [sfx (channel-sfxinfo c)])
      (when sfx
        ;; (stop the sound playing: nothing is playing yet)
        ;; degrade usefulness of sound data
        (sfxinfo-usefulness-set! sfx (fx- (sfxinfo-usefulness sfx) 1))
        (channel-sfxinfo-set! c #f))))

  ;; S_AdjustSoundParams: changes volume, stereo-separation, and pitch
  ;; variables from the norm of a sound effect to be played. Returns
  ;; (values audible? vol sep pitch); when not audible, C returns early,
  ;; leaving the parameters as they were.
  (define (s-adjust-sound-params listener source vol sep pitch)
    ;; calculate the distance to sound origin and clip it if necessary
    (let* ([adx (fxabs (int32 (fx- (degenmobj-x listener) (degenmobj-x source))))]
           [ady (fxabs (int32 (fx- (degenmobj-y listener) (degenmobj-y source))))]
           ;; From _GG1_ p.428. Appox. eucledian distance fast.
           [approx-dist (fx- (fx+ adx ady) (fxsra (fxmin adx ady) 1))])
      (if (and (not (fx= gamemap 8)) (fx> approx-dist S_CLIPPING_DIST))
          (values #f vol sep pitch)
          ;; angle of source to listener
          (let* ([angle (r-point-to-angle2 (degenmobj-x listener) (degenmobj-y listener)
                                           (degenmobj-x source) (degenmobj-y source))]
                 [la (mobj-angle listener)]
                 [angle (if (> angle la)
                            (- angle la)
                            (fxand (+ angle (- #xffffffff la)) #xffffffff))]
                 [angle (fxsrl angle ANGLETOFINESHIFT)]
                 ;; stereo separation
                 [sep (fx- 128 (fxsra (fixed-mul S_STEREO_SWING (fxvector-ref finesine angle)) FRACBITS))]
                 ;; volume calculation
                 [vol (cond
                        [(fx< approx-dist S_CLOSE_DIST) snd_SfxVolume]
                        [(fx= gamemap 8)
                         (let ([d (fxmin approx-dist S_CLIPPING_DIST)])
                           (fx+ 15 (fxquotient (fx* (fx- snd_SfxVolume 15)
                                                    (fxsra (fx- S_CLIPPING_DIST d) FRACBITS))
                                               S_ATTENUATOR)))]
                        [else
                         ;; distance effect
                         (fxquotient (fx* snd_SfxVolume (fxsra (fx- S_CLIPPING_DIST approx-dist) FRACBITS))
                                     S_ATTENUATOR)])])
            (values (fx> vol 0) vol sep pitch)))))

  ;; S_getChannel: if none available, return -1. Otherwise channel #.
  (define (s-get-channel origin sfxinfo)
    ;; Find an open channel
    (let ([cnum (let loop ([cnum 0])
                  (cond
                    [(fx= cnum numChannels) cnum]
                    [(not (channel-sfxinfo (vector-ref channels cnum))) cnum]
                    [(and origin (eq? (channel-origin (vector-ref channels cnum)) origin))
                     (s-stop-channel cnum)
                     cnum]
                    [else (loop (fx+ cnum 1))]))])
      (let ([cnum (if (fx= cnum numChannels)
                      ;; None available: look for lower priority
                      (let loop ([cnum 0])
                        (cond
                          [(fx= cnum numChannels) -1] ; No lower priority. Sorry, Charlie.
                          [(fx>= (sfxinfo-priority (channel-sfxinfo (vector-ref channels cnum)))
                                 (sfxinfo-priority sfxinfo))
                           ;; Otherwise, kick out lower priority.
                           (s-stop-channel cnum)
                           cnum]
                          [else (loop (fx+ cnum 1))]))
                      cnum)])
        (unless (fx< cnum 0)
          ;; channel is decided to be cnum.
          (let ([c (vector-ref channels cnum)])
            (channel-sfxinfo-set! c sfxinfo)
            (channel-origin-set! c origin)))
        cnum))))
