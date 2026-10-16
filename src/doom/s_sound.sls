;;; s_sound: the sound system's interface to the game.
;;; TODO(milestone 8): everything. Until then these do nothing. Note that
;;; S_StartSound calls M_Random for some sounds, so rndindex (not the
;;; playsim's prndindex) will differ from the oracle until it's ported.
(library (doom s_sound)
  (export s-init s-start s-start-sound s-stop-sound s-start-music s-change-music
          s-stop-music s-pause-sound s-resume-sound s-update-sounds)
  (import (chezscheme))

  (define (s-init sfxvolume musicvolume) (void))
  ;; Per level startup: kills playing sounds, starts the level's music.
  (define (s-start) (void))
  ;; origin is an mobj, a sector's degenmobj, or #f.
  (define (s-start-sound origin sfx-id) (void))
  (define (s-stop-sound origin) (void))
  (define (s-start-music music-id) (void))
  (define (s-change-music music-id looping) (void))
  (define (s-stop-music) (void))
  (define (s-pause-sound) (void))
  (define (s-resume-sound) (void))
  (define (s-update-sounds listener) (void)))
