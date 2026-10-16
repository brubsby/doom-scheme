;;; m_misc.c: defaults and file helpers. Included by d_main.sls.
;;; TODO(milestone 9): reading and writing the config file (~/.doomrc),
;;; M_ScreenShot.


;; M_LoadDefaults: the built-in defaults (the key, mouse and joystick
;; bindings start at theirs in g_game). No config file is read yet.
(define (m-load-defaults)
  (set! mouseSensitivity 5)
  (set! snd_SfxVolume 8)
  (set! snd_MusicVolume 8)
  (set! showMessages 1)
  (set! key_right KEY_RIGHTARROW)
  (set! key_left KEY_LEFTARROW)
  (set! key_up KEY_UPARROW)
  (set! key_down KEY_DOWNARROW)
  (set! key_strafeleft (char->integer #\,))
  (set! key_straferight (char->integer #\.))
  (set! key_fire KEY_RCTRL)
  (set! key_use (char->integer #\space))
  (set! key_strafe KEY_RALT)
  (set! key_speed KEY_RSHIFT)
  (set! usemouse 1)
  (set! mousebfire 0)
  (set! mousebstrafe 1)
  (set! mousebforward 2)
  (set! usejoystick 0)
  (set! joybfire 0)
  (set! joybstrafe 1)
  (set! joybuse 3)
  (set! joybspeed 2)
  (set! screenblocks 9)
  (set! detailLevel 0)
  (set! numChannels 3)
  (set! usegamma 0))

;; M_WriteFile: the first `length` bytes of bv.
(define (m-write-file name bv length)
  (call-with-port (open-file-output-port name (file-options no-fail))
    (lambda (p) (put-bytevector p bv 0 length)))
  #t)

;; M_ScreenShot. TODO(milestone 9).
(define (m-screen-shot)
  (unported 'M_ScreenShot))
