;;; p_lights.c: light thinkers and light-changing line specials. Included
;;; by p_local.sls.

;; T_FireFlicker
(define (t-fire-flicker flick)
  (fireflicker-count-set! flick (fx- (fireflicker-count flick) 1))
  (when (fx= (fireflicker-count flick) 0)
    (let ([amount (fx* (fxand (p-random) 3) 16)]
          [sector (fireflicker-sector flick)])
      (if (fx< (fx- (sector-lightlevel sector) amount) (fireflicker-minlight flick))
          (sector-lightlevel-set! sector (fireflicker-minlight flick))
          (sector-lightlevel-set! sector (fx- (fireflicker-maxlight flick) amount)))
      (fireflicker-count-set! flick 4))))

;; P_SpawnFireFlicker
(define (p-spawn-fire-flicker sector)
  ;; Note that we are resetting sector attributes. Nothing special about it
  ;; during gameplay.
  (sector-special-set! sector 0)
  (let ([flick (make-fireflicker)])
    (p-add-thinker flick)
    (thinker-func-set! flick t-fire-flicker)
    (fireflicker-sector-set! flick sector)
    (fireflicker-maxlight-set! flick (sector-lightlevel sector))
    (fireflicker-minlight-set! flick (fx+ (p-find-min-surrounding-light sector (sector-lightlevel sector)) 16))
    (fireflicker-count-set! flick 4)))

;; BROKEN LIGHT FLASHING

;; T_LightFlash: do flashing lights.
(define (t-light-flash flash)
  (lightflash-count-set! flash (fx- (lightflash-count flash) 1))
  (when (fx= (lightflash-count flash) 0)
    (let ([sector (lightflash-sector flash)])
      (if (fx= (sector-lightlevel sector) (lightflash-maxlight flash))
          (begin
            (sector-lightlevel-set! sector (lightflash-minlight flash))
            (lightflash-count-set! flash (fx+ (fxand (p-random) (lightflash-mintime flash)) 1)))
          (begin
            (sector-lightlevel-set! sector (lightflash-maxlight flash))
            (lightflash-count-set! flash (fx+ (fxand (p-random) (lightflash-maxtime flash)) 1)))))))

;; P_SpawnLightFlash: after the map has been loaded, scan each sector for
;; specials that spawn thinkers.
(define (p-spawn-light-flash sector)
  ;; nothing special about it during gameplay
  (sector-special-set! sector 0)
  (let ([flash (make-lightflash)])
    (p-add-thinker flash)
    (thinker-func-set! flash t-light-flash)
    (lightflash-sector-set! flash sector)
    (lightflash-maxlight-set! flash (sector-lightlevel sector))
    (lightflash-minlight-set! flash (p-find-min-surrounding-light sector (sector-lightlevel sector)))
    (lightflash-maxtime-set! flash 64)
    (lightflash-mintime-set! flash 7)
    (lightflash-count-set! flash (fx+ (fxand (p-random) (lightflash-maxtime flash)) 1))))

;; STROBE LIGHT FLASHING

;; T_StrobeFlash
(define (t-strobe-flash flash)
  (strobe-count-set! flash (fx- (strobe-count flash) 1))
  (when (fx= (strobe-count flash) 0)
    (let ([sector (strobe-sector flash)])
      (if (fx= (sector-lightlevel sector) (strobe-minlight flash))
          (begin
            (sector-lightlevel-set! sector (strobe-maxlight flash))
            (strobe-count-set! flash (strobe-brighttime flash)))
          (begin
            (sector-lightlevel-set! sector (strobe-minlight flash))
            (strobe-count-set! flash (strobe-darktime flash)))))))

;; P_SpawnStrobeFlash: after the map has been loaded, scan each sector for
;; specials that spawn thinkers.
(define (p-spawn-strobe-flash sector fast-or-slow in-sync)
  (let ([flash (make-strobe)])
    (p-add-thinker flash)
    (strobe-sector-set! flash sector)
    (strobe-darktime-set! flash fast-or-slow)
    (strobe-brighttime-set! flash STROBEBRIGHT)
    (thinker-func-set! flash t-strobe-flash)
    (strobe-maxlight-set! flash (sector-lightlevel sector))
    (strobe-minlight-set! flash (p-find-min-surrounding-light sector (sector-lightlevel sector)))
    (when (fx= (strobe-minlight flash) (strobe-maxlight flash))
      (strobe-minlight-set! flash 0))
    ;; nothing special about it during gameplay
    (sector-special-set! sector 0)
    (strobe-count-set! flash (if (fx= in-sync 0) (fx+ (fxand (p-random) 7) 1) 1))))

;; Start strobing lights (usually from a trigger)
(define (ev-start-light-strobing line)
  (for-each-tagged-sector
   line
   (lambda (sec secnum)
     (unless (sector-specialdata sec)
       (p-spawn-strobe-flash sec SLOWDARK 0)))))

;; TURN LINE'S TAG LIGHTS OFF
(define (ev-turn-tag-lights-off line)
  (vector-for-each
   (lambda (sector)
     (when (fx= (sector-tag sector) (line-tag line))
       (sector-lightlevel-set! sector
                               (fold-neighbors sector (sector-lightlevel sector)
                                               (lambda (tsec min) (fxmin (sector-lightlevel tsec) min))))))
   sectors))

;; TURN LINE'S TAG LIGHTS ON. bright = 0 means to search for the highest
;; light level surrounding sector. (Once found, that level is used for the
;; following tagged sectors too.)
(define (ev-light-turn-on line bright)
  (let loop ([i 0] [bright bright])
    (when (fx< i numsectors)
      (let ([sector (vector-ref sectors i)])
        (if (fx= (sector-tag sector) (line-tag line))
            (let ([bright (if (fx= bright 0)
                              (fold-neighbors sector bright
                                              (lambda (temp b) (fxmax (sector-lightlevel temp) b)))
                              bright)])
              (sector-lightlevel-set! sector bright)
              (loop (fx+ i 1) bright))
            (loop (fx+ i 1) bright))))))

;; Spawn glowing light
(define (t-glow g)
  (let ([sector (glow-sector g)])
    (case (glow-direction g)
      [(-1)
       ;; DOWN
       (sector-lightlevel-set! sector (fx- (sector-lightlevel sector) GLOWSPEED))
       (when (fx<= (sector-lightlevel sector) (glow-minlight g))
         (sector-lightlevel-set! sector (fx+ (sector-lightlevel sector) GLOWSPEED))
         (glow-direction-set! g 1))]
      [(1)
       ;; UP
       (sector-lightlevel-set! sector (fx+ (sector-lightlevel sector) GLOWSPEED))
       (when (fx>= (sector-lightlevel sector) (glow-maxlight g))
         (sector-lightlevel-set! sector (fx- (sector-lightlevel sector) GLOWSPEED))
         (glow-direction-set! g -1))])))

(define (p-spawn-glowing-light sector)
  (let ([g (make-glow)])
    (p-add-thinker g)
    (glow-sector-set! g sector)
    (glow-minlight-set! g (p-find-min-surrounding-light sector (sector-lightlevel sector)))
    (glow-maxlight-set! g (sector-lightlevel sector))
    (thinker-func-set! g t-glow)
    (glow-direction-set! g -1)
    (sector-special-set! sector 0)))
