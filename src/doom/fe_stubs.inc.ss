;;; Stand-ins for the front end, milestone 7: st_stuff, hu_stuff, m_menu,
;;; wi_stuff, f_finale and f_wipe. Included by d_main.sls. None of these
;;; affect the playsim except the intermission and finale, which are
;;; logged as unported and skipped.

;; hu_stuff
(define (hu-init) (void))
(define (hu-start) (void))
(define (hu-ticker) (void))
(define (hu-drawer) (void))
(define (hu-erase) (void))
(define (hu-responder ev) #f)
(define (hu-dequeue-chat-char) 0)

;; st_stuff: the status bar background only.
(define (st-init) (void))
(define (st-start) (void))
(define (st-ticker) (void))
(define (st-responder ev) #f)
(define (st-drawer fullscreen refresh)
  (unless fullscreen
    (v-draw-patch 0 (fx- SCREENHEIGHT 32) 0 (w-cache-lump-name "STBAR"))))

;; m_menu. Until the menu exists, a key in the title loop starts a new
;; game on E1M1 (skill 3), and ESC quits.
(define inhelpscreens #f)
(define (m-init) (void))
(define (m-ticker) (void))
(define (m-drawer) (void))
(define (m-responder ev)
  (if (and (fx= (event-type ev) ev_keydown) (fx= (event-data1 ev) KEY_ESCAPE))
      (i-quit)
      #f))
(define (m-start-control-panel)
  (unless (fx= gamestate GS_LEVEL)
    (g-defered-init-new sk_medium 1 1)))

;; wi_stuff: no intermission yet; go straight on to the next level.
(define (wi-start wbstartstruct)
  (unported 'WI_Start)
  (g-world-done))
(define (wi-ticker) (void))
(define (wi-drawer) (void))

;; f_finale: no finale yet; back to the title loop.
(define (f-start-finale)
  (unported 'F_StartFinale)
  (set! gameaction ga_nothing)
  (d-start-title))
(define (f-responder ev) #f)
(define (f-ticker) (void))
(define (f-drawer) (void))
