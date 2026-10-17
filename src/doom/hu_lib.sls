;;; hu_lib: the heads-up text widgets: a text line, a scrolling text
;;; window (the message line) and an input line (chat).
;;;
;;; A line's text is a bytevector of C chars; `on` is a thunk (C: a
;;; boolean*).
(library (doom hu_lib)
  (export HU_MAXLINES HU_MAXLINELENGTH
          hu-textline make-hu-textline hu-textline-x hu-textline-y hu-textline-len
          hu-textline-l hu-textline-needsupdate
          hulib-clear-text-line hulib-init-text-line hulib-add-char-to-text-line
          hulib-del-char-from-text-line hulib-draw-text-line hulib-erase-text-line
          hu-stext make-hu-stext hu-stext-l hu-stext-cl hulib-init-stext hulib-add-line-to-stext
          hulib-add-message-to-stext hulib-draw-stext hulib-erase-stext
          hu-itext make-hu-itext hu-itext-l hulib-init-itext hulib-del-char-from-itext
          hulib-erase-line-from-itext hulib-reset-itext hulib-add-prefix-to-itext
          hulib-key-in-itext hulib-draw-itext hulib-erase-itext)
  (import (chezscheme) (doom struct) (doom doomdef) (doom doomstat) (doom v_video)
          (doom r_local))

  ;; background and foreground screen numbers (different from other
  ;; modules)
  (define BG 1)
  (define FG 0)
  (define HU_MAXLINES 4)
  (define HU_MAXLINELENGTH 80)

  ;; hu_textline_t: left-justified position, font, start character, line of
  ;; text, current length, whether this line needs to be udpated.
  (define-struct hu-textline ()
    (x 0) (y 0) (f #f) (sc 0) (l (make-bytevector (fx+ HU_MAXLINELENGTH 1) 0)) (len 0)
    (needsupdate 0))
  ;; hu_stext_t: text lines to draw, height in lines, current line number,
  ;; on, last value of on.
  (define-struct hu-stext ()
    (l (let ([v (make-vector HU_MAXLINES)])
         (do ([i 0 (fx+ i 1)]) ((fx= i HU_MAXLINES) v)
           (vector-set! v i (make-hu-textline)))))
    (h 0) (cl 0) (on #f) (laston #f))
  ;; hu_itext_t: text line to input on, left margin past which I am not to
  ;; delete characters, on, last value of on.
  (define-struct hu-itext () (l (make-hu-textline)) (lm 0) (on #f) (laston #f))

  (define (hulib-clear-text-line t)
    (hu-textline-len-set! t 0)
    (bytevector-u8-set! (hu-textline-l t) 0 0)
    (hu-textline-needsupdate-set! t 1))   ; (true)

  (define (hulib-init-text-line t x y f sc)
    (hu-textline-x-set! t x)
    (hu-textline-y-set! t y)
    (hu-textline-f-set! t f)
    (hu-textline-sc-set! t sc)
    (hulib-clear-text-line t))

  (define (hulib-add-char-to-text-line t ch)
    (let ([len (hu-textline-len t)])
      (if (fx= len HU_MAXLINELENGTH)
          #f
          (begin
            (bytevector-u8-set! (hu-textline-l t) len (fxand ch #xFF))
            (bytevector-u8-set! (hu-textline-l t) (fx+ len 1) 0)
            (hu-textline-len-set! t (fx+ len 1))
            (hu-textline-needsupdate-set! t 4)
            #t))))

  (define (hulib-del-char-from-text-line t)
    (let ([len (hu-textline-len t)])
      (if (fx= len 0)
          #f
          (begin
            (hu-textline-len-set! t (fx- len 1))
            (bytevector-u8-set! (hu-textline-l t) (fx- len 1) 0)
            (hu-textline-needsupdate-set! t 4)
            #t))))

  (define (toupper c)
    (if (fx<= (char->integer #\a) c (char->integer #\z)) (fx- c 32) c))

  (define (hulib-draw-text-line l drawcursor)
    (let* ([f (hu-textline-f l)] [sc (hu-textline-sc l)] [y (hu-textline-y l)]
           ;; draw the new stuff
           [x (let loop ([i 0] [x (hu-textline-x l)])
                (if (fx< i (hu-textline-len l))
                    (let ([c (toupper (bytevector-u8-ref (hu-textline-l l) i))])
                      (if (and (not (fx= c 32)) (fx>= c sc) (fx<= c (char->integer #\_)))
                          (let* ([p (vector-ref f (fx- c sc))]
                                 [w (patch-width p)])
                            (if (fx> (fx+ x w) SCREENWIDTH)
                                x
                                (begin
                                  (v-draw-patch-direct x y FG p)
                                  (loop (fx+ i 1) (fx+ x w)))))
                          (let ([x (fx+ x 4)])
                            (if (fx>= x SCREENWIDTH)
                                x
                                (loop (fx+ i 1) x)))))
                    x))])
      ;; draw the cursor if requested
      (let ([cursor (vector-ref f (fx- (char->integer #\_) sc))])
        (when (and drawcursor (fx<= (fx+ x (patch-width cursor)) SCREENWIDTH))
          (v-draw-patch-direct x y FG cursor)))))

  ;; sorta called by HU_Erase and just better darn get things straight
  (define (hulib-erase-text-line l)
    ;; Only erases when NOT in automap and the screen is reduced, and the
    ;; text must either need updating or refreshing (because of a recent
    ;; change back from the automap)
    ;; (in 320x200: the view2d- window)
    (when (and (not automapactive) (not (fx= view2d-x 0))
               (not (fx= (hu-textline-needsupdate l) 0)))
      (let ([lh (fx+ (patch-height (vector-ref (hu-textline-f l) 0)) 1)])
        (do ([y (hu-textline-y l) (fx+ y 1)]
             [yoffset (fx* (hu-textline-y l) SCREENWIDTH) (fx+ yoffset SCREENWIDTH)])
            ((fx>= y (fx+ (hu-textline-y l) lh)))
          (if (or (fx< y view2d-y) (fx>= y (fx+ view2d-y view2d-height)))
              (r-video-erase yoffset SCREENWIDTH) ; erase entire line
              (begin
                (r-video-erase yoffset view2d-x) ; erase left border
                ;; erase right border
                (r-video-erase (fx+ yoffset view2d-x view2d-width) view2d-x))))))
    (unless (fx= (hu-textline-needsupdate l) 0)
      (hu-textline-needsupdate-set! l (fx- (hu-textline-needsupdate l) 1))))

  (define (hulib-init-stext s x y h font startchar on)
    (hu-stext-h-set! s h)
    (hu-stext-on-set! s on)
    (hu-stext-laston-set! s #t)
    (hu-stext-cl-set! s 0)
    (do ([i 0 (fx+ i 1)]) ((fx= i h))
      (hulib-init-text-line (vector-ref (hu-stext-l s) i)
                            x (fx- y (fx* i (fx+ (patch-height (vector-ref font 0)) 1)))
                            font startchar)))

  (define (hulib-add-line-to-stext s)
    ;; add a clear line
    (hu-stext-cl-set! s (let ([cl (fx+ (hu-stext-cl s) 1)]) (if (fx= cl (hu-stext-h s)) 0 cl)))
    (hulib-clear-text-line (vector-ref (hu-stext-l s) (hu-stext-cl s)))
    ;; everything needs updating
    (do ([i 0 (fx+ i 1)]) ((fx= i (hu-stext-h s)))
      (hu-textline-needsupdate-set! (vector-ref (hu-stext-l s) i) 4)))

  ;; prefix and msg are strings (prefix may be #f).
  (define (hulib-add-message-to-stext s prefix msg)
    (hulib-add-line-to-stext s)
    (let ([l (vector-ref (hu-stext-l s) (hu-stext-cl s))])
      (when prefix
        (string-for-each (lambda (c) (hulib-add-char-to-text-line l (char->integer c))) prefix))
      (string-for-each (lambda (c) (hulib-add-char-to-text-line l (char->integer c))) msg)))

  (define (hulib-draw-stext s)
    (when ((hu-stext-on s))             ; if not on, don't draw
      ;; draw everything
      (do ([i 0 (fx+ i 1)]) ((fx= i (hu-stext-h s)))
        (let ([idx (fx- (hu-stext-cl s) i)])
          ;; handle queue of lines
          (hulib-draw-text-line (vector-ref (hu-stext-l s) (if (fx< idx 0) (fx+ idx (hu-stext-h s)) idx))
                                #f)))))  ; no cursor, please

  (define (hulib-erase-stext s)
    (do ([i 0 (fx+ i 1)]) ((fx= i (hu-stext-h s)))
      (let ([l (vector-ref (hu-stext-l s) i)])
        (when (and (hu-stext-laston s) (not ((hu-stext-on s))))
          (hu-textline-needsupdate-set! l 4))
        (hulib-erase-text-line l)))
    (hu-stext-laston-set! s ((hu-stext-on s))))

  (define (hulib-init-itext it x y font startchar on)
    (hu-itext-lm-set! it 0)             ; default left margin is start of text
    (hu-itext-on-set! it on)
    (hu-itext-laston-set! it #t)
    (hulib-init-text-line (hu-itext-l it) x y font startchar))

  ;; The following deletion routines adhere to the left margin restriction
  (define (hulib-del-char-from-itext it)
    (unless (fx= (hu-textline-len (hu-itext-l it)) (hu-itext-lm it))
      (hulib-del-char-from-text-line (hu-itext-l it))))

  (define (hulib-erase-line-from-itext it)
    (let loop ()
      (unless (fx= (hu-itext-lm it) (hu-textline-len (hu-itext-l it)))
        (hulib-del-char-from-text-line (hu-itext-l it))
        (loop))))

  ;; Resets left margin as well
  (define (hulib-reset-itext it)
    (hu-itext-lm-set! it 0)
    (hulib-clear-text-line (hu-itext-l it)))

  (define (hulib-add-prefix-to-itext it str)
    (string-for-each (lambda (c) (hulib-add-char-to-text-line (hu-itext-l it) (char->integer c))) str)
    (hu-itext-lm-set! it (hu-textline-len (hu-itext-l it))))

  ;; wrapper function for handling general keyed input. returns true if it
  ;; ate the key
  (define (hulib-key-in-itext it ch)
    (let ([ch (fxand ch #xFF)])
      (cond
        [(and (fx>= ch 32) (fx<= ch (char->integer #\_)))
         (hulib-add-char-to-text-line (hu-itext-l it) ch)
         #t]
        [(fx= ch KEY_BACKSPACE) (hulib-del-char-from-itext it) #t]
        [(not (fx= ch KEY_ENTER)) #f]     ; did not eat key
        [else #t])))

  (define (hulib-draw-itext it)
    (when ((hu-itext-on it))
      (hulib-draw-text-line (hu-itext-l it) #t))) ; draw the line w/ cursor

  (define (hulib-erase-itext it)
    (when (and (hu-itext-laston it) (not ((hu-itext-on it))))
      (hu-textline-needsupdate-set! (hu-itext-l it) 4))
    (hulib-erase-text-line (hu-itext-l it))
    (hu-itext-laston-set! it ((hu-itext-on it)))))
