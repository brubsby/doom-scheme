;;; m_misc.c: the defaults file, file helpers and screenshots. Included by
;;; d_main.sls.


;; DEFAULTS

;; Variables only the config file uses here: the sound server's name and
;; memory size (SNDSERV), and the mouse device (LINUX).
(define sndserver_filename "sndserver")
(define mb_used 2)
(define mousedev "/dev/ttyS0")
(define mousetype "microsoft")

;; default_t: name, the variable (get and set), and the default value. C
;; tells strings from numbers by the default: anything outside +-0xfff is
;; a pointer to a string.
(define-syntax default
  (syntax-rules ()
    [(_ name var value) (list name (lambda () var) (lambda (v) (set! var v)) value)]))

(define defaults
  (list
   (default "mouse_sensitivity" mouseSensitivity 5)
   (default "sfx_volume" snd_SfxVolume 8)
   (default "music_volume" snd_MusicVolume 8)
   (default "show_messages" showMessages 1)
   (default "key_right" key_right KEY_RIGHTARROW)
   (default "key_left" key_left KEY_LEFTARROW)
   (default "key_up" key_up KEY_UPARROW)
   (default "key_down" key_down KEY_DOWNARROW)
   (default "key_strafeleft" key_strafeleft (char->integer #\,))
   (default "key_straferight" key_straferight (char->integer #\.))
   (default "key_fire" key_fire KEY_RCTRL)
   (default "key_use" key_use (char->integer #\space))
   (default "key_strafe" key_strafe KEY_RALT)
   (default "key_speed" key_speed KEY_RSHIFT)
   ;; UNIX hack, to be removed.
   (default "sndserver" sndserver_filename "sndserver")
   (default "mb_used" mb_used 2)
   (default "mousedev" mousedev "/dev/ttyS0")
   (default "mousetype" mousetype "microsoft")
   (default "use_mouse" usemouse 1)
   (default "mouseb_fire" mousebfire 0)
   (default "mouseb_strafe" mousebstrafe 1)
   (default "mouseb_forward" mousebforward 2)
   (default "use_joystick" usejoystick 0)
   (default "joyb_fire" joybfire 0)
   (default "joyb_strafe" joybstrafe 1)
   (default "joyb_use" joybuse 3)
   (default "joyb_speed" joybspeed 2)
   (default "screenblocks" screenblocks 9)
   (default "detaillevel" detailLevel 0)
   (default "snd_channels" numChannels 3)
   (default "usegamma" usegamma 0)))

;; chat_macros[i] (in hu_stuff, which is included later: hence the
;; procedures)
(define (chatmacro-defaults)
  (let loop ([i 0]
             [values (list HUSTR_CHATMACRO0 HUSTR_CHATMACRO1 HUSTR_CHATMACRO2 HUSTR_CHATMACRO3
                           HUSTR_CHATMACRO4 HUSTR_CHATMACRO5 HUSTR_CHATMACRO6 HUSTR_CHATMACRO7
                           HUSTR_CHATMACRO8 HUSTR_CHATMACRO9)])
    (if (null? values)
        '()
        (cons (let ([i i])
                (list (format "chatmacro~a" i)
                      (lambda () (vector-ref chat_macros i))
                      (lambda (v) (vector-set! chat_macros i v))
                      (car values)))
              (loop (fx+ i 1) (cdr values))))))

(define (all-defaults) (append defaults (chatmacro-defaults)))

(define defaultfile "")
(define basedefault "")

;; M_SaveDefaults
(define (m-save-defaults)
  (guard (c [(i/o-error? c) (void)])   ; can't write the file, but don't complain
    (call-with-port (open-file-output-port defaultfile (file-options no-fail)
                                           (buffer-mode block) (native-transcoder))
      (lambda (f)
        (for-each
         (lambda (d)
           (let ([v ((cadr d))])
             (if (string? (cadddr d))
                 (fprintf f "~a\t\t\"~a\"\n" (car d) v)
                 (fprintf f "~a\t\t~a\n" (car d) v))))
         (all-defaults))))))

;; M_LoadDefaults
(define (m-load-defaults)
  ;; set everything to base values
  (for-each (lambda (d) ((caddr d) (cadddr d))) (all-defaults))
  ;; check for a custom default file
  (let ([i (m-check-parm "-config")])
    (if (and (fx> i 0) (fx< i (fx- myargc 1)))
        (begin
          (set! defaultfile (vector-ref myargv (fx+ i 1)))
          (printf "	default file: ~a\n" defaultfile))
        (set! defaultfile basedefault)))
  ;; read the file in, overriding any set defaults
  (let ([text (guard (c [(i/o-error? c) #f])
                (call-with-port (open-file-input-port defaultfile) get-bytevector-all))])
    (when (bytevector? text)
      (let ([sc (make-scanner text)] [parm 0])
        ;; while (!feof(f)): fscanf (f, "%79s %[^\n]\n", def, strparm)
        (let loop ()
          (unless (scanner-eof? sc)
            (let* ([def (scan-word sc 79)]
                   [strparm (and def (begin (scan-space sc) (scan-line sc)))])
              (when strparm
                (scan-space sc)
                (let* ([isstring (and (fx> (string-length strparm) 0)
                                      (char=? (string-ref strparm 0) #\"))]
                       ;; get a string default: drop the first and last
                       ;; characters
                       [newstring (and isstring
                                       (substring strparm 1 (fxmax 1 (fx- (string-length strparm) 1))))])
                  (unless isstring
                    (let ([n (if (and (fx>= (string-length strparm) 2)
                                      (string=? (substring strparm 0 2) "0x"))
                                 (scan-int (substring strparm 2 (string-length strparm)) 16)
                                 (scan-int strparm 0))])
                      ;; (a failed sscanf leaves parm as it was)
                      (when n (set! parm n))))
                  (let ([d (assoc def (all-defaults))])
                    ;; (a value of the wrong kind would be a pointer stored as
                    ;; a number, or the reverse; it's ignored here)
                    (when d
                      (cond
                        [(and isstring (string? (cadddr d))) ((caddr d) newstring)]
                        [(and (not isstring) (not (string? (cadddr d)))) ((caddr d) parm)])))))
              (loop))))))))

;; Enough of fscanf for "%79s %[^\n]\n" over the file's bytes, with C's
;; isspace and feof (set when a read reaches the end).
(define (make-scanner bv) (vector bv 0 #f))
(define (scanner-eof? sc) (vector-ref sc 2))
(define (scan-peek sc)
  (let ([bv (vector-ref sc 0)] [p (vector-ref sc 1)])
    (if (fx< p (bytevector-length bv))
        (integer->char (bytevector-u8-ref bv p))
        (begin (vector-set! sc 2 #t) #f))))
(define (scan-advance! sc) (vector-set! sc 1 (fx+ (vector-ref sc 1) 1)))
(define (c-space? c) (memv c '(#\space #\tab #\newline #\vtab #\page #\return)))
(define (scan-space sc)
  (let loop ()
    (let ([c (scan-peek sc)])
      (when (and c (c-space? c)) (scan-advance! sc) (loop)))))
;; %Ns: skip white space, then up to n non-space characters; #f at the end
;; of the file.
(define (scan-word sc n)
  (scan-space sc)
  (and (scan-peek sc)
       (let loop ([k 0] [acc '()])
         (let ([c (scan-peek sc)])
           (if (and c (fx< k n) (not (c-space? c)))
               (begin (scan-advance! sc) (loop (fx+ k 1) (cons c acc)))
               (list->string (reverse acc)))))))
;; %[^\n]: one or more characters up to a newline; #f if none.
(define (scan-line sc)
  (let loop ([acc '()])
    (let ([c (scan-peek sc)])
      (if (and c (not (char=? c #\newline)))
          (begin (scan-advance! sc) (loop (cons c acc)))
          (and (pair? acc) (list->string (reverse acc)))))))

;; sscanf's %i (base 0: 0x hex, 0 octal, else decimal) or %x (base 16):
;; the longest number at the start of s after white space, wrapped to an
;; int, or #f if there is none.
(define (scan-int s base)
  (let* ([n (string-length s)]
         [i (let skip ([i 0]) (if (and (fx< i n) (c-space? (string-ref s i))) (skip (fx+ i 1)) i))]
         [neg (and (fx< i n) (char=? (string-ref s i) #\-))]
         [i (if (and (fx< i n) (memv (string-ref s i) '(#\- #\+))) (fx+ i 1) i)]
         [digit (lambda (c b) (let ([d (cond [(char<=? #\0 c #\9) (fx- (char->integer c) 48)]
                                             [(char<=? #\a (char-downcase c) #\f)
                                              (fx- (char->integer (char-downcase c)) 87)]
                                             [else 99])])
                                (and (fx< d b) d)))]
         [hexprefix (and (memv base '(0 16)) (fx< (fx+ i 1) n) (char=? (string-ref s i) #\0)
                         (memv (string-ref s (fx+ i 1)) '(#\x #\X))
                         (fx< (fx+ i 2) n) (digit (string-ref s (fx+ i 2)) 16))]
         [b (cond [hexprefix 16] [(fx= base 16) 16]
                  [(and (fx< i n) (char=? (string-ref s i) #\0)) 8] [else 10])]
         [i (if hexprefix (fx+ i 2) i)])
    (let loop ([i i] [v 0] [any #f])
      (let ([d (and (fx< i n) (digit (string-ref s i) b))])
        (if d
            (loop (fx+ i 1) (+ (* v b) d) #t)
            (and any (int32 (if neg (- v) v))))))))

;; M_WriteFile: the first `length` bytes of bv.
(define (m-write-file name bv length)
  (call-with-port (open-file-output-port name (file-options no-fail))
    (lambda (p) (put-bytevector p bv 0 length)))
  #t)

;; M_ReadFile: the file's bytes.
(define (m-read-file name)
  (guard (c [(i/o-error? c) (i-error "Couldn't read file ~a" name)])
    (call-with-port (open-file-input-port name) get-bytevector-all)))

;; SCREEN SHOTS

;; WritePCXfile: pcx_t's 128-byte header (8 bits per pixel, one plane),
;; the image, where any byte with its top two bits set is stored as 0xc1
;; and itself, then 0x0c and the 768-byte palette.
(define (write-pcx-file filename data width height palette)
  (let ([pcx (make-bytevector (fx+ (fx* width height 2) 1000) 0)])
    (define (short! off v) (bytevector-u16-set! pcx off v (endianness little)))
    (bytevector-u8-set! pcx 0 #x0a)           ; PCX id
    (bytevector-u8-set! pcx 1 5)              ; 256 color
    (bytevector-u8-set! pcx 2 1)              ; uncompressed
    (bytevector-u8-set! pcx 3 8)              ; 256 color
    (short! 4 0) (short! 6 0)                 ; xmin, ymin
    (short! 8 (fx- width 1)) (short! 10 (fx- height 1))
    (short! 12 width) (short! 14 height)      ; hres, vres
    ;; (palette[48] and reserved stay 0)
    (bytevector-u8-set! pcx 65 1)             ; chunky image
    (short! 66 width)                         ; bytes_per_line
    (short! 68 2)                             ; not a grey scale
    ;; pack the image
    (let loop ([i 0] [pack 128])
      (if (fx< i (fx* width height))
          (let ([b (bytevector-u8-ref data i)])
            (if (fx= (fxand b #xc0) #xc0)
                (begin (bytevector-u8-set! pcx pack #xc1)
                       (bytevector-u8-set! pcx (fx+ pack 1) b)
                       (loop (fx+ i 1) (fx+ pack 2)))
                (begin (bytevector-u8-set! pcx pack b)
                       (loop (fx+ i 1) (fx+ pack 1)))))
          (begin
            ;; write the palette
            (bytevector-u8-set! pcx pack #x0c) ; palette ID byte
            (bytevector-copy! palette 0 pcx (fx+ pack 1) 768)
            ;; write output file
            (m-write-file filename pcx (fx+ pack 1 768)))))))

;; M_ScreenShot
(define (m-screen-shot)
  ;; munge planar buffer to linear (I_ReadScreen into screens[2])
  (let ([linear (make-bytevector (fx* SCREENWIDTH SCREENHEIGHT))])
    (bytevector-copy! screens (screen-base 0) linear 0 (fx* SCREENWIDTH SCREENHEIGHT))
    (bytevector-copy! linear 0 screens (screen-base 2) (fx* SCREENWIDTH SCREENHEIGHT))
    ;; find a file name to save it to
    (let ([lbmname (let loop ([i 0])
                     (cond
                       [(fx= i 100) (i-error "M_ScreenShot: Couldn't create a PCX")]
                       [(file-exists? (format "DOOM~2,'0d.pcx" i)) (loop (fx+ i 1))]
                       [else (format "DOOM~2,'0d.pcx" i)]))])
      ;; save the pcx file
      (write-pcx-file lbmname linear SCREENWIDTH SCREENHEIGHT (w-cache-lump-name "PLAYPAL"))
      (player-message-set! (vector-ref players consoleplayer) "screen shot"))))
