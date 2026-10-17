;;; hu_stuff.c: the heads-up display: the message line, the automap's
;;; level title, and multiplayer chat. Included by d_main.sls.

(define HU_FONTSTART (char->integer #\!)) ; the first font characters
(define HU_FONTEND (char->integer #\_))   ; the last font characters
(define HU_FONTSIZE (fx+ (fx- HU_FONTEND HU_FONTSTART) 1))
(define HU_BROADCAST 5)
(define HU_MSGREFRESH KEY_ENTER)
(define HU_MSGX 0)
(define HU_MSGY 0)
(define HU_MSGHEIGHT 1)                 ; in lines
(define HU_MSGTIMEOUT (* 4 TICRATE))

(define HU_TITLEX 0)
(define (HU_TITLEY) (fx- 167 (patch-height (vector-ref hu_font 0))))
(define HU_INPUTTOGGLE (char->integer #\t))
(define (HU_INPUTX) HU_MSGX)
(define (HU_INPUTY) (fx+ HU_MSGY (fx* HU_MSGHEIGHT (fx+ (patch-height (vector-ref hu_font 0)) 1))))

(define chat_macros
  (vector HUSTR_CHATMACRO0 HUSTR_CHATMACRO1 HUSTR_CHATMACRO2 HUSTR_CHATMACRO3
          HUSTR_CHATMACRO4 HUSTR_CHATMACRO5 HUSTR_CHATMACRO6 HUSTR_CHATMACRO7
          HUSTR_CHATMACRO8 HUSTR_CHATMACRO9))

(define plr #f)
(define hu_font (make-vector HU_FONTSIZE #f))
(define w_title (make-hu-textline))
(define chat_on #f)
(define w_chat (make-hu-itext))
(define always_off #f)
(define chat_dest (make-fxvector MAXPLAYERS 0))
(define w_inputbuffer
  (let ([v (make-vector MAXPLAYERS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS) v)
      (vector-set! v i (make-hu-itext)))))
(define message_on #f)
(define message_dontfuckwithme #f)
(define message_nottobefuckedwith #f)
(define w_message (make-hu-stext))
(define message_counter 0)
(define headsupactive #f)

;; Builtin map names. The actual names can be found in DStrings.h.
(define mapnames                        ; DOOM shareware/registered/retail (Ultimate) names.
  (vector HUSTR_E1M1 HUSTR_E1M2 HUSTR_E1M3 HUSTR_E1M4 HUSTR_E1M5 HUSTR_E1M6 HUSTR_E1M7
          HUSTR_E1M8 HUSTR_E1M9
          HUSTR_E2M1 HUSTR_E2M2 HUSTR_E2M3 HUSTR_E2M4 HUSTR_E2M5 HUSTR_E2M6 HUSTR_E2M7
          HUSTR_E2M8 HUSTR_E2M9
          HUSTR_E3M1 HUSTR_E3M2 HUSTR_E3M3 HUSTR_E3M4 HUSTR_E3M5 HUSTR_E3M6 HUSTR_E3M7
          HUSTR_E3M8 HUSTR_E3M9
          HUSTR_E4M1 HUSTR_E4M2 HUSTR_E4M3 HUSTR_E4M4 HUSTR_E4M5 HUSTR_E4M6 HUSTR_E4M7
          HUSTR_E4M8 HUSTR_E4M9
          "NEWLEVEL" "NEWLEVEL" "NEWLEVEL" "NEWLEVEL" "NEWLEVEL" "NEWLEVEL" "NEWLEVEL"
          "NEWLEVEL" "NEWLEVEL"))

(define mapnames2                       ; DOOM 2 map names.
  (vector HUSTR_1 HUSTR_2 HUSTR_3 HUSTR_4 HUSTR_5 HUSTR_6 HUSTR_7 HUSTR_8 HUSTR_9 HUSTR_10
          HUSTR_11 HUSTR_12 HUSTR_13 HUSTR_14 HUSTR_15 HUSTR_16 HUSTR_17 HUSTR_18 HUSTR_19
          HUSTR_20 HUSTR_21 HUSTR_22 HUSTR_23 HUSTR_24 HUSTR_25 HUSTR_26 HUSTR_27 HUSTR_28
          HUSTR_29 HUSTR_30 HUSTR_31 HUSTR_32))

;; (the Plutonia and TNT names, mapnamesp and mapnamest, are unused in
;; linuxdoom: HU_Start's cases for them are commented out)

(define (shift-table punct)
  ;; 0-31, then ' ' to '&', then the shifted punctuation and digits (32
  ;; entries from '\''), then '>' '?' '@', A-Z, [ ! ] " _ ', A-Z, { | } ~ 127
  (u8-list->bytevector
   (append (iota 32)
           (map char->integer (string->list " !\"#$%&"))
           (map char->integer (string->list punct))
           (map char->integer (string->list ">?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[!]\"_'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~"))
           (list 127))))
;;                          '()*+,-./0123456789:;<=
(define french_shiftxform (shift-table "\"()*+?_>?0123456789/.<+"))
(define english_shiftxform (shift-table "\"()*+<_>?)!@#$%^&*(::<+"))

(define frenchKeyMap
  (u8-list->bytevector
   (append (iota 32)
           (map char->integer
                (string->list
                 (string-append
                  " !\"#$%&%()*+;-:!"
                  "0123456789:M<=>?"
                  "@QBCDEFGHIJKL,NO"
                  "PARSTUVZXYW^\\$^_"
                  "@QBCDEFGHIJKL,NO"
                  "PARSTUVZXYW^\\$^")))
           (list 127))))

(define (foreign-translation ch)
  (if (fx< ch 128) (bytevector-u8-ref frenchKeyMap ch) ch))

(define shiftxform english_shiftxform)

(define (hu-init)
  ;; C tests `if (french)`, the enum constant (1), not `language == french`,
  ;; so linuxdoom always takes the French table.
  (set! shiftxform french_shiftxform)
  ;; load the heads-up font
  (do ([i 0 (fx+ i 1)]) ((fx= i HU_FONTSIZE))
    (vector-set! hu_font i (w-cache-lump-name (format "STCFN~3,'0d" (fx+ HU_FONTSTART i))))))

(define (hu-stop)
  (set! headsupactive #f))

(define (hu-start)
  (when headsupactive
    (hu-stop))
  (set! plr (vector-ref players consoleplayer))
  (set! message_on #f)
  (set! message_dontfuckwithme #f)
  (set! message_nottobefuckedwith #f)
  (set! chat_on #f)
  ;; create the message widget
  (hulib-init-stext w_message HU_MSGX HU_MSGY HU_MSGHEIGHT hu_font HU_FONTSTART
                    (lambda () message_on))
  ;; create the map title widget
  (hulib-init-text-line w_title HU_TITLEX (HU_TITLEY) hu_font HU_FONTSTART)
  (let ([s (if (memv gamemode (list shareware registered retail))
               (vector-ref mapnames (fx+ (fx* (fx- gameepisode 1) 9) gamemap -1))
               (vector-ref mapnames2 (fx- gamemap 1)))])
    (string-for-each (lambda (c) (hulib-add-char-to-text-line w_title (char->integer c))) s))
  ;; create the chat widget
  (hulib-init-itext w_chat (HU_INPUTX) (HU_INPUTY) hu_font HU_FONTSTART (lambda () chat_on))
  ;; create the inputbuffer widgets
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
    (hulib-init-itext (vector-ref w_inputbuffer i) 0 0 #f 0 (lambda () always_off)))
  (set! headsupactive #t))

(define (hu-drawer)
  (hulib-draw-stext w_message)
  (hulib-draw-itext w_chat)
  (when automapactive
    (hulib-draw-text-line w_title #f)))

(define (hu-erase)
  (hulib-erase-stext w_message)
  (hulib-erase-itext w_chat)
  (hulib-erase-text-line w_title))

;; A line's text as a string (up to its NUL).
(define (textline-string l)
  (let ([bv (hu-textline-l l)])
    (let loop ([n 0])
      (if (fx= (bytevector-u8-ref bv n) 0)
          (list->string (map integer->char (bytevector->u8-list (bytevector-truncate! (bytevector-copy bv) n))))
          (loop (fx+ n 1))))))

(define (hu-ticker)
  ;; tick down message counter if message is up
  (unless (fx= message_counter 0)
    (set! message_counter (fx- message_counter 1))
    (when (fx= message_counter 0)
      (set! message_on #f)
      (set! message_nottobefuckedwith #f)))
  (when (or (not (fx= showMessages 0)) message_dontfuckwithme)
    ;; display message if necessary
    (when (and (player-message plr)
               (or (not message_nottobefuckedwith) message_dontfuckwithme))
      (hulib-add-message-to-stext w_message #f (player-message plr))
      (player-message-set! plr #f)
      (set! message_on #t)
      (set! message_counter HU_MSGTIMEOUT)
      (set! message_nottobefuckedwith message_dontfuckwithme)
      (set! message_dontfuckwithme #f)))
  ;; check for incoming chat characters
  (when netgame
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (let* ([cmd (player-cmd (vector-ref players i))]
               [c (ticcmd-chatchar cmd)])
          (when (and (not (fx= i consoleplayer)) (not (fx= c 0)))
            (if (fx<= c HU_BROADCAST)
                (fxvector-set! chat_dest i c)
                (let* ([c (if (fx<= (char->integer #\a) c (char->integer #\z))
                              (bytevector-u8-ref shiftxform c)
                              c)]
                       [ib (vector-ref w_inputbuffer i)]
                       [rc (hulib-key-in-itext ib c)])
                  (when (and rc (fx= c KEY_ENTER))
                    (when (and (not (fx= (hu-textline-len (hu-itext-l ib)) 0))
                               (or (fx= (fxvector-ref chat_dest i) (fx+ consoleplayer 1))
                                   (fx= (fxvector-ref chat_dest i) HU_BROADCAST)))
                      (hulib-add-message-to-stext w_message (vector-ref player_names i)
                                                  (textline-string (hu-itext-l ib)))
                      (set! message_nottobefuckedwith #t)
                      (set! message_on #t)
                      (set! message_counter HU_MSGTIMEOUT)
                      (s-start-sound #f (if (fx= gamemode commercial) sfx_radio sfx_tink)))
                    (hulib-reset-itext ib))))
            (ticcmd-chatchar-set! cmd 0)))))))

(define QUEUESIZE 128)
(define chatchars (make-bytevector QUEUESIZE 0))
(define head 0)
(define tail 0)

(define (hu-queue-chat-char c)
  (if (fx= (fxand (fx+ head 1) (fx- QUEUESIZE 1)) tail)
      (player-message-set! plr HUSTR_MSGU)
      (begin
        (bytevector-u8-set! chatchars head (fxand c #xFF))
        (set! head (fxand (fx+ head 1) (fx- QUEUESIZE 1))))))

(define (hu-dequeue-chat-char)
  (if (fx= head tail)
      0
      (let ([c (bytevector-u8-ref chatchars tail)])
        (set! tail (fxand (fx+ tail 1) (fx- QUEUESIZE 1)))
        c)))

(define lastmessage "")
(define shiftdown #f)
(define altdown #f)
(define destination_keys (fxvector HUSTR_KEYGREEN HUSTR_KEYINDIGO HUSTR_KEYBROWN HUSTR_KEYRED))
(define num_nobrainers 0)

(define (hu-responder ev)
  (let ([numplayers (let loop ([i 0] [n 0])
                      (if (fx= i MAXPLAYERS) n
                          (loop (fx+ i 1) (if (vector-ref playeringame i) (fx+ n 1) n))))]
        [data1 (event-data1 ev)])
    (cond
      [(fx= data1 KEY_RSHIFT)
       (set! shiftdown (fx= (event-type ev) ev_keydown))
       #f]
      [(or (fx= data1 KEY_RALT) (fx= data1 KEY_LALT))
       (set! altdown (fx= (event-type ev) ev_keydown))
       #f]
      [(not (fx= (event-type ev) ev_keydown)) #f]
      [(not chat_on)
       (cond
         [(fx= data1 HU_MSGREFRESH)
          (set! message_on #t)
          (set! message_counter HU_MSGTIMEOUT)
          #t]
         [(and netgame (fx= data1 HU_INPUTTOGGLE))
          (set! chat_on #t)
          (hulib-reset-itext w_chat)
          (hu-queue-chat-char HU_BROADCAST)
          #t]
         [(and netgame (fx> numplayers 2))
          (let loop ([i 0] [eatkey #f])
            (cond
              [(fx= i MAXPLAYERS) eatkey]
              [(fx= data1 (fxvector-ref destination_keys i))
               (cond
                 [(and (vector-ref playeringame i) (not (fx= i consoleplayer)))
                  (set! chat_on #t)
                  (hulib-reset-itext w_chat)
                  (hu-queue-chat-char (fx+ i 1))
                  #t]
                 [(fx= i consoleplayer)
                  (set! num_nobrainers (fx+ num_nobrainers 1))
                  (player-message-set! plr (cond [(fx< num_nobrainers 3) HUSTR_TALKTOSELF1]
                                                 [(fx< num_nobrainers 6) HUSTR_TALKTOSELF2]
                                                 [(fx< num_nobrainers 9) HUSTR_TALKTOSELF3]
                                                 [(fx< num_nobrainers 32) HUSTR_TALKTOSELF4]
                                                 [else HUSTR_TALKTOSELF5]))
                  (loop (fx+ i 1) eatkey)]
                 [else (loop (fx+ i 1) eatkey)])]
              [else (loop (fx+ i 1) eatkey)]))]
         [else #f])]
      [else
       (let ([c (fxand data1 #xFF)])
         (if altdown
             ;; send a macro
             (let ([c (fxand (fx- c (char->integer #\0)) #xFF)])
               (if (fx> c 9)
                   #f
                   (let ([macromessage (vector-ref chat_macros c)])
                     ;; kill last message with a '\n'
                     (hu-queue-chat-char KEY_ENTER) ; DEBUG!!!
                     ;; send the macro message
                     (string-for-each (lambda (ch) (hu-queue-chat-char (char->integer ch))) macromessage)
                     (hu-queue-chat-char KEY_ENTER)
                     ;; leave chat mode and notify that it was sent
                     (set! chat_on #f)
                     (set! lastmessage macromessage)
                     (player-message-set! plr lastmessage)
                     #t)))
             ;; (`if (french)`: always, as in HU_Init)
             (let* ([c (foreign-translation c)]
                    [c (if (or shiftdown (fx<= (char->integer #\a) c (char->integer #\z)))
                           (bytevector-u8-ref shiftxform c)
                           c)]
                    [eatkey (hulib-key-in-itext w_chat c)])
               (when eatkey
                 (hu-queue-chat-char c))
               (cond
                 [(fx= c KEY_ENTER)
                  (set! chat_on #f)
                  (unless (fx= (hu-textline-len (hu-itext-l w_chat)) 0)
                    (set! lastmessage (textline-string (hu-itext-l w_chat)))
                    (player-message-set! plr lastmessage))]
                 [(fx= c KEY_ESCAPE)
                  (set! chat_on #f)])
               eatkey)))])))
