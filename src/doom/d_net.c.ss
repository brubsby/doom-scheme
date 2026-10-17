;;; d_net.c: network game communication, and tic timing: commands go out
;;; as packets to every node (to this node itself as the "rebound" packet),
;;; and a tic runs when every node's command for it is in. i_net sends and
;;; receives. Included by d_main.sls.

(define NCMD_EXIT #x80000000)
(define NCMD_RETRANSMIT #x40000000)
(define NCMD_SETUP #x20000000)
(define NCMD_KILL #x10000000)           ; kill game
(define NCMD_CHECKSUM #x0fffffff)

(define BACKUPTICS 12)
(define RESENDCOUNT 10)
(define PL_DRONE #x80)                  ; bit flag in doomdata->player

(define (make-ticcmds n)
  (let ([v (make-vector n)])
    (do ([i 0 (fx+ i 1)]) ((fx= i n) v)
      (vector-set! v i (make-ticcmd)))))

(define localcmds (make-ticcmds BACKUPTICS))
;; netcmds[player][tic % BACKUPTICS]
(define netcmds
  (let ([v (make-vector MAXPLAYERS)])
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS) v)
      (vector-set! v i (make-ticcmds BACKUPTICS)))))
(define nettics (make-fxvector MAXNETNODES 0))
(define nodeingame (make-vector MAXNETNODES #f))   ; set false as nodes leave game
(define remoteresend (make-vector MAXNETNODES #f)) ; set when local needs tics
(define resendto (make-fxvector MAXNETNODES 0))    ; set when remote needs tics
(define resendcount (make-fxvector MAXNETNODES 0))
(define nodeforplayer (make-fxvector MAXPLAYERS 0))
(define skiptics 0)
(define maxsend 0)                      ; BACKUPTICS/(2*ticdup)-1

(define (netbuffer) (doomcom-data doomcom*))

(define reboundpacket #f)
(define reboundstore (make-doomdata))
(define (doomdata-copy! dst src)
  (doomdata-checksum-set! dst (doomdata-checksum src))
  (doomdata-retransmitfrom-set! dst (doomdata-retransmitfrom src))
  (doomdata-starttic-set! dst (doomdata-starttic src))
  (doomdata-player-set! dst (doomdata-player src))
  (doomdata-numtics-set! dst (doomdata-numtics src))
  (do ([i 0 (fx+ i 1)]) ((fx= i BACKUPTICS))
    (struct-copy! (vector-ref (doomdata-cmds dst) i) (vector-ref (doomdata-cmds src) i))))

;; -debugfile: debugN.txt, a log of the packets (as their bytes in memory:
;; the checksum and the ticcmds' shorts little-endian) and of TryRunTics.
(define debugfile #f)
(define (debug-bytes)
  (let* ([nb (netbuffer)] [bv (make-bytevector (fx+ 8 (fx* 8 BACKUPTICS)) 0)])
    (bytevector-u32-set! bv 0 (doomdata-checksum nb) (endianness little))
    (bytevector-u8-set! bv 4 (doomdata-retransmitfrom nb))
    (bytevector-u8-set! bv 5 (doomdata-starttic nb))
    (bytevector-u8-set! bv 6 (doomdata-player nb))
    (bytevector-u8-set! bv 7 (doomdata-numtics nb))
    (do ([c 0 (fx+ c 1)]) ((fx= c BACKUPTICS))
      (let ([cmd (vector-ref (doomdata-cmds nb) c)] [o (fx+ 8 (fx* 8 c))])
        (bytevector-s8-set! bv o (ticcmd-forwardmove cmd))
        (bytevector-s8-set! bv (fx+ o 1) (ticcmd-sidemove cmd))
        (bytevector-u16-set! bv (fx+ o 2) (fxand (ticcmd-angleturn cmd) #xffff) (endianness little))
        (bytevector-u16-set! bv (fx+ o 4) (fxand (ticcmd-consistancy cmd) #xffff) (endianness little))
        (bytevector-u8-set! bv (fx+ o 6) (ticcmd-chatchar cmd))
        (bytevector-u8-set! bv (fx+ o 7) (ticcmd-buttons cmd))))
    (do ([i 0 (fx+ i 1)]) ((fx= i (doomcom-datalength doomcom*)))
      (fprintf debugfile "~a " (bytevector-u8-ref bv (fxmin i (fx- (bytevector-length bv) 1)))))
    (fprintf debugfile "\n")))
(define (debug-retransmit)
  (if (fxlogtest (doomdata-checksum (netbuffer)) NCMD_RETRANSMIT)
      (expand-tics (doomdata-retransmitfrom (netbuffer)))
      -1))

;; NetbufferSize: the header and the tics in it.
(define (netbuffer-size) (fx+ 8 (fx* 8 (doomdata-numtics (netbuffer)))))

;; NetbufferChecksum: NORMALUNIX returns 0 ("byte order problems").
(define (netbuffer-checksum) 0)

;; ExpandTics: a tic number from its low byte, near maketic.
(define (expand-tics low)
  (let ([delta (fx- low (fxand maketic #xff))])
    (cond
      [(fx<= -64 delta 64) (fx+ (fxand maketic (fxnot #xff)) low)]
      [(fx> delta 64) (fx+ (fx- (fxand maketic (fxnot #xff)) 256) low)]
      [else (fx+ (fx+ (fxand maketic (fxnot #xff)) 256) low)])))

;; HSendPacket
(define (h-send-packet node flags)
  (doomdata-checksum-set! (netbuffer) (fxior (netbuffer-checksum) flags))
  (cond
    [(fx= node 0)
     (doomdata-copy! reboundstore (netbuffer))
     (set! reboundpacket #t)]
    [demoplayback (void)]
    [else
     (unless netgame
       (i-error "Tried to transmit to another node"))
     (doomcom-command-set! doomcom* CMD_SEND)
     (doomcom-remotenode-set! doomcom* node)
     (doomcom-datalength-set! doomcom* (netbuffer-size))
     (when debugfile
       (fprintf debugfile "send (~a + ~a, R ~a) [~a] "
                (expand-tics (doomdata-starttic (netbuffer))) (doomdata-numtics (netbuffer))
                (debug-retransmit) (doomcom-datalength doomcom*))
       (debug-bytes))
     (i-net-cmd)]))

;; HGetPacket: returns #f if no packet is waiting.
(define (h-get-packet)
  (cond
    [reboundpacket
     (doomdata-copy! (netbuffer) reboundstore)
     (doomcom-remotenode-set! doomcom* 0)
     (set! reboundpacket #f)
     #t]
    [(not netgame) #f]
    [demoplayback #f]
    [else
     (doomcom-command-set! doomcom* CMD_GET)
     (i-net-cmd)
     (cond
       [(fx= (doomcom-remotenode doomcom*) -1) #f]
       [(not (fx= (doomcom-datalength doomcom*) (netbuffer-size)))
        (when debugfile
          (fprintf debugfile "bad packet length ~a\n" (doomcom-datalength doomcom*)))
        #f]
       [(not (fx= (netbuffer-checksum) (fxand (doomdata-checksum (netbuffer)) NCMD_CHECKSUM)))
        (when debugfile (fprintf debugfile "bad packet checksum\n"))
        #f]
       [else
        (when debugfile
          (if (fxlogtest (doomdata-checksum (netbuffer)) NCMD_SETUP)
              (fprintf debugfile "setup packet\n")
              (begin
                (fprintf debugfile "get ~a = (~a + ~a, R ~a)[~a] "
                         (doomcom-remotenode doomcom*)
                         (expand-tics (doomdata-starttic (netbuffer))) (doomdata-numtics (netbuffer))
                         (debug-retransmit) (doomcom-datalength doomcom*))
                (debug-bytes))))
        #t])]))

;; GetPackets
(define exitmsg "")
(define (get-packets)
  (let loop ()
    (when (h-get-packet)
      (let ([nb (netbuffer)])
        (unless (fxlogtest (doomdata-checksum nb) NCMD_SETUP) ; extra setup packet
          (let* ([netconsole (fxand (doomdata-player nb) (fxnot PL_DRONE))]
                 [netnode (doomcom-remotenode doomcom*)]
                 ;; to save bytes, only the low byte of tic numbers are sent
                 ;; Figure out what the rest of the bytes are
                 [realstart (expand-tics (doomdata-starttic nb))]
                 [realend (fx+ realstart (doomdata-numtics nb))])
            (cond
              ;; check for exiting the game
              [(fxlogtest (doomdata-checksum nb) NCMD_EXIT)
               (when (vector-ref nodeingame netnode)
                 (vector-set! nodeingame netnode #f)
                 (vector-set! playeringame netconsole #f)
                 (set! exitmsg (format "Player ~a left the game" (fx+ netconsole 1)))
                 (player-message-set! (vector-ref players consoleplayer) exitmsg)
                 (when demorecording
                   (g-check-demo-status)))]
              ;; check for a remote game kill
              [(fxlogtest (doomdata-checksum nb) NCMD_KILL)
               (i-error "Killed by network driver")]
              [else
               (fxvector-set! nodeforplayer netconsole netnode)
               ;; check for retransmit request
               (if (and (fx<= (fxvector-ref resendcount netnode) 0)
                        (fxlogtest (doomdata-checksum nb) NCMD_RETRANSMIT))
                   (begin
                     (fxvector-set! resendto netnode (expand-tics (doomdata-retransmitfrom nb)))
                     (when debugfile
                       (fprintf debugfile "retransmit from ~a\n" (fxvector-ref resendto netnode)))
                     (fxvector-set! resendcount netnode RESENDCOUNT))
                   (fxvector-set! resendcount netnode (fx- (fxvector-ref resendcount netnode) 1)))
               (cond
                 ;; check for out of order / duplicated packet
                 [(fx= realend (fxvector-ref nettics netnode)) (void)]
                 [(fx< realend (fxvector-ref nettics netnode))
                  (when debugfile
                    (fprintf debugfile "out of order packet (~a + ~a)\n" realstart (doomdata-numtics nb)))]
                 ;; check for a missed packet: stop processing until the
                 ;; other system resends the missed tics
                 [(fx> realstart (fxvector-ref nettics netnode))
                  (when debugfile
                    (fprintf debugfile "missed tics from ~a (~a - ~a)\n"
                             netnode realstart (fxvector-ref nettics netnode)))
                  (vector-set! remoteresend netnode #t)]
                 [else
                  ;; update command store from the packet
                  (vector-set! remoteresend netnode #f)
                  (let loop ([src (fx- (fxvector-ref nettics netnode) realstart)])
                    (when (fx< (fxvector-ref nettics netnode) realend)
                      (let ([dest (vector-ref (vector-ref netcmds netconsole)
                                              (fxremainder (fxvector-ref nettics netnode) BACKUPTICS))])
                        (fxvector-set! nettics netnode (fx+ (fxvector-ref nettics netnode) 1))
                        (struct-copy! dest (vector-ref (doomdata-cmds nb) src))
                        (loop (fx+ src 1)))))])]))))
      (loop))))

;; NetUpdate: builds ticcmds for console player, sends out a packet.
(define gametime 0)
(define (net-update)
  ;; check time
  (let* ([nowtime (fxquotient (i-get-time) ticdup)]
         [newtics (fx- nowtime gametime)])
    (set! gametime nowtime)
    (when (fx> newtics 0)               ; (else nothing new to update)
      (let ([newtics (if (fx<= skiptics newtics)
                         (let ([n (fx- newtics skiptics)]) (set! skiptics 0) n)
                         (begin (set! skiptics (fx- skiptics newtics)) 0))]
            [gameticdiv (fxquotient gametic ticdup)])
        (doomdata-player-set! (netbuffer) consoleplayer)
        ;; build new ticcmds for console player
        (let loop ([i 0])
          (when (fx< i newtics)
            (i-start-tic d-post-event)
            (d-process-events)
            (unless (fx>= (fx- maketic gameticdiv) (fx- (fxquotient BACKUPTICS 2) 1)) ; can't hold any more
              (g-build-ticcmd (vector-ref localcmds (fxremainder maketic BACKUPTICS)))
              (set! maketic (fx+ maketic 1))
              (loop (fx+ i 1)))))
        ;; singletic update is syncronous
        (unless singletics
          ;; send the packet to the other nodes
          (do ([i 0 (fx+ i 1)]) ((fx= i (doomcom-numnodes doomcom*)))
            (when (vector-ref nodeingame i)
              (let* ([nb (netbuffer)] [realstart (fxvector-ref resendto i)])
                (doomdata-starttic-set! nb (fxand realstart #xff))
                (doomdata-numtics-set! nb (fxand (fx- maketic realstart) #xff))
                (when (fx> (fx- maketic realstart) BACKUPTICS)
                  (i-error "NetUpdate: netbuffer->numtics > BACKUPTICS"))
                (fxvector-set! resendto i (fx- maketic (doomcom-extratics doomcom*)))
                (do ([j 0 (fx+ j 1)]) ((fx= j (doomdata-numtics nb)))
                  (struct-copy! (vector-ref (doomdata-cmds nb) j)
                                (vector-ref localcmds (fxremainder (fx+ realstart j) BACKUPTICS))))
                (if (vector-ref remoteresend i)
                    (begin
                      (doomdata-retransmitfrom-set! nb (fxand (fxvector-ref nettics i) #xff))
                      (h-send-packet i NCMD_RETRANSMIT))
                    (begin
                      (doomdata-retransmitfrom-set! nb 0)
                      (h-send-packet i 0)))))))))
    ;; listen for other packets
    (get-packets)))

;; CheckAbort: ESC gives up on starting a netgame.
(define (check-abort)
  (let ([stoptic (fx+ (i-get-time) 2)])
    (let loop ()
      (when (fx< (i-get-time) stoptic)
        (i-start-tic d-post-event)
        (loop))))
  (i-start-tic d-post-event)
  (let loop ()
    (unless (fx= eventtail eventhead)
      (let ([ev (vector-ref events eventtail)])
        (when (and (fx= (event-type ev) ev_keydown) (fx= (event-data1 ev) KEY_ESCAPE))
          (i-error "Network game synchronization aborted.")))
      (set! eventtail (fxand (fx+ eventtail 1) (fx- MAXEVENTS 1)))
      (loop))))

;; D_ArbitrateNetStart: the key player sends the game's settings; the
;; others wait for them.
(define (d-arbitrate-net-start)
  (set! autostart #t)
  (if (not (fx= (doomcom-consoleplayer doomcom*) 0))
      ;; listen for setup info from key player
      (begin
        (printf "listening for network start info...\n")
        (let loop ()
          (check-abort)
          (if (and (h-get-packet) (fxlogtest (doomdata-checksum (netbuffer)) NCMD_SETUP))
              (let ([nb (netbuffer)])
                (unless (fx= (doomdata-player nb) VERSION)
                  (i-error "Different DOOM versions cannot play a net game!"))
                (set! startskill (fxand (doomdata-retransmitfrom nb) 15))
                (set! deathmatch (fxsra (fxand (doomdata-retransmitfrom nb) #xc0) 6))
                (set! nomonsters (fx> (fxand (doomdata-retransmitfrom nb) #x20) 0))
                (set! respawnparm (fx> (fxand (doomdata-retransmitfrom nb) #x10) 0))
                (set! startmap (fxand (doomdata-starttic nb) #x3f))
                (set! startepisode (fxsra (doomdata-starttic nb) 6)))
              (loop))))
      ;; key player, send the setup info
      (begin
        (printf "sending network start info...\n")
        (let ([gotinfo (make-vector MAXNETNODES #f)])
          (let loop ()
            (check-abort)
            (do ([i 0 (fx+ i 1)]) ((fx= i (doomcom-numnodes doomcom*)))
              (let ([nb (netbuffer)])
                (doomdata-retransmitfrom-set!
                 nb (fxior startskill
                           (if (fx= deathmatch 0) 0 (fxsll deathmatch 6))
                           (if nomonsters #x20 0)
                           (if respawnparm #x10 0)))
                (doomdata-starttic-set! nb (fxand (fx+ (fx* startepisode 64) startmap) #xff))
                (doomdata-player-set! nb VERSION)
                (doomdata-numtics-set! nb 0)
                (h-send-packet i NCMD_SETUP)))
            (let get ([i 10])
              (when (and (fx> i 0) (h-get-packet))
                (let ([p (fxand (doomdata-player (netbuffer)) #x7f)])
                  (when (fx< p MAXNETNODES)
                    (vector-set! gotinfo p #t)))
                (get (fx- i 1))))
            (unless (let all ([i 1])
                      (or (fx>= i (doomcom-numnodes doomcom*))
                          (and (vector-ref gotinfo i) (all (fx+ i 1)))))
              (loop)))))))

;; D_CheckNetGame: works out player numbers among the net participants.
(define (d-check-net-game)
  (do ([i 0 (fx+ i 1)]) ((fx= i MAXNETNODES))
    (vector-set! nodeingame i #f)
    (fxvector-set! nettics i 0)
    (vector-set! remoteresend i #f)     ; set when local needs tics
    (fxvector-set! resendto i 0))       ; which tic to start sending
  ;; I_InitNetwork sets doomcom* and netgame
  (i-init-network)
  (unless (fx= (doomcom-id doomcom*) DOOMCOM_ID)
    (i-error "Doomcom buffer invalid!"))
  (set! consoleplayer (doomcom-consoleplayer doomcom*))
  (set! displayplayer consoleplayer)
  (when netgame
    (d-arbitrate-net-start))
  (printf "startskill ~a  deathmatch: ~a  startmap: ~a  startepisode: ~a\n"
          startskill deathmatch startmap startepisode)
  ;; read values out of doomcom*
  (set! ticdup (doomcom-ticdup doomcom*))
  (set! maxsend (fxmax 1 (fx- (fxquotient BACKUPTICS (fx* 2 ticdup)) 1)))
  (do ([i 0 (fx+ i 1)]) ((fx= i (doomcom-numplayers doomcom*)))
    (vector-set! playeringame i #t))
  (do ([i 0 (fx+ i 1)]) ((fx= i (doomcom-numnodes doomcom*)))
    (vector-set! nodeingame i #t))
  (printf "player ~a of ~a (~a nodes)\n"
          (fx+ consoleplayer 1) (doomcom-numplayers doomcom*) (doomcom-numnodes doomcom*)))

;; D_QuitNetGame: called before quitting to leave a net game without
;; hanging the other players.
(define (d-quit-net-game)
  (when debugfile
    (close-port debugfile)
    (set! debugfile #f))
  (when (and netgame usergame (not (fx= consoleplayer -1)) (not demoplayback))
    ;; send a bunch of packets for security
    (doomdata-player-set! (netbuffer) consoleplayer)
    (doomdata-numtics-set! (netbuffer) 0)
    (do ([i 0 (fx+ i 1)]) ((fx= i 4))
      (do ([j 1 (fx+ j 1)]) ((fx= j (doomcom-numnodes doomcom*)))
        (when (vector-ref nodeingame j)
          (h-send-packet j NCMD_EXIT)))
      (i-wait-vbl 1))))

;; TryRunTics
(define oldentertics 0)

(define (run-one-tic)
  (interp-save-tic!)
  (set! lasttic-ms (i-get-time-ms))
  (when advancedemo
    (d-do-advance-demo))
  (m-ticker)
  (g-ticker)
  (set! gametic (fx+ gametic 1))
  (when d-tic-hook (d-tic-hook)))

;; The lowest tic every node in the game has sent.
(define (lowest-nettic)
  (let loop ([i 0] [lowtic (greatest-fixnum)])
    (if (fx= i (doomcom-numnodes doomcom*))
        lowtic
        (loop (fx+ i 1) (if (and (vector-ref nodeingame i) (fx< (fxvector-ref nettics i) lowtic))
                            (fxvector-ref nettics i)
                            lowtic)))))

(define frameon 0)
(define frameskip (make-vector 4 #f))
(define oldnettics 0)

(define (try-run-tics)
  ;; get real tics
  (let* ([entertic (fxquotient (i-get-time) ticdup)]
         [realtics (fx- entertic oldentertics)])
    (set! oldentertics entertic)
    ;; get available tics
    (net-update)
    (let* ([lowtic (lowest-nettic)]
           [availabletics (fx- lowtic (fxquotient gametic ticdup))]
           ;; decide how many tics to run
           [counts (cond [(fx< realtics (fx- availabletics 1)) (fx+ realtics 1)]
                         [(fx< realtics availabletics) realtics]
                         [else availabletics])]
           [counts (if (fx< counts 1) 1 counts)])
      (set! frameon (fx+ frameon 1))
      (when debugfile
        (fprintf debugfile "=======real: ~a  avail: ~a  game: ~a\n" realtics availabletics counts))
      (unless demoplayback
        ;; ideally nettics[0] should be 1 - 3 tics above lowtic; if we are
        ;; consistantly slower, speed up time
        (let ([i (let find ([i 0])
                   (if (or (fx= i MAXPLAYERS) (vector-ref playeringame i)) i (find (fx+ i 1))))])
          (unless (fx= consoleplayer i)   ; the key player does not adapt
            (let ([theirs (fxvector-ref nettics (fxvector-ref nodeforplayer i))])
              (when (fx<= (fxvector-ref nettics 0) theirs)
                (set! gametime (fx- gametime 1)))
              (vector-set! frameskip (fxand frameon 3) (fx> oldnettics theirs))
              (set! oldnettics (fxvector-ref nettics 0))
              (when (and (vector-ref frameskip 0) (vector-ref frameskip 1)
                         (vector-ref frameskip 2) (vector-ref frameskip 3))
                (set! skiptics 1))))))
      ;; wait for new tics if needed (-uncapped: don't wait; run what there
      ;; is, possibly nothing, and draw another frame)
      (let wait ([lowtic lowtic]
                 [counts (if interp-enabled (fxmin counts availabletics) counts)])
        (if (fx< lowtic (fx+ (fxquotient gametic ticdup) counts))
            (begin
              (net-update)
              (let ([lowtic (lowest-nettic)])
                (when (fx< lowtic (fxquotient gametic ticdup))
                  (i-error "TryRunTics: lowtic < gametic"))
                ;; don't stay in here forever -- give the menu a chance to
                ;; work
                (if (fx>= (fx- (fxquotient (i-get-time) ticdup) entertic) 20)
                    (m-ticker)
                    (wait lowtic counts))))
            ;; run the count * ticdup dics
            (do ([c counts (fx- c 1)]) ((fx= c 0))
              (do ([i 0 (fx+ i 1)]) ((fx= i ticdup))
                (when (fx> (fxquotient gametic ticdup) lowtic)
                  (i-error "gametic>lowtic"))
                (run-one-tic)
                ;; modify command for duplicated tics
                (unless (fx= i (fx- ticdup 1))
                  (let ([buf (fxremainder (fxquotient gametic ticdup) BACKUPTICS)])
                    (do ([j 0 (fx+ j 1)]) ((fx= j MAXPLAYERS))
                      (let ([cmd (vector-ref (vector-ref netcmds j) buf)])
                        (ticcmd-chatchar-set! cmd 0)
                        (when (fxlogtest (ticcmd-buttons cmd) BT_SPECIAL)
                          (ticcmd-buttons-set! cmd 0)))))))
              (net-update)))))))        ; check for new console commands
