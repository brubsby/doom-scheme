;;; i_net: the network driver, as linuxdoom's i_net.c: UDP datagrams on
;;; port DOOMPORT (5029, or -port), one per send to a node, read without
;;; blocking. A packet is identified by its sender's address alone. The
;;; wire format is doomdata_t's, with the checksum and the ticcmds' shorts
;;; in network byte order, so the port and linuxdoom can play together.
;;;
;;; The sockets are libc's, through Chez's FFI.
(library (doom i_net)
  (export i-init-network i-net-cmd net-bind-address
          doomcom* doomcom-id doomcom-command doomcom-command-set!
          doomcom-remotenode doomcom-remotenode-set! doomcom-datalength doomcom-datalength-set!
          doomcom-numnodes doomcom-ticdup doomcom-extratics doomcom-consoleplayer
          doomcom-numplayers doomcom-data
          doomdata make-doomdata doomdata? doomdata-checksum doomdata-checksum-set!
          doomdata-retransmitfrom doomdata-retransmitfrom-set! doomdata-starttic doomdata-starttic-set!
          doomdata-player doomdata-player-set! doomdata-numtics doomdata-numtics-set! doomdata-cmds
          DOOMCOM_ID MAXNETNODES CMD_SEND CMD_GET)
  (import (chezscheme) (doom global) (doom struct) (doom doomdef) (doom doomstat)
          (doom d_ticcmd) (doom m_argv) (doom i_system))

  (define MAXNETNODES 8)                ; max computers in a game
  (define BACKUPTICS 12)                ; commands kept to resend
  (define CMD_SEND 1)
  (define CMD_GET 2)
  (define DOOMCOM_ID #x12345678)

  ;; doomdata_t: the packet. (The bytes are the network format; see
  ;; packet-send.)
  (define-struct doomdata ()
    (checksum 0)                        ; high bit is retransmit request
    (retransmitfrom 0)                  ; only valid if NCMD_RETRANSMIT
    (starttic 0)
    (player 0)
    (numtics 0)
    (cmds (let ([v (make-vector BACKUPTICS)])
            (do ([i 0 (fx+ i 1)]) ((fx= i BACKUPTICS) v)
              (vector-set! v i (make-ticcmd))))))

  ;; doomcom_t: the communication between DOOM and the driver.
  (define-struct doomcom ()
    (id 0) (intnum 0) (command 0) (remotenode 0) (datalength 0)
    (numnodes 0) (ticdup 1) (extratics 0) (deathmatch 0) (savegame 0)
    (episode 0) (map 0) (skill 0) (consoleplayer 0) (numplayers 0)
    (angleoffset 0) (drone 0)
    (data (make-doomdata)))

;; C's doomcom: the doomcom_t I_InitNetwork sets up.
  (define-global doomcom* (make-doomcom))

  ;; Tests run two nodes on one machine: each must then have its own
  ;; address (packets are told apart by address alone), and both sockets
  ;; are bound to it. #f: INADDR_ANY, as linuxdoom.
  (define-global net-bind-address #f)

  ;; libc
  (define libc (load-shared-object "libc.so.6"))
  (define c-socket (foreign-procedure "socket" (int int int) int))
  (define c-bind (foreign-procedure "bind" (int u8* int) int))
  (define c-sendto (foreign-procedure "sendto" (int u8* size_t int u8* int) iptr))
  (define c-recvfrom (foreign-procedure "recvfrom" (int u8* size_t int u8* u8*) iptr))
  (define c-inet-addr (foreign-procedure "inet_addr" (string) unsigned-32))
  (define c-gethostbyname (foreign-procedure "gethostbyname" (string) uptr))
  (define c-errno-location (foreign-procedure "__errno_location" () uptr))
  (define c-strerror (foreign-procedure "strerror" (int) string))
  (define (errno) (foreign-ref 'int (c-errno-location) 0))
  (define PF_INET 2)
  (define SOCK_DGRAM 2)
  (define SOCK_NONBLOCK #o4000)
  (define IPPROTO_UDP 17)
  (define EWOULDBLOCK 11)

  (define DOOMPORT (+ 5000 #x1d))       ; IPPORT_USERRESERVED + 0x1d
  (define sendsocket -1)
  (define insocket -1)
  ;; sockaddr_in per node: family, port (network order), address (as
  ;; stored: network order), zero padding.
  (define sendaddress
    (let ([v (make-vector MAXNETNODES)])
      (do ([i 0 (fx+ i 1)]) ((fx= i MAXNETNODES) v)
        (vector-set! v i (make-bytevector 16 0)))))

  (define (sockaddr! sa addr port)
    (bytevector-fill! sa 0)
    (bytevector-u16-native-set! sa 0 PF_INET)
    (bytevector-u16-set! sa 2 port (endianness big))
    (bytevector-u32-native-set! sa 4 addr))
  (define (sockaddr-addr sa) (bytevector-u32-native-ref sa 4))

  (define (udp-socket)
    ;; (linuxdoom sets non-blocking with ioctl FIONBIO on the in socket)
    (let ([s (c-socket PF_INET (fxior SOCK_DGRAM SOCK_NONBLOCK) IPPROTO_UDP)])
      (when (fx< s 0)
        (i-error "can't create socket: ~a" (c-strerror (errno))))
      s))

  (define (bind-to-local-port s port)
    (let ([address (make-bytevector 16 0)])
      (sockaddr! address (if net-bind-address (c-inet-addr net-bind-address) 0) port)
      (when (fx= (c-bind s address 16) -1)
        (i-error "BindToPort: bind: ~a" (c-strerror (errno))))))

  ;; The packet as bytes: 8 bytes of header, then 8 per ticcmd.
  (define packet (make-bytevector (fx+ 8 (fx* 8 BACKUPTICS)) 0))

  (define (packet-send)
    (let ([nb (doomcom-data doomcom*)])
      ;; byte swap
      (bytevector-u32-set! packet 0 (doomdata-checksum nb) (endianness big))
      (bytevector-u8-set! packet 4 (doomdata-retransmitfrom nb))
      (bytevector-u8-set! packet 5 (doomdata-starttic nb))
      (bytevector-u8-set! packet 6 (doomdata-player nb))
      (bytevector-u8-set! packet 7 (doomdata-numtics nb))
      (do ([c 0 (fx+ c 1)]) ((fx>= c (fxmin (doomdata-numtics nb) BACKUPTICS)))
        (let ([cmd (vector-ref (doomdata-cmds nb) c)] [o (fx+ 8 (fx* 8 c))])
          (bytevector-s8-set! packet o (ticcmd-forwardmove cmd))
          (bytevector-s8-set! packet (fx+ o 1) (ticcmd-sidemove cmd))
          (bytevector-u16-set! packet (fx+ o 2) (fxand (ticcmd-angleturn cmd) #xffff) (endianness big))
          (bytevector-u16-set! packet (fx+ o 4) (fxand (ticcmd-consistancy cmd) #xffff) (endianness big))
          (bytevector-u8-set! packet (fx+ o 6) (ticcmd-chatchar cmd))
          (bytevector-u8-set! packet (fx+ o 7) (ticcmd-buttons cmd))))
      (c-sendto sendsocket packet (doomcom-datalength doomcom*) 0
                (vector-ref sendaddress (doomcom-remotenode doomcom*)) 16)
      (void)))

  (define first-packet #t)
  (define fromaddress (make-bytevector 16 0))
  (define fromlen (make-bytevector 4 0))

  (define (packet-get)
    (bytevector-s32-native-set! fromlen 0 16)
    (let ([c (c-recvfrom insocket packet (bytevector-length packet) 0 fromaddress fromlen)])
      (if (fx= c -1)
          (begin
            (unless (fx= (errno) EWOULDBLOCK)
              (i-error "GetPacket: ~a" (c-strerror (errno))))
            (doomcom-remotenode-set! doomcom* -1)) ; no packet
          (begin
            (when first-packet
              (printf "len=~a:p=[0x~x 0x~x] \n" c
                      (bytevector-u32-native-ref packet 0) (bytevector-u32-native-ref packet 4))
              (set! first-packet #f))
            ;; find remote node number
            (let ([i (let loop ([i 0])
                       (cond
                         [(fx= i (doomcom-numnodes doomcom*)) i]
                         [(= (sockaddr-addr fromaddress) (sockaddr-addr (vector-ref sendaddress i))) i]
                         [else (loop (fx+ i 1))]))])
              (if (fx= i (doomcom-numnodes doomcom*))
                  ;; packet is not from one of the players (new game broadcast)
                  (doomcom-remotenode-set! doomcom* -1)
                  (let ([nb (doomcom-data doomcom*)])
                    ;; good packet from a game player
                    (doomcom-remotenode-set! doomcom* i)
                    (doomcom-datalength-set! doomcom* c)
                    ;; byte swap
                    (doomdata-checksum-set! nb (bytevector-u32-ref packet 0 (endianness big)))
                    (doomdata-player-set! nb (bytevector-u8-ref packet 6))
                    (doomdata-retransmitfrom-set! nb (bytevector-u8-ref packet 4))
                    (doomdata-starttic-set! nb (bytevector-u8-ref packet 5))
                    (doomdata-numtics-set! nb (bytevector-u8-ref packet 7))
                    (do ([c 0 (fx+ c 1)]) ((fx>= c (fxmin (doomdata-numtics nb) BACKUPTICS)))
                      (let ([cmd (vector-ref (doomdata-cmds nb) c)] [o (fx+ 8 (fx* 8 c))])
                        (ticcmd-forwardmove-set! cmd (bytevector-s8-ref packet o))
                        (ticcmd-sidemove-set! cmd (bytevector-s8-ref packet (fx+ o 1)))
                        (ticcmd-angleturn-set! cmd (bytevector-s16-ref packet (fx+ o 2) (endianness big)))
                        (ticcmd-consistancy-set! cmd (bytevector-s16-ref packet (fx+ o 4) (endianness big)))
                        (ticcmd-chatchar-set! cmd (bytevector-u8-ref packet (fx+ o 6)))
                        (ticcmd-buttons-set! cmd (bytevector-u8-ref packet (fx+ o 7))))))))))))

  ;; gethostbyname's first address, as stored.
  (define (host-address name)
    (let ([h (c-gethostbyname name)])
      (when (= h 0)
        (i-error "gethostbyname: couldn't find ~a" name))
      ;; hostent: h_addr_list at offset 24 (64-bit)
      (foreign-ref 'unsigned-32 (foreign-ref 'uptr (foreign-ref 'uptr h 24) 0) 0)))

  ;; I_InitNetwork
  (define (i-init-network)
    (set! doomcom* (make-doomcom))
    ;; set up for network
    (let ([i (m-check-parm "-dup")])
      (doomcom-ticdup-set! doomcom*
                           (if (and (fx> i 0) (fx< i (fx- myargc 1)))
                               (fxmax 1 (fxmin 9 (fx- (char->integer (string-ref (vector-ref myargv (fx+ i 1)) 0))
                                                      (char->integer #\0))))
                               1)))
    (doomcom-extratics-set! doomcom* (if (fx> (m-check-parm "-extratic") 0) 1 0))
    (let ([p (m-check-parm "-port")])
      (when (and (fx> p 0) (fx< p (fx- myargc 1)))
        (set! DOOMPORT (or (string->number (vector-ref myargv (fx+ p 1))) 0))
        (printf "using alternate port ~a\n" DOOMPORT)))
    ;; parse network game options, -net <consoleplayer> <host> <host> ...
    (let ([i (m-check-parm "-net")])
      (if (fx= i 0)
          ;; single player game
          (begin
            (set! netgame #f)
            (doomcom-id-set! doomcom* DOOMCOM_ID)
            (doomcom-numplayers-set! doomcom* 1)
            (doomcom-numnodes-set! doomcom* 1)
            (doomcom-deathmatch-set! doomcom* 0)
            (doomcom-consoleplayer-set! doomcom* 0))
          (begin
            (set! netgame #t)
            ;; parse player number and host list
            (doomcom-consoleplayer-set! doomcom* (fx- (char->integer (string-ref (vector-ref myargv (fx+ i 1)) 0))
                                                     (char->integer #\1)))
            (doomcom-numnodes-set! doomcom* 1) ; this node for sure
            (let loop ([i (fx+ i 2)])
              (when (and (fx< i myargc) (not (char=? (string-ref (vector-ref myargv i) 0) #\-)))
                (let ([host (vector-ref myargv i)] [n (doomcom-numnodes doomcom*)])
                  (sockaddr! (vector-ref sendaddress n)
                             (if (char=? (string-ref host 0) #\.)
                                 (c-inet-addr (substring host 1 (string-length host)))
                                 (host-address host))
                             DOOMPORT)
                  (doomcom-numnodes-set! doomcom* (fx+ n 1))
                  (loop (fx+ i 1)))))
            (doomcom-id-set! doomcom* DOOMCOM_ID)
            (doomcom-numplayers-set! doomcom* (doomcom-numnodes doomcom*))
            ;; build message to receive
            (set! insocket (udp-socket))
            (bind-to-local-port insocket DOOMPORT)
            (set! sendsocket (udp-socket))
            ;; (bound only for tests on one machine: see net-bind-address)
            (when net-bind-address (bind-to-local-port sendsocket 0))))))

  ;; I_NetCmd
  (define (i-net-cmd)
    (cond
      [(fx= (doomcom-command doomcom*) CMD_SEND) (packet-send)]
      [(fx= (doomcom-command doomcom*) CMD_GET) (packet-get)]
      [else (i-error "Bad net cmd: ~a\n" (doomcom-command doomcom*))])))
