;;; g_game: game flow: new games, level loading, demo playback, and the
;;; per-tic game driver.
;;;
;;; Status (milestone 4): G_InitNew, G_DoLoadLevel, G_DoReborn (single
;;; player), demo playback, and G_Ticker's level path. Input, demo
;;; recording, saves, intermissions and netgames are milestone 6.
(library (doom g_game)
  (export netcmds demobuffer demo-p demo-ended
          g-init-new g-do-load-level g-do-reborn g-ticker
          g-play-demo-bytes g-do-play-demo g-read-demo-ticcmd
          DEMOMARKER)
  (import (chezscheme) (doom global) (doom doomdef) (doom doomstat) (doom d_event)
          (doom d_ticcmd) (doom d_player) (doom info) (doom m_fixed) (doom m_random)
          (doom w_wad) (doom r_data) (doom r_local) (doom p_local) (doom am_map)
          (doom s_sound) (doom struct) (doom i_system))

  ;; The commands for the current tic (C: netcmds[player][buf], filled by
  ;; G_BuildTiccmd and the network code).
  (define netcmds (vector (make-ticcmd) (make-ticcmd) (make-ticcmd) (make-ticcmd)))

  ;; G_InitNew. Can be called by the startup code or the menu task.
  (define (g-init-new skill episode map)
    (when paused
      (set! paused #f)
      (s-resume-sound))
    (let* ([skill (if (fx> skill sk_nightmare) sk_nightmare skill)]
           ;; This was quite messy with SPECIAL and commented parts.
           ;; Supposedly hacks to make the latest edition work. It might not
           ;; work properly.
           [episode (if (fx< episode 1) 1 episode)]
           [episode (cond [(fx= gamemode retail) (fxmin episode 4)]
                          [(fx= gamemode shareware) (fxmin episode 1)] ; only episode 1 on shareware
                          [else (fxmin episode 3)])]
           [map (if (fx< map 1) 1 map)]
           [map (if (and (fx> map 9) (not (fx= gamemode commercial))) 9 map)])
      (m-clear-random)
      (set! respawnmonsters (or (fx= skill sk_nightmare) respawnparm))
      (cond
        [(or fastparm (and (fx= skill sk_nightmare) (not (fx= gameskill sk_nightmare))))
         (do ([i S_SARG_RUN1 (fx+ i 1)]) ((fx> i S_SARG_PAIN2))
           (let ([st (vector-ref states i)])
             (state-tics-set! st (fxsra (state-tics st) 1))))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_BRUISERSHOT) (* 20 FRACUNIT))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_HEADSHOT) (* 20 FRACUNIT))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_TROOPSHOT) (* 20 FRACUNIT))]
        [(and (not (fx= skill sk_nightmare)) (fx= gameskill sk_nightmare))
         (do ([i S_SARG_RUN1 (fx+ i 1)]) ((fx> i S_SARG_PAIN2))
           (let ([st (vector-ref states i)])
             (state-tics-set! st (fxsll (state-tics st) 1))))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_BRUISERSHOT) (* 15 FRACUNIT))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_HEADSHOT) (* 10 FRACUNIT))
         (mobjinfo-speed-set! (vector-ref mobjinfo-table MT_TROOPSHOT) (* 10 FRACUNIT))])
      ;; force players to be initialized upon first level load
      (vector-for-each (lambda (p) (player-playerstate-set! p PST_REBORN)) players)
      (set! usergame #t)                ; will be set false if a demo
      (set! paused #f)
      (set! demoplayback #f)
      (set! automapactive #f)
      (set! viewactive #t)
      (set! gameepisode episode)
      (set! gamemap map)
      (set! gameskill skill)
      ;; set the sky map for the episode
      (set! skytexture
            (if (fx= gamemode commercial)
                (r-texture-num-for-name (cond [(fx< gamemap 12) "SKY1"] [(fx< gamemap 21) "SKY2"] [else "SKY3"]))
                (r-texture-num-for-name (case episode [(1) "SKY1"] [(2) "SKY2"] [(3) "SKY3"] [else "SKY4"]))))
      (g-do-load-level)))

  ;; G_DoLoadLevel
  (define (g-do-load-level)
    ;; Set the sky map. First thing, we have a dummy sky texture name, a
    ;; flat. The data is in the WAD only because we look for an actual
    ;; index, instead of simply setting one.
    (set! skyflatnum (r-flat-num-for-name SKYFLATNAME))
    ;; DOOM determines the sky texture to be used depending on the current
    ;; episode, and the game version.
    (when (fx= gamemode commercial)
      (set! skytexture (r-texture-num-for-name
                        (cond [(fx< gamemap 12) "SKY1"] [(fx< gamemap 21) "SKY2"] [else "SKY3"]))))
    (set! levelstarttic gametic)        ; for time calculation
    ;; TODO(milestone 7): force a wipe (wipegamestate).
    (set! gamestate GS_LEVEL)
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (let ([p (vector-ref players i)])
        (when (and (vector-ref playeringame i) (fx= (player-playerstate p) PST_DEAD))
          (player-playerstate-set! p PST_REBORN))
        (fxvector-fill! (player-frags p) 0)))
    (p-setup-level gameepisode gamemap 0 gameskill)
    (set! displayplayer consoleplayer)  ; view the guy you are playing
    (set! gameaction ga_nothing)
    ;; TODO(milestone 6): clear cmd building stuff (gamekeydown, mouse,
    ;; joystick, sendpause, sendsave).
    (set! paused #f))

  ;; G_DoReborn
  (define (g-do-reborn playernum)
    (if (not netgame)
        ;; reload the level from scratch
        (set! gameaction ga_loadlevel)
        (i-error "G_DoReborn: netgames are not ported yet")))

  ;; DEMO PLAYBACK
  (define DEMOMARKER #x80)
  (define VERSION 110)
  (define-global demobuffer (bytevector))
  (define-global demo-p 0)
  ;; Set when playback reaches the end marker. (C's G_CheckDemoStatus ends
  ;; the game or a timedemo there; the caller decides here.)
  (define-global demo-ended #f)

  ;; G_DoPlayDemo, from the demo's bytes (C reads the lump defdemoname).
  ;; Like the oracle, this accepts version 109 demos (the shareware's own)
  ;; as well as 110.
  (define (g-play-demo-bytes bv)
    (set! demobuffer bv)
    (g-do-play-demo))

  (define (g-do-play-demo)
    (set! gameaction ga_nothing)
    (set! demo-p 0)
    (set! demo-ended #f)
    (let ([version (next-demo-byte)])
      (if (not (memv version (list VERSION 109)))
          (begin
            (display "Demo is from a different game version!\n" (current-error-port))
            (set! gameaction ga_nothing))
          (let* ([skill (next-demo-byte)]
                 [episode (next-demo-byte)]
                 [map (next-demo-byte)])
            (set! deathmatch (next-demo-byte))
            (set! respawnparm (not (fx= (next-demo-byte) 0)))
            (set! fastparm (not (fx= (next-demo-byte) 0)))
            (set! nomonsters (not (fx= (next-demo-byte) 0)))
            (set! consoleplayer (next-demo-byte))
            (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
              (vector-set! playeringame i (not (fx= (next-demo-byte) 0))))
            (when (vector-ref playeringame 1)
              (i-error "G_DoPlayDemo: netgame demos are not ported yet"))
            ;; don't spend a lot of time in loadlevel
            (set! precache #f)
            (g-init-new skill episode map)
            (set! precache #t)
            (set! usergame #f)
            (set! demoplayback #t)))))

  (define (next-demo-byte)
    (let ([b (bytevector-u8-ref demobuffer demo-p)])
      (set! demo-p (fx+ demo-p 1))
      b))

  (define (s8 b) (if (fx> b 127) (fx- b 256) b))
  (define (s16 x) (fx- (fxand (fx+ x #x8000) #xFFFF) #x8000))

  ;; G_ReadDemoTiccmd
  (define (g-read-demo-ticcmd cmd)
    (if (fx= (bytevector-u8-ref demobuffer demo-p) DEMOMARKER)
        ;; end of demo data stream
        (set! demo-ended #t)
        (begin
          (ticcmd-forwardmove-set! cmd (s8 (next-demo-byte)))
          (ticcmd-sidemove-set! cmd (s8 (next-demo-byte)))
          ;; (angleturn is a short)
          (ticcmd-angleturn-set! cmd (s16 (fxsll (next-demo-byte) 8)))
          (ticcmd-buttons-set! cmd (next-demo-byte)))))

  ;; G_Ticker: make ticcmd_ts for the players. Returns #f if demo playback
  ;; ended this tic (C exits a timedemo there, before running the tic).
  (define (g-ticker)
    ;; do player reborns if needed
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (and (vector-ref playeringame i)
                 (fx= (player-playerstate (vector-ref players i)) PST_REBORN))
        (g-do-reborn i)))
    ;; do things to change the game state
    (let loop ()
      (unless (fx= gameaction ga_nothing)
        (cond
          [(fx= gameaction ga_loadlevel) (g-do-load-level)]
          [(fx= gameaction ga_playdemo) (g-do-play-demo)]
          [else
           ;; TODO(milestone 6): ga_completed (G_DoCompleted), ga_worlddone,
           ;; ga_victory, ga_newgame, loading and saving.
           (unported (vector-ref '#(ga_nothing ga_loadlevel ga_newgame ga_loadgame ga_savegame
                                    ga_playdemo ga_completed ga_victory ga_worlddone ga_screenshot)
                                 gameaction))
           (set! gameaction ga_nothing)])
        (loop)))
    ;; get commands
    (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
      (when (vector-ref playeringame i)
        (let ([cmd (player-cmd (vector-ref players i))])
          (struct-copy! cmd (vector-ref netcmds i))
          (when demoplayback
            (g-read-demo-ticcmd cmd)))))
    (if demo-ended
        #f
        (begin
          ;; check for special buttons
          (do ([i 0 (fx+ i 1)]) ((fx= i MAXPLAYERS))
            (when (vector-ref playeringame i)
              (let ([buttons (ticcmd-buttons (player-cmd (vector-ref players i)))])
                (when (fxlogtest buttons BT_SPECIAL)
                  (cond
                    [(fx= (fxand buttons BT_SPECIALMASK) BTS_PAUSE)
                     (set! paused (not paused))
                     (if paused (s-pause-sound) (s-resume-sound))]
                    [(fx= (fxand buttons BT_SPECIALMASK) BTS_SAVEGAME)
                     (i-error "G_Ticker: saving is not ported yet")])))))
          ;; do main actions
          (cond
            [(fx= gamestate GS_LEVEL)
             (p-ticker)
             ;; TODO(milestone 7): ST_Ticker, HU_Ticker.
             (am-ticker)]
            [else (i-error "G_Ticker: gamestate ~a is not ported yet" gamestate)])
          #t))))
