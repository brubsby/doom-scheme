;;; trace: the per-tic oracle trace (format: tools/oracle/oracle.c).
(library (doom trace)
  (export FNV_INIT fnv words-hash framehash mobjhash sectorhash trace-fields
          trace-field-names)
  (import (chezscheme) (doom doomdef) (doom doomstat) (doom v_video) (doom m_random)
          (doom d_think) (doom d_player) (doom p_mobj.h) (doom info) (doom r_defs)
          (doom r_state) (doom p_local))

  ;; FNV-1a over 32-bit words: h ^= (uint32)w; h *= 16777619 (mod 2^32).
  (define FNV_INIT 2166136261)
  (define (fnv h w)
    (fxand (fx* (fxxor h (fxand w #xFFFFFFFF)) 16777619) #xFFFFFFFF))

  ;; Hash nbytes (a multiple of 4) of bv from offset as little-endian words.
  (define (words-hash bv offset nbytes)
    (let loop ([i 0] [h FNV_INIT])
      (if (fx= i nbytes)
          h
          (loop (fx+ i 4) (fnv h (bytevector-u32-ref bv (fx+ offset i) (endianness little)))))))

  (define (framehash)
    (words-hash screens 0 (fx* SCREENWIDTH SCREENHEIGHT)))

  ;; For each mobj in thinker-list order: x y z angle momx momy momz type
  ;; (state-states) tics health flags. Returns (values count hash).
  (define (mobjhash)
    (let loop ([th (thinker-next thinkercap)] [n 0] [h FNV_INIT])
      (cond
        [(eq? th thinkercap) (values n h)]
        [(eq? (thinker-func th) p-mobj-thinker)
         (loop (thinker-next th) (fx+ n 1)
               (fold-left fnv h
                          (list (mobj-x th) (mobj-y th) (mobj-z th) (mobj-angle th)
                                (mobj-momx th) (mobj-momy th) (mobj-momz th) (mobj-type th)
                                (state-index (mobj-state th)) (mobj-tics th)
                                (mobj-health th) (mobj-flags th))))]
        [else (loop (thinker-next th) n h)])))

  ;; For each sector: floorheight ceilingheight lightlevel special.
  (define (sectorhash)
    (let loop ([i 0] [h FNV_INIT])
      (if (fx= i numsectors)
          h
          (let ([s (vector-ref sectors i)])
            (loop (fx+ i 1)
                  (fold-left fnv h (list (sector-floorheight s) (sector-ceilingheight s)
                                         (sector-lightlevel s) (sector-special s))))))))

  (define trace-field-names
    '(gametic gamestate leveltime prndindex rndindex px py pz pangle pmomx pmomy
      health armor nmobjs mobjhash sectorhash framehash palettehash))

  ;; One trace line's fields, as numbers. framehash and palettehash are
  ;; supplied by the caller (#f when nothing was drawn).
  (define (trace-fields framehash palettehash)
    (let ([p (vector-ref players 0)])
      (let-values ([(nmobjs mh) (if (fx= gamestate GS_LEVEL) (mobjhash) (values 0 FNV_INIT))])
        (let ([mo (player-mo p)])
          (list gametic gamestate leveltime prndindex rndindex
                (if mo (mobj-x mo) 0) (if mo (mobj-y mo) 0) (if mo (mobj-z mo) 0)
                (if mo (mobj-angle mo) 0) (if mo (mobj-momx mo) 0) (if mo (mobj-momy mo) 0)
                (player-health p) (player-armorpoints p)
                nmobjs mh (if (fx= gamestate GS_LEVEL) (sectorhash) FNV_INIT)
                framehash palettehash))))))
