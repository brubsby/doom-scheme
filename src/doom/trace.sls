;;; trace: the per-tic oracle trace (format: tools/oracle/oracle.c).
(library (doom trace)
  (export FNV_INIT fnv words-hash framehash)
  (import (chezscheme) (doom doomdef) (doom v_video))

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
    (words-hash screens 0 (fx* SCREENWIDTH SCREENHEIGHT))))
