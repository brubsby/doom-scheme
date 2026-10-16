;;; w_wad + v_video against the oracle: TITLEPIC drawn by V_DrawPatch must be
;;; byte-identical to linuxdoom's first title-screen frame.
(import (chezscheme) (doom w_wad) (doom v_video) (doom doomdef) (doom trace))
(load "tests/lib.ss")

(w-init-multiple-files (list wad-path))
(check "numlumps (doom1.wad v1.9)" 1264 (w-num-lumps))
(check "PLAYPAL is 14 palettes" (* 14 768) (w-lump-length (w-get-num-for-name "PLAYPAL")))
(check "lookup is case-insensitive" (w-get-num-for-name "TITLEPIC") (w-check-num-for-name "titlepic"))
(check "missing lump" -1 (w-check-num-for-name "NOSUCH"))
(check "cache returns the same bytevector"
       #t (eq? (w-cache-lump-name "STBAR") (w-cache-lump-name "STBAR")))

(v-init)
(v-draw-patch 0 0 0 (w-cache-lump-name "TITLEPIC"))
;; build/oracle/title.trace: the oracle's first frame, run with no arguments.
;; Field 17 (1-based) of a trace line is framehash.
(define oracle-title-framehash
  (let ([f (with-input-from-string
            (string-append "(" (call-with-input-file "build/oracle/title.trace" get-line) ")")
            read)])
    (list-ref f 16)))
(check "TITLEPIC framehash matches oracle" oracle-title-framehash (framehash))

;; Status bar, flipped face, block copy round trip.
(v-draw-patch 0 168 0 (w-cache-lump-name "STBAR"))
(v-draw-patch-flipped 143 168 0 (w-cache-lump-name "STFST00"))
(let ([blk (make-bytevector (* 40 30))])
  (v-get-block 10 10 0 40 30 blk)
  (v-draw-block 200 100 1 40 30 blk)
  (v-copy-rect 200 100 1 40 30 10 10 0)
  (let ([again (make-bytevector (* 40 30))])
    (v-get-block 10 10 0 40 30 again)
    (check "get/draw/copy block round trip" blk again)))
(finish)
