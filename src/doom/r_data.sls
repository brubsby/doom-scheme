;;; r_data: textures, flats, sprite lumps and colormaps: the renderer's
;;; data, built at startup from the WAD.
;;;
;;; Pointers into lumps become (bytevector, offset) pairs: R_GetColumn
;;; returns both as two values. A composite texture is one bytevector with
;;; its multi-patch columns laid out back to back, as in C, so reads that run
;;; off one column into the next (short textures drawn with &127) see the
;;; same bytes. Composites are zero-filled where C's Z_Malloc leaves
;;; whatever was in memory.
(library (doom r_data)
  (export texture make-texture texture? texture-name texture-width texture-height
          texture-patches texpatch make-texpatch texpatch? texpatch-originx
          texpatch-originy texpatch-patch
          numtextures textures texturewidthmask textureheight texturetranslation
          texturecolumnlump texturecolumnofs texturecomposite texturecompositesize
          firstflat lastflat numflats flattranslation
          firstspritelump lastspritelump numspritelumps
          spritewidth spriteoffset spritetopoffset colormaps
          r-init-data r-init-textures r-init-flats r-init-sprite-lumps r-init-colormaps
          r-flat-num-for-name r-check-texture-num-for-name r-texture-num-for-name
          r-get-column r-generate-composite r-precache-level)
  (import (chezscheme) (doom global) (doom struct) (doom w_wad) (doom m_fixed)
          (doom i_system))

  ;; texpatch_t: a patch placed in a texture. patch is a lump number.
  (define-struct texpatch () (originx 0) (originy 0) (patch 0))
  ;; texture_t. patches is a vector of texpatch.
  (define-struct texture () (name "") (width 0) (height 0) (patches (vector)))

  (define-global numtextures 0)
  (define-global textures (vector))
  (define-global texturewidthmask (fxvector))
  (define-global textureheight (fxvector))   ; fixed_t
  (define-global texturetranslation (fxvector))

  ;; Per texture: for each column, the patch lump that alone covers it (or
  ;; -1 for a composite column) and the offset of the column's pixels in
  ;; that lump (or in the composite).
  (define-global texturecolumnlump (vector))   ; fxvectors
  (define-global texturecolumnofs (vector))    ; fxvectors
  (define-global texturecomposite (vector))    ; bytevector or #f (not built yet)
  (define-global texturecompositesize (fxvector))

  (define-global firstflat 0)
  (define-global lastflat 0)
  (define-global numflats 0)
  (define-global flattranslation (fxvector))

  (define-global firstspritelump 0)
  (define-global lastspritelump 0)
  (define-global numspritelumps 0)
  ;; fixed_t, per sprite lump (relative to firstspritelump).
  (define-global spritewidth (fxvector))
  (define-global spriteoffset (fxvector))
  (define-global spritetopoffset (fxvector))

  ;; The COLORMAP lump: 34 light tables of 256 bytes. lighttable_t pointers
  ;; are offsets into it.
  (define-global colormaps (bytevector))

  (define (s16 bv o) (bytevector-s16-ref bv o (endianness little)))
  (define (s32 bv o) (bytevector-s32-ref bv o (endianness little)))
  (define (name8 bv o)
    (let loop ([i 0])
      (if (or (fx= i 8) (fx= (bytevector-u8-ref bv (fx+ o i)) 0))
          (let ([s (make-string i)])
            (do ([j 0 (fx+ j 1)]) ((fx= j i) s)
              (string-set! s j (integer->char (bytevector-u8-ref bv (fx+ o j))))))
          (loop (fx+ i 1)))))

  ;; R_InitTextures. maptexture_t: name[8], masked (int), width, height
  ;; (short), columndirectory (4 bytes, obsolete), patchcount (short), then
  ;; mappatch_t[patchcount] of 10 bytes: originx, originy, patch, stepdir,
  ;; colormap.
  (define (r-init-textures)
    (let* ([names (w-cache-lump-name "PNAMES")]
           [nummappatches (s32 names 0)]
           [patchlookup (make-fxvector nummappatches)])
      (do ([i 0 (fx+ i 1)]) ((fx= i nummappatches))
        (fxvector-set! patchlookup i (w-check-num-for-name (name8 names (fx+ 4 (fx* i 8))))))
      (let* ([maptex1 (w-cache-lump-name "TEXTURE1")]
             [numtextures1 (s32 maptex1 0)]
             [maxoff1 (w-lump-length (w-get-num-for-name "TEXTURE1"))]
             [maptex2 (and (not (= (w-check-num-for-name "TEXTURE2") -1))
                           (w-cache-lump-name "TEXTURE2"))]
             [numtextures2 (if maptex2 (s32 maptex2 0) 0)]
             [maxoff2 (if maptex2 (w-lump-length (w-get-num-for-name "TEXTURE2")) 0)]
             [n (fx+ numtextures1 numtextures2)])
        (set! numtextures n)
        (set! textures (make-vector n #f))
        (set! texturewidthmask (make-fxvector n 0))
        (set! texturecolumnlump (make-vector n #f))
        (set! texturecolumnofs (make-vector n #f))
        (set! texturecomposite (make-vector n #f))
        (set! texturecompositesize (make-fxvector n 0))
        (set! textureheight (make-fxvector n 0))
        (do ([i 0 (fx+ i 1)]) ((fx= i n))
          (let*-values ([(maptex maxoff dir)
                         (if (fx< i numtextures1)
                             (values maptex1 maxoff1 i)
                             (values maptex2 maxoff2 (fx- i numtextures1)))])
            (let ([offset (s32 maptex (fx+ 4 (fx* dir 4)))])
              (when (fx> offset maxoff)
                (i-error "R_InitTextures: bad texture directory"))
              (let* ([tname (name8 maptex offset)]
                     [width (s16 maptex (fx+ offset 12))]
                     [height (s16 maptex (fx+ offset 14))]
                     [patchcount (s16 maptex (fx+ offset 20))]
                     [patches (make-vector patchcount)])
                (do ([j 0 (fx+ j 1)]) ((fx= j patchcount))
                  (let* ([mp (fx+ offset 22 (fx* j 10))]
                         [lump (fxvector-ref patchlookup (s16 maptex (fx+ mp 4)))])
                    (when (fx= lump -1)
                      (i-error "R_InitTextures: Missing patch in texture ~a" tname))
                    (vector-set! patches j
                                 (make-texpatch (s16 maptex mp) (s16 maptex (fx+ mp 2)) lump))))
                (vector-set! textures i (make-texture tname width height patches))
                ;; C: Z_Malloc'd, so columns R_GenerateLookup skips (after a
                ;; patchless column) hold garbage; ours hold 0.
                (vector-set! texturecolumnlump i (make-fxvector width 0))
                (vector-set! texturecolumnofs i (make-fxvector width 0))
                (fxvector-set! texturewidthmask i
                               (let loop ([j 1])
                                 (if (fx<= (fx* j 2) width) (loop (fxsll j 1)) (fx- j 1))))
                (fxvector-set! textureheight i (fxsll height FRACBITS)))))))
      ;; Precalculate whatever possible.
      (do ([i 0 (fx+ i 1)]) ((fx= i numtextures))
        (r-generate-lookup i))
      (set! texturetranslation (make-fxvector (fx+ numtextures 1) 0))
      (do ([i 0 (fx+ i 1)]) ((fx= i numtextures))
        (fxvector-set! texturetranslation i i))))

  (define (r-init-flats)
    (set! firstflat (fx+ (w-get-num-for-name "F_START") 1))
    (set! lastflat (fx- (w-get-num-for-name "F_END") 1))
    (set! numflats (fx+ (fx- lastflat firstflat) 1))
    (set! flattranslation (make-fxvector (fx+ numflats 1) 0))
    (do ([i 0 (fx+ i 1)]) ((fx= i numflats))
      (fxvector-set! flattranslation i i)))

  ;; Any lump with that name counts, not just those between F_START and F_END.
  (define (r-flat-num-for-name name)
    (let ([i (w-check-num-for-name name)])
      (when (fx= i -1) (i-error "R_FlatNumForName: ~a not found" name))
      (fx- i firstflat)))

  ;; "-" (anything starting with '-') is the no-texture marker, 0. Otherwise
  ;; the first texture whose name matches, ignoring case.
  (define (r-check-texture-num-for-name name)
    (if (and (fx> (string-length name) 0) (char=? (string-ref name 0) #\-))
        0
        (let loop ([i 0])
          (cond
            [(fx= i numtextures) -1]
            [(string-ci=? (texture-name (vector-ref textures i)) name) i]
            [else (loop (fx+ i 1))]))))

  (define (r-texture-num-for-name name)
    (let ([i (r-check-texture-num-for-name name)])
      (when (fx= i -1) (i-error "R_TextureNumForName: ~a not found" name))
      i))

  (define (patch-width p) (s16 p 0))
  (define (patch-columnofs p col) (s32 p (fx+ 8 (fx* col 4))))

  ;; R_DrawColumnInCache: clip and draw a column from a patch into a
  ;; cached post. `col` is the column's offset in `patch`, `cache` the
  ;; column's offset in `block`.
  (define (r-draw-column-in-cache patch col block cache originy cacheheight)
    (let loop ([col col])
      (let ([topdelta (bytevector-u8-ref patch col)])
        (unless (fx= topdelta #xff)
          (let* ([len (bytevector-u8-ref patch (fx+ col 1))]
                 [position (fx+ originy topdelta)]
                 [count (if (fx< position 0) (fx+ len position) len)]
                 [position (if (fx< position 0) 0 position)]
                 [count (if (fx> (fx+ position count) cacheheight)
                            (fx- cacheheight position)
                            count)])
            (when (fx> count 0)
              (bytevector-copy! patch (fx+ col 3) block (fx+ cache position) count))
            (loop (fx+ col len 4)))))))

  ;; R_GenerateComposite: build the composite columns of a texture.
  (define (r-generate-composite texnum)
    (let* ([texture (vector-ref textures texnum)]
           [block (make-bytevector (fxvector-ref texturecompositesize texnum) 0)]
           [collump (vector-ref texturecolumnlump texnum)]
           [colofs (vector-ref texturecolumnofs texnum)]
           [width (texture-width texture)])
      (vector-for-each
       (lambda (patch)
         (let* ([realpatch (w-cache-lump-num (texpatch-patch patch))]
                [x1 (texpatch-originx patch)]
                [x2 (fxmin (fx+ x1 (patch-width realpatch)) width)])
           (do ([x (fxmax x1 0) (fx+ x 1)]) ((fx>= x x2))
             ;; Column does not have multiple patches?
             (when (fx< (fxvector-ref collump x) 0)
               (r-draw-column-in-cache realpatch (patch-columnofs realpatch (fx- x x1))
                                       block (fxvector-ref colofs x)
                                       (texpatch-originy patch) (texture-height texture))))))
       (texture-patches texture))
      (vector-set! texturecomposite texnum block)))

  ;; R_GenerateLookup
  (define (r-generate-lookup texnum)
    (let* ([texture (vector-ref textures texnum)]
           [width (texture-width texture)]
           [collump (vector-ref texturecolumnlump texnum)]
           [colofs (vector-ref texturecolumnofs texnum)]
           [patchcount (make-fxvector width 0)])
      ;; Composited texture not created yet.
      (vector-set! texturecomposite texnum #f)
      (fxvector-set! texturecompositesize texnum 0)
      ;; Count the patches covering each column. Columns with only a
      ;; single patch are done here.
      (vector-for-each
       (lambda (patch)
         (let* ([realpatch (w-cache-lump-num (texpatch-patch patch))]
                [x1 (texpatch-originx patch)]
                [x2 (fxmin (fx+ x1 (patch-width realpatch)) width)])
           (do ([x (fxmax x1 0) (fx+ x 1)]) ((fx>= x x2))
             (fxvector-set! patchcount x (fx+ (fxvector-ref patchcount x) 1))
             (fxvector-set! collump x (texpatch-patch patch))
             (fxvector-set! colofs x (fx+ (patch-columnofs realpatch (fx- x x1)) 3)))))
       (texture-patches texture))
      (let loop ([x 0])
        (when (fx< x width)
          (cond
            [(fx= (fxvector-ref patchcount x) 0)
             ;; Note: gives up on the rest of the texture.
             (printf "R_GenerateLookup: column without a patch (~a)\n" (texture-name texture))]
            [else
             (when (fx> (fxvector-ref patchcount x) 1)
               ;; Use the cached block.
               (fxvector-set! collump x -1)
               (fxvector-set! colofs x (fxvector-ref texturecompositesize texnum))
               (when (fx> (fxvector-ref texturecompositesize texnum)
                          (fx- #x10000 (texture-height texture)))
                 (i-error "R_GenerateLookup: texture ~a is >64k" texnum))
               (fxvector-set! texturecompositesize texnum
                              (fx+ (fxvector-ref texturecompositesize texnum)
                                   (texture-height texture))))
             (loop (fx+ x 1))])))))

  ;; R_GetColumn: a texture column's pixels, as (values bytevector offset).
  (define (r-get-column tex col)
    (let* ([col (fxand col (fxvector-ref texturewidthmask tex))]
           [lump (fxvector-ref (vector-ref texturecolumnlump tex) col)]
           [ofs (fxvector-ref (vector-ref texturecolumnofs tex) col)])
      (if (fx> lump 0)
          (values (w-cache-lump-num lump) ofs)
          (begin
            (unless (vector-ref texturecomposite tex)
              (r-generate-composite tex))
            (values (vector-ref texturecomposite tex) ofs)))))

  ;; R_InitSpriteLumps: the patch header fields the renderer needs.
  (define (r-init-sprite-lumps)
    (set! firstspritelump (fx+ (w-get-num-for-name "S_START") 1))
    (set! lastspritelump (fx- (w-get-num-for-name "S_END") 1))
    (set! numspritelumps (fx+ (fx- lastspritelump firstspritelump) 1))
    (set! spritewidth (make-fxvector numspritelumps 0))
    (set! spriteoffset (make-fxvector numspritelumps 0))
    (set! spritetopoffset (make-fxvector numspritelumps 0))
    (do ([i 0 (fx+ i 1)]) ((fx= i numspritelumps))
      (let ([patch (w-cache-lump-num (fx+ firstspritelump i))])
        (fxvector-set! spritewidth i (fxsll (s16 patch 0) FRACBITS))
        (fxvector-set! spriteoffset i (fxsll (s16 patch 4) FRACBITS))
        (fxvector-set! spritetopoffset i (fxsll (s16 patch 6) FRACBITS)))))

  (define (r-init-colormaps)
    (set! colormaps (w-read-lump (w-get-num-for-name "COLORMAP"))))

  ;; R_InitData: locates all the lumps that will be used by all views.
  (define (r-init-data)
    (r-init-textures)
    (r-init-flats)
    (r-init-sprite-lumps)
    (r-init-colormaps))

  ;; R_PrecacheLevel only pulls lumps into the zone cache, which has no
  ;; visible effect here.
  (define (r-precache-level) (void)))
