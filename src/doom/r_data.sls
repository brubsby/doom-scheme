;;; r_data: texture and flat directories.
;;;
;;; Milestone 2 has the name-to-number lookups p_setup needs (R_InitTextures
;;; without the column lookup tables, R_InitFlats). Composites, colormaps and
;;; sprite lumps arrive with the renderer.
(library (doom r_data)
  (export texture make-texture texture? texture-name texture-width texture-height
          texture-patches texpatch make-texpatch texpatch? texpatch-originx
          texpatch-originy texpatch-patch
          numtextures textures texturewidthmask textureheight texturetranslation
          firstflat lastflat numflats flattranslation
          r-init-textures r-init-flats r-flat-num-for-name
          r-check-texture-num-for-name r-texture-num-for-name)
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

  (define-global firstflat 0)
  (define-global lastflat 0)
  (define-global numflats 0)
  (define-global flattranslation (fxvector))

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
                (fxvector-set! texturewidthmask i
                               (let loop ([j 1])
                                 (if (fx<= (fx* j 2) width) (loop (fxsll j 1)) (fx- j 1))))
                (fxvector-set! textureheight i (fxsll height FRACBITS)))))))
      ;; TODO(milestone 3): R_GenerateLookup for each texture.
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
      i)))
