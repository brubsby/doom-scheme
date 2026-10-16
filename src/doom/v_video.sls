;;; v_video: the screen buffers and 2D drawing (patches, blocks, rects).
;;;
;;; screens[0..4] live in ONE bytevector, `screens`, at offsets
;;; (screen-base n) = n*SCREENWIDTH*SCREENHEIGHT. In C, screens[0..3] are
;;; carved contiguously out of one allocation, and some code relies on that
;;; (G_DoSaveGame writes its buffer at screens[1]+0x4000 and can run on into
;;; screens[2]). screens[4], the status bar background, is a separate
;;; Z_Malloc in C; here it is simply the fifth slot.
(library (doom v_video)
  (export screens screen-base gammatable usegamma dirtybox
          v-init v-mark-rect v-copy-rect v-draw-patch v-draw-patch-flipped
          v-draw-patch-direct v-draw-block v-get-block
          patch-width patch-height patch-leftoffset patch-topoffset)
  (import (chezscheme) (doom global) (doom doomdef) (doom i_system)
          (doom m_bbox) (doom v_video_data))

  (define screensize (fx* SCREENWIDTH SCREENHEIGHT))
  (define screens (make-bytevector (fx* 5 screensize) 0))
  (define (screen-base n) (fx* n screensize))

  (define-global usegamma 0)
  (define dirtybox (make-box))

  ;; patch_t header: width, height, leftoffset, topoffset (all int16), then
  ;; int32 columnofs[width] relative to the start of the patch.
  (define (patch-width p)      (bytevector-s16-ref p 0 (endianness little)))
  (define (patch-height p)     (bytevector-s16-ref p 2 (endianness little)))
  (define (patch-leftoffset p) (bytevector-s16-ref p 4 (endianness little)))
  (define (patch-topoffset p)  (bytevector-s16-ref p 6 (endianness little)))
  (define (patch-columnofs p col)
    (bytevector-s32-ref p (fx+ 8 (fx* col 4)) (endianness little)))

  (define (v-init)
    (bytevector-fill! screens 0))

  (define (v-mark-rect x y width height)
    (m-add-to-box dirtybox x y)
    (m-add-to-box dirtybox (fx- (fx+ x width) 1) (fx- (fx+ y height) 1)))

  (define (check-rect who x y width height scrn)
    (when (or (fx< x 0) (fx> (fx+ x width) SCREENWIDTH)
              (fx< y 0) (fx> (fx+ y height) SCREENHEIGHT)
              (not (fx<= 0 scrn 4)))
      (i-error "Bad ~a" who)))

  (define (v-copy-rect srcx srcy srcscrn width height destx desty destscrn)
    (check-rect "V_CopyRect" srcx srcy width height srcscrn)
    (check-rect "V_CopyRect" destx desty width height destscrn)
    (v-mark-rect destx desty width height)
    (let loop ([h height]
               [src (fx+ (screen-base srcscrn) (fx* SCREENWIDTH srcy) srcx)]
               [dest (fx+ (screen-base destscrn) (fx* SCREENWIDTH desty) destx)])
      (when (fx> h 0)
        (bytevector-copy! screens src screens dest width)
        (loop (fx- h 1) (fx+ src SCREENWIDTH) (fx+ dest SCREENWIDTH)))))

  ;; Draw columns of `patch` at desttop+col; column-of maps col -> source column.
  ;; A column is a list of posts: topdelta (0xff ends), length, pad,
  ;; length bytes of pixels, pad.
  (define (draw-columns patch desttop w column-of)
    (do ([col 0 (fx+ col 1)]) ((fx= col w))
      (let post ([c (patch-columnofs patch (column-of col))])
        (let ([topdelta (bytevector-u8-ref patch c)])
          (unless (fx= topdelta #xff)
            (let ([len (bytevector-u8-ref patch (fx+ c 1))])
              (let px ([i 0]
                       [src (fx+ c 3)]
                       [dest (fx+ desttop col (fx* topdelta SCREENWIDTH))])
                (when (fx< i len)
                  (bytevector-u8-set! screens dest (bytevector-u8-ref patch src))
                  (px (fx+ i 1) (fx+ src 1) (fx+ dest SCREENWIDTH))))
              (post (fx+ c len 4))))))))

  ;; V_DrawPatch: masked draw. Out-of-bounds patches are reported and skipped
  ;; (RANGECHECK is on in linuxdoom; "what is up with TNT.WAD?").
  (define (v-draw-patch x y scrn patch)
    (let ([y (fx- y (patch-topoffset patch))]
          [x (fx- x (patch-leftoffset patch))]
          [w (patch-width patch)])
      (if (or (fx< x 0) (fx> (fx+ x w) SCREENWIDTH)
              (fx< y 0) (fx> (fx+ y (patch-height patch)) SCREENHEIGHT)
              (not (fx<= 0 scrn 4)))
          (begin
            (fprintf (current-error-port) "Patch at ~a,~a exceeds LFB\n" x y)
            (fprintf (current-error-port) "V_DrawPatch: bad patch (ignored)\n"))
          (begin
            (when (fx= scrn 0) (v-mark-rect x y w (patch-height patch)))
            (draw-columns patch (fx+ (screen-base scrn) (fx* y SCREENWIDTH) x) w
                          (lambda (col) col))))))

  ;; V_DrawPatchFlipped: mirrored horizontally (e.g. the status bar face).
  (define (v-draw-patch-flipped x y scrn patch)
    (let ([y (fx- y (patch-topoffset patch))]
          [x (fx- x (patch-leftoffset patch))]
          [w (patch-width patch)])
      (when (or (fx< x 0) (fx> (fx+ x w) SCREENWIDTH)
                (fx< y 0) (fx> (fx+ y (patch-height patch)) SCREENHEIGHT)
                (not (fx<= 0 scrn 4)))
        (fprintf (current-error-port) "Patch origin ~a,~a exceeds LFB\n" x y)
        (i-error "Bad V_DrawPatch in V_DrawPatchFlipped"))
      (when (fx= scrn 0) (v-mark-rect x y w (patch-height patch)))
      (draw-columns patch (fx+ (screen-base scrn) (fx* y SCREENWIDTH) x) w
                    (lambda (col) (fx- w 1 col)))))

  ;; On Linux V_DrawPatchDirect is just V_DrawPatch.
  (define (v-draw-patch-direct x y scrn patch)
    (v-draw-patch x y scrn patch))

  ;; V_DrawBlock: copy a linear width*height block from src (at src-offset).
  (define v-draw-block
    (case-lambda
      [(x y scrn width height src) (v-draw-block x y scrn width height src 0)]
      [(x y scrn width height src src-offset)
       (check-rect "V_DrawBlock" x y width height scrn)
       (v-mark-rect x y width height)
       (let loop ([h height] [s src-offset]
                  [dest (fx+ (screen-base scrn) (fx* y SCREENWIDTH) x)])
         (when (fx> h 0)
           (bytevector-copy! src s screens dest width)
           (loop (fx- h 1) (fx+ s width) (fx+ dest SCREENWIDTH))))]))

  ;; V_GetBlock: the inverse of V_DrawBlock.
  (define v-get-block
    (case-lambda
      [(x y scrn width height dest) (v-get-block x y scrn width height dest 0)]
      [(x y scrn width height dest dest-offset)
       (check-rect "V_DrawBlock" x y width height scrn)   ; sic: C says DrawBlock
       (let loop ([h height] [d dest-offset]
                  [src (fx+ (screen-base scrn) (fx* y SCREENWIDTH) x)])
         (when (fx> h 0)
           (bytevector-copy! screens src dest d width)
           (loop (fx- h 1) (fx+ d width) (fx+ src SCREENWIDTH))))])))
