;;; r_draw.c: the column and span drawers, and the view window.
;;; Included by r_local.sls.
;;;
;;; All drawing goes into `rbuf`: `screens` (v_video), or with -hires the
;;; renderer's own buffer, rwidth pixels wide. ylookup[] and columnofs[] are
;;; offsets into it. dc_source / ds_source pointers are a bytevector plus an
;;; offset; colormap pointers are offsets into `colormaps`.

;; status bar height at bottom of screen
(define SBARHEIGHT 32)

(define-global viewwidth 0)
(define-global scaledviewwidth 0)
(define-global viewheight 0)
(define-global viewwindowx 0)
(define-global viewwindowy 0)
(define ylookup (make-fxvector MAXHEIGHT 0))
(define columnofs (make-fxvector MAXWIDTH 0))

;; R_DrawColumn state. dc_source is the top of the column to scale
;; (dc-source-ofs into dc-source).
(define dc-colormap 0)
(define dc-x 0)
(define dc-yl 0)
(define dc-yh 0)
(define dc-iscale 0)
(define dc-texturemid 0)
(define dc-source (bytevector))
(define dc-source-ofs 0)

;; A source byte, or 0 past either end of the bytevector: vanilla reads
;; whatever memory is there (see DESIGN.md, "Garbage reads").
(define-syntax src-ref
  (syntax-rules ()
    [(_ bv i) (let ([j i])
                (if (and (fx>= j 0) (fx< j (bytevector-length bv)))
                    (bytevector-u8-ref bv j)
                    0))]))

;; src-ref for a column drawer writing screen offset(s) dest ...: a read
;; past the source also marks those pixels in garbage-mask, when tracking.
(define-syntax src-ref/mark
  (syntax-rules ()
    [(_ bv i dest ...)
     (let ([j i])
       (if (and (fx>= j 0) (fx< j (bytevector-length bv)))
           (bytevector-u8-ref bv j)
           (begin
             (when garbage-mask
               (bytevector-u8-set! garbage-mask dest 1) ...)
             0)))]))

(define (check-column who)
  (when (or (not (fx< -1 dc-x rwidth)) (fx< dc-yl 0) (fx>= dc-yh rheight))
    (i-error "~a: ~a to ~a at ~a" who dc-yl dc-yh dc-x)))

;; A column is a vertical slice/span from a wall texture that, given the
;; DOOM style restrictions on the view orientation, will always have
;; constant z depth. (frac>>16)&127 depends only on bits 16-22 of frac, so
;; frac needs no 32-bit wrapping here.
(define (r-draw-column)
  (let ([count (fx- dc-yh dc-yl)])
    ;; Zero length, column does not exceed a pixel.
    (unless (fx< count 0)
      (check-column "R_DrawColumn")
      (let ([fracstep dc-iscale] [src dc-source] [sofs dc-source-ofs]
            [cmap colormaps] [cofs dc-colormap] [buf rbuf] [pitch rwidth])
        (let loop ([dest (fx+ (fxvector-ref ylookup dc-yl) (fxvector-ref columnofs dc-x))]
                   [frac (fx+ dc-texturemid (fx* (fx- dc-yl centery) fracstep))]
                   [count count])
          ;; Re-map color indices from wall texture column using a
          ;; lighting/special effects LUT.
          (bytevector-u8-set!
           buf dest
           (bytevector-u8-ref cmap (fx+ cofs (src-ref/mark src (fx+ sofs (fxand (fxsra frac FRACBITS) 127))
                                                           dest))))
          (unless (fx= count 0)
            (loop (fx+ dest pitch) (fx+ frac fracstep) (fx- count 1))))))))

;; Low detail: each column is drawn twice. Note that this doubles the
;; global dc_x, which the masked drawers' loops then continue from.
(define (r-draw-column-low)
  (let ([count (fx- dc-yh dc-yl)])
    (unless (fx< count 0)
      (check-column "R_DrawColumn")
      ;; Blocky mode, need to multiply by 2.
      (set! dc-x (fxsll dc-x 1))
      (let ([fracstep dc-iscale] [src dc-source] [sofs dc-source-ofs]
            [cmap colormaps] [cofs dc-colormap] [buf rbuf] [pitch rwidth])
        (let loop ([dest (fx+ (fxvector-ref ylookup dc-yl) (fxvector-ref columnofs dc-x))]
                   [dest2 (fx+ (fxvector-ref ylookup dc-yl) (fxvector-ref columnofs (fx+ dc-x 1)))]
                   [frac (fx+ dc-texturemid (fx* (fx- dc-yl centery) fracstep))]
                   [count count])
          ;; Hack. Does not work corretly.
          (let ([c (bytevector-u8-ref
                    cmap (fx+ cofs (src-ref/mark src (fx+ sofs (fxand (fxsra frac FRACBITS) 127))
                                                 dest dest2)))])
            (bytevector-u8-set! buf dest c)
            (bytevector-u8-set! buf dest2 c))
          (unless (fx= count 0)
            (loop (fx+ dest pitch) (fx+ dest2 pitch) (fx+ frac fracstep)
                  (fx- count 1))))))))

;; Spectre/Invisibility.
(define FUZZTABLE 50)
;; (in rows: the drawer multiplies by the pitch)
(define FUZZOFF 1)
(define fuzzoffset
  (fxvector FUZZOFF (- FUZZOFF) FUZZOFF (- FUZZOFF) FUZZOFF FUZZOFF (- FUZZOFF)
            FUZZOFF FUZZOFF (- FUZZOFF) FUZZOFF FUZZOFF FUZZOFF (- FUZZOFF)
            FUZZOFF FUZZOFF FUZZOFF (- FUZZOFF) (- FUZZOFF) (- FUZZOFF) (- FUZZOFF)
            FUZZOFF (- FUZZOFF) (- FUZZOFF) FUZZOFF FUZZOFF FUZZOFF FUZZOFF (- FUZZOFF)
            FUZZOFF (- FUZZOFF) FUZZOFF FUZZOFF (- FUZZOFF) (- FUZZOFF) FUZZOFF
            FUZZOFF (- FUZZOFF) (- FUZZOFF) (- FUZZOFF) (- FUZZOFF) FUZZOFF FUZZOFF
            FUZZOFF FUZZOFF (- FUZZOFF) FUZZOFF FUZZOFF (- FUZZOFF) FUZZOFF))
(define fuzzpos 0)

;; Framebuffer postprocessing: creates a fuzzy image by copying pixels from
;; the rows above and below, darkened through colormap 6. Does not work
;; with blocky mode (dc_x is not doubled here).
(define (r-draw-fuzz-column)
  ;; Adjust borders. Low...
  (when (fx= dc-yl 0) (set! dc-yl 1))
  ;; .. and high.
  (when (fx= dc-yh (fx- viewheight 1)) (set! dc-yh (fx- viewheight 2)))
  (let ([count (fx- dc-yh dc-yl)])
    (unless (fx< count 0)
      (check-column "R_DrawFuzzColumn")
      (let ([cmap colormaps] [buf rbuf] [pitch rwidth])
        (let loop ([dest (fx+ (fxvector-ref ylookup dc-yl) (fxvector-ref columnofs dc-x))]
                   [count count])
          (let ([src (fx+ dest (fx* pitch (fxvector-ref fuzzoffset fuzzpos)))])
            (bytevector-u8-set!
             buf dest
             (bytevector-u8-ref cmap (fx+ (fx* 6 256) (bytevector-u8-ref buf src))))
            ;; a pixel copied from a garbage pixel is garbage too
            (when (and garbage-mask (fx= (bytevector-u8-ref garbage-mask src) 1))
              (bytevector-u8-set! garbage-mask dest 1)))
          ;; Clamp table lookup index.
          (set! fuzzpos (fx+ fuzzpos 1))
          (when (fx= fuzzpos FUZZTABLE) (set! fuzzpos 0))
          (unless (fx= count 0)
            (loop (fx+ dest pitch) (fx- count 1))))))))

;; R_DrawTranslatedColumn: used to draw player sprites with the green
;; colorramp mapped to others. dc_translation is an offset into
;; translationtables. Note there is no &127 here, so frac's 32-bit wrap
;; matters.
(define dc-translation 0)
(define translationtables (make-bytevector (* 256 3) 0))

(define (r-draw-translated-column)
  (let ([count (fx- dc-yh dc-yl)])
    (unless (fx< count 0)
      (check-column "R_DrawColumn")
      (let ([fracstep dc-iscale] [src dc-source] [sofs dc-source-ofs]
            [cmap colormaps] [cofs dc-colormap] [tofs dc-translation] [buf rbuf] [pitch rwidth])
        (let loop ([dest (fx+ (fxvector-ref ylookup dc-yl) (fxvector-ref columnofs dc-x))]
                   [frac (int32 (fx+ dc-texturemid (int32 (fx* (fx- dc-yl centery) fracstep))))]
                   [count count])
          (bytevector-u8-set!
           buf dest
           (bytevector-u8-ref
            cmap (fx+ cofs (src-ref translationtables
                                    (fx+ tofs (src-ref/mark src (fx+ sofs (fxsra frac FRACBITS))
                                                            dest))))))
          (unless (fx= count 0)
            (loop (fx+ dest pitch) (int32 (fx+ frac fracstep)) (fx- count 1))))))))

;; R_InitTranslationTables: creates the translation tables to map the green
;; color ramp to gray, brown, red.
(define (r-init-translation-tables)
  (do ([i 0 (fx+ i 1)]) ((fx= i 256))
    (if (fx<= #x70 i #x7f)
        ;; map green ramp to gray, brown, red
        (begin
          (bytevector-u8-set! translationtables i (fx+ #x60 (fxand i #xf)))
          (bytevector-u8-set! translationtables (fx+ i 256) (fx+ #x40 (fxand i #xf)))
          (bytevector-u8-set! translationtables (fx+ i 512) (fx+ #x20 (fxand i #xf))))
        ;; Keep all other colors as is.
        (begin
          (bytevector-u8-set! translationtables i i)
          (bytevector-u8-set! translationtables (fx+ i 256) i)
          (bytevector-u8-set! translationtables (fx+ i 512) i)))))

;; R_DrawSpan state. ds_source is a 64*64 flat.
(define ds-y 0)
(define ds-x1 0)
(define ds-x2 0)
(define ds-colormap 0)
(define ds-xfrac 0)
(define ds-yfrac 0)
(define ds-xstep 0)
(define ds-ystep 0)
(define ds-source (bytevector))

(define (check-span)
  (when (or (fx< ds-x2 ds-x1) (fx< ds-x1 0) (fx>= ds-x2 rwidth)
            (not (fx<= 0 ds-y rheight)))
    (i-error "R_DrawSpan: ~a to ~a at ~a" ds-x1 ds-x2 ds-y)))

;; Current texture index in u,v: spot depends only on bits of the fracs, so
;; they need no 32-bit wrapping.
(define-syntax spot
  (syntax-rules ()
    [(_ xfrac yfrac)
     (fx+ (fxand (fxsra yfrac (fx- 16 6)) (fx* 63 64)) (fxand (fxsra xfrac 16) 63))]))

(define (r-draw-span)
  (check-span)
  (let ([src ds-source] [cmap colormaps] [cofs ds-colormap]
        [xstep ds-xstep] [ystep ds-ystep] [buf rbuf])
    ;; We do not check for zero spans here?
    (let loop ([dest (fx+ (fxvector-ref ylookup ds-y) (fxvector-ref columnofs ds-x1))]
               [xfrac ds-xfrac] [yfrac ds-yfrac] [count (fx- ds-x2 ds-x1)])
      (bytevector-u8-set! buf dest
                          (bytevector-u8-ref cmap (fx+ cofs (bytevector-u8-ref src (spot xfrac yfrac)))))
      (unless (fx= count 0)
        (loop (fx+ dest 1) (fx+ xfrac xstep) (fx+ yfrac ystep) (fx- count 1))))))

;; Low detail. Note that doubling x1 and x2 also doubles the count, so
;; linuxdoom draws each span about twice as wide as it should (the excess
;; is overdrawn by later spans or runs past the view).
(define (r-draw-span-low)
  (check-span)
  ;; Blocky mode, need to multiply by 2.
  (set! ds-x1 (fxsll ds-x1 1))
  (set! ds-x2 (fxsll ds-x2 1))
  (let ([src ds-source] [cmap colormaps] [cofs ds-colormap]
        [xstep ds-xstep] [ystep ds-ystep] [buf rbuf])
    (let loop ([dest (fx+ (fxvector-ref ylookup ds-y) (fxvector-ref columnofs ds-x1))]
               [xfrac ds-xfrac] [yfrac ds-yfrac] [count (fx- ds-x2 ds-x1)])
      (let ([c (bytevector-u8-ref cmap (fx+ cofs (bytevector-u8-ref src (spot xfrac yfrac))))])
        (bytevector-u8-set! buf dest c)
        (bytevector-u8-set! buf (fx+ dest 1) c))
      (unless (fx= count 0)
        (loop (fx+ dest 2) (fx+ xfrac xstep) (fx+ yfrac ystep) (fx- count 1))))))

;; R_InitBuffer: creates lookup tables that avoid multiplies and other
;; hazzles for getting the framebuffer address of a pixel to draw.
(define (r-init-buffer width height)
  ;; Handle resize, e.g. smaller view windows with border and/or status bar.
  (set! viewwindowx (fxsra (fx- rwidth width) 1))
  ;; Column offset. For windows.
  (do ([i 0 (fx+ i 1)]) ((fx= i width))
    (fxvector-set! columnofs i (fx+ viewwindowx i)))
  ;; Same with base row offset.
  (set! viewwindowy (if (fx= width rwidth)
                        0
                        (fxsra (fx- (fx- rheight (fxsll SBARHEIGHT hires-shift)) height) 1)))
  ;; Precalculate all row offsets.
  (do ([i 0 (fx+ i 1)]) ((fx= i height))
    (fxvector-set! ylookup i (fx* (fx+ i viewwindowy) rwidth)))
  ;; (and the window at 320x200, for the 2D layer)
  (set! view2d-x (fxsra viewwindowx hires-shift))
  (set! view2d-width (fxsra viewwidth hires-shift))
  (set! view2d-y (fxsra viewwindowy hires-shift))
  (set! view2d-scaledwidth (fxsra width hires-shift))
  (set! view2d-height (fxsra height hires-shift)))

;; R_FillBackScreen: fills the back screen (screens[1]) with a pattern for
;; variable screen sizes. Also draws a beveled edge. (In 320x200, like all
;; the 2D layer: see view2d-x.)
(define (r-fill-back-screen)
  (unless (fx= view2d-scaledwidth 320)
    (let ([src (w-cache-lump-name (if (fx= gamemode commercial) "GRNROCK" "FLOOR7_2"))])
      (let loop ([y 0] [dest (screen-base 1)])
        (when (fx< y (fx- SCREENHEIGHT SBARHEIGHT))
          (let ([row (fxsll (fxand y 63) 6)])
            (do ([x 0 (fx+ x 1)]) ((fx= x (fxquotient SCREENWIDTH 64)))
              (bytevector-copy! src row screens (fx+ dest (fx* x 64)) 64))
            (let ([dest (fx+ dest (fx* 64 (fxquotient SCREENWIDTH 64)))])
              (unless (fx= (fxand SCREENWIDTH 63) 0)
                (bytevector-copy! src row screens dest (fxand SCREENWIDTH 63)))
              (loop (fx+ y 1) (fx+ dest (fxand SCREENWIDTH 63))))))))
    (let ([patch (w-cache-lump-name "brdr_t")])
      (do ([x 0 (fx+ x 8)]) ((fx>= x view2d-scaledwidth))
        (v-draw-patch (fx+ view2d-x x) (fx- view2d-y 8) 1 patch)))
    (let ([patch (w-cache-lump-name "brdr_b")])
      (do ([x 0 (fx+ x 8)]) ((fx>= x view2d-scaledwidth))
        (v-draw-patch (fx+ view2d-x x) (fx+ view2d-y view2d-height) 1 patch)))
    (let ([patch (w-cache-lump-name "brdr_l")])
      (do ([y 0 (fx+ y 8)]) ((fx>= y view2d-height))
        (v-draw-patch (fx- view2d-x 8) (fx+ view2d-y y) 1 patch)))
    (let ([patch (w-cache-lump-name "brdr_r")])
      (do ([y 0 (fx+ y 8)]) ((fx>= y view2d-height))
        (v-draw-patch (fx+ view2d-x view2d-scaledwidth) (fx+ view2d-y y) 1 patch)))
    ;; Draw beveled edge.
    (v-draw-patch (fx- view2d-x 8) (fx- view2d-y 8) 1 (w-cache-lump-name "brdr_tl"))
    (v-draw-patch (fx+ view2d-x view2d-scaledwidth) (fx- view2d-y 8) 1
                  (w-cache-lump-name "brdr_tr"))
    (v-draw-patch (fx- view2d-x 8) (fx+ view2d-y view2d-height) 1
                  (w-cache-lump-name "brdr_bl"))
    (v-draw-patch (fx+ view2d-x view2d-scaledwidth) (fx+ view2d-y view2d-height) 1
                  (w-cache-lump-name "brdr_br"))))

;; Copy a screen buffer.
(define (r-video-erase ofs count)
  (bytevector-copy! screens (fx+ (screen-base 1) ofs) screens (fx+ (screen-base 0) ofs) count))

;; R_DrawViewBorder: draws the border around the view for different size
;; windows.
(define (r-draw-view-border)
  (unless (fx= view2d-scaledwidth SCREENWIDTH)
    (let* ([top (fxquotient (fx- (fx- SCREENHEIGHT SBARHEIGHT) view2d-height) 2)]
           [side (fxquotient (fx- SCREENWIDTH view2d-scaledwidth) 2)])
      ;; copy top and one line of left side
      (r-video-erase 0 (fx+ (fx* top SCREENWIDTH) side))
      ;; copy one line of right side and bottom
      (r-video-erase (fx- (fx* (fx+ view2d-height top) SCREENWIDTH) side)
                     (fx+ (fx* top SCREENWIDTH) side))
      ;; copy sides using wraparound
      (let ([side2 (fxsll side 1)])
        (do ([i 1 (fx+ i 1)]
             [ofs (fx- (fx+ (fx* top SCREENWIDTH) SCREENWIDTH) side) (fx+ ofs SCREENWIDTH)])
            ((fx>= i view2d-height))
          (r-video-erase ofs side2)))
      (v-mark-rect 0 0 SCREENWIDTH (fx- SCREENHEIGHT SBARHEIGHT)))))

;; -hires k (2 or 4): the renderer draws k times 320x200 into its own
;; buffer. Called before R_Init.
(define (r-set-hires! k)
  (let ([shift (case k [(1) 0] [(2) 1] [(4) 2] [else (i-error "-hires: 2 or 4, not ~a" k)])])
    (set! hires-shift shift)
    (set! rwidth (fxsll SCREENWIDTH shift))
    (set! rheight (fxsll SCREENHEIGHT shift))
    (set! vp-empty (if (fx= shift 0) #xff #xffff))
    (if (fx= shift 0)
        (begin (set! rbuf screens)
               (set! hires-k 1) (set! hires-buf #f) (set! overlay-mask #f))
        ;; (twice the size: linuxdoom's low-detail spans run past the view,
        ;; at the bottom past screen 0 into screen 1, and so here past the
        ;; picture)
        (begin (set! rbuf (make-bytevector (fx* 2 rwidth rheight) 0))
               (set! hires-k k) (set! hires-buf rbuf)
               (set! overlay-mask (make-bytevector (fx* SCREENWIDTH SCREENHEIGHT) 0))))))

;; After a hi-res render: leave the view, downscaled, in screen 0 (so the
;; wipe, screenshots and anything else reading screen 0 see the picture),
;; clear the overlay mask over it (2D drawing from here on covers the
;; view), and tell I_FinishUpdate where the view is.
(define (r-hires-finish-view)
  (unless (fx= hires-shift 0)
    (let ([k (fxsll 1 hires-shift)] [buf rbuf] [mask overlay-mask])
      (do ([y 0 (fx+ y 1)]) ((fx= y view2d-height))
        (let ([dst (fx+ (fx* (fx+ y view2d-y) SCREENWIDTH) view2d-x)]
              [src (fx+ (fx* (fx* (fx+ y view2d-y) k) rwidth) (fx* view2d-x k))])
          (do ([x 0 (fx+ x 1)]) ((fx= x view2d-scaledwidth))
            (bytevector-u8-set! screens (fx+ dst x) (bytevector-u8-ref buf (fx+ src (fx* x k))))
            (bytevector-u8-set! mask (fx+ dst x) 0))))
      (set! hires-view (list viewwindowx viewwindowy scaledviewwidth viewheight)))))
