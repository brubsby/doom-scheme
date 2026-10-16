;;; r_sky.c: sky mapping. Included by r_local.sls.

;; SKY, store the number for name.
(define SKYFLATNAME "F_SKY1")

;; The sky map is 256*128*4 maps.
(define ANGLETOSKYSHIFT 22)

(define-global skyflatnum 0)
(define-global skytexture 0)
(define skytexturemid 0)

;; R_InitSkyMap: called whenever the view size changes.
(define (r-init-sky-map)
  (set! skytexturemid (* 100 FRACUNIT)))
