;;; doomdata: on-disk map format constants (doomdata.h). The map lumps
;;; themselves are parsed in p_setup straight from the WAD bytevectors.
(library (doom doomdata)
  (export ML_LABEL ML_THINGS ML_LINEDEFS ML_SIDEDEFS ML_VERTEXES ML_SEGS
          ML_SSECTORS ML_NODES ML_SECTORS ML_REJECT ML_BLOCKMAP
          ML_BLOCKING ML_BLOCKMONSTERS ML_TWOSIDED ML_DONTPEGTOP ML_DONTPEGBOTTOM
          ML_SECRET ML_SOUNDBLOCK ML_DONTDRAW ML_MAPPED NF_SUBSECTOR
          mapthing make-mapthing mapthing? mapthing-x mapthing-x-set!
          mapthing-y mapthing-y-set! mapthing-angle mapthing-angle-set!
          mapthing-type mapthing-type-set! mapthing-options mapthing-options-set!
          copy-mapthing)
  (import (chezscheme) (doom struct))

  ;; Lump order in a map entry: ExMy, then these.
  (define ML_LABEL 0)
  (define ML_THINGS 1)
  (define ML_LINEDEFS 2)
  (define ML_SIDEDEFS 3)
  (define ML_VERTEXES 4)
  (define ML_SEGS 5)
  (define ML_SSECTORS 6)
  (define ML_NODES 7)
  (define ML_SECTORS 8)
  (define ML_REJECT 9)
  (define ML_BLOCKMAP 10)

  ;; Linedef flags.
  (define ML_BLOCKING 1)
  (define ML_BLOCKMONSTERS 2)
  (define ML_TWOSIDED 4)
  (define ML_DONTPEGTOP 8)
  (define ML_DONTPEGBOTTOM 16)
  (define ML_SECRET 32)
  (define ML_SOUNDBLOCK 64)
  (define ML_DONTDRAW 128)
  (define ML_MAPPED 256)

  (define NF_SUBSECTOR #x8000)

  ;; mapthing_t is kept as a record because C copies it around by value
  ;; (playerstarts[], mobj->spawnpoint); copy-mapthing is that struct copy.
  (define-struct mapthing () (x 0) (y 0) (angle 0) (type 0) (options 0))
  (define (copy-mapthing m)
    (make-mapthing (mapthing-x m) (mapthing-y m) (mapthing-angle m)
                   (mapthing-type m) (mapthing-options m))))
