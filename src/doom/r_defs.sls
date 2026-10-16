;;; r_defs: the runtime map structures (r_defs.h).
;;;
;;; C keeps these in arrays and links them with pointers; here each is a
;;; record linked by reference, and the arrays (r_state) are vectors of them.
;;; Where C turns a pointer back into an array index (`sec - sectors`), the
;;; record carries its `index`.
(library (doom r_defs)
  (export ST_HORIZONTAL ST_VERTICAL ST_POSITIVE ST_NEGATIVE
          vertex make-vertex vertex? vertex-x vertex-x-set! vertex-y vertex-y-set!
          degenmobj make-degenmobj degenmobj?
          degenmobj-x degenmobj-x-set! degenmobj-y degenmobj-y-set!
          degenmobj-z degenmobj-z-set!
          sector make-sector sector? sector-index sector-index-set!
          sector-floorheight sector-floorheight-set!
          sector-ceilingheight sector-ceilingheight-set!
          sector-floorpic sector-floorpic-set! sector-ceilingpic sector-ceilingpic-set!
          sector-lightlevel sector-lightlevel-set! sector-special sector-special-set!
          sector-tag sector-tag-set! sector-soundtraversed sector-soundtraversed-set!
          sector-soundtarget sector-soundtarget-set! sector-blockbox sector-blockbox-set!
          sector-soundorg sector-soundorg-set! sector-validcount sector-validcount-set!
          sector-thinglist sector-thinglist-set! sector-specialdata sector-specialdata-set!
          sector-linecount sector-linecount-set! sector-lines sector-lines-set!
          side make-side side? side-index side-index-set!
          side-textureoffset side-textureoffset-set! side-rowoffset side-rowoffset-set!
          side-toptexture side-toptexture-set! side-bottomtexture side-bottomtexture-set!
          side-midtexture side-midtexture-set! side-sector side-sector-set!
          line make-line line? line-index line-index-set!
          line-v1 line-v1-set! line-v2 line-v2-set! line-dx line-dx-set!
          line-dy line-dy-set! line-flags line-flags-set! line-special line-special-set!
          line-tag line-tag-set! line-sidenum line-sidenum-set! line-bbox line-bbox-set!
          line-slopetype line-slopetype-set! line-frontsector line-frontsector-set!
          line-backsector line-backsector-set! line-validcount line-validcount-set!
          line-specialdata line-specialdata-set!
          subsector make-subsector subsector? subsector-index subsector-index-set!
          subsector-sector subsector-sector-set! subsector-numlines subsector-numlines-set!
          subsector-firstline subsector-firstline-set!
          seg make-seg seg? seg-index seg-index-set! seg-v1 seg-v1-set! seg-v2 seg-v2-set!
          seg-offset seg-offset-set! seg-angle seg-angle-set! seg-sidedef seg-sidedef-set!
          seg-linedef seg-linedef-set! seg-frontsector seg-frontsector-set!
          seg-backsector seg-backsector-set!
          node make-node node? node-x node-x-set! node-y node-y-set! node-dx node-dx-set!
          node-dy node-dy-set! node-bbox node-bbox-set! node-children node-children-set!)
  (import (chezscheme) (doom struct) (doom d_think))

  (define ST_HORIZONTAL 0)
  (define ST_VERTICAL 1)
  (define ST_POSITIVE 2)
  (define ST_NEGATIVE 3)

  (define-struct vertex () (x 0) (y 0))

  ;; degenmobj_t: a sector's sound origin. mobj_t is its subtype, since C
  ;; passes a degenmobj_t* wherever an mobj_t* origin is expected and only
  ;; reads x/y/z through it.
  (define-struct degenmobj (thinker) (x 0) (y 0) (z 0))

  (define-struct sector ()
    (index 0)
    (floorheight 0) (ceilingheight 0)
    (floorpic 0) (ceilingpic 0)
    (lightlevel 0) (special 0) (tag 0)
    (soundtraversed 0)          ; 0 = untraversed, 1,2 = sndlines-1
    (soundtarget #f)            ; mobj that made a sound
    (blockbox (make-fxvector 4 0))  ; mapblock bounding box, BOXTOP etc.
    (soundorg (make-degenmobj))
    (validcount 0)
    (thinglist #f)              ; first mobj in the sector (snext chain)
    (specialdata #f)            ; thinker for reversible actions
    (linecount 0)
    (lines (vector)))           ; [linecount] lines

  (define-struct side ()
    (index 0)
    (textureoffset 0) (rowoffset 0)
    (toptexture 0) (bottomtexture 0) (midtexture 0)
    (sector #f))

  (define-struct line ()
    (index 0)
    (v1 #f) (v2 #f)
    (dx 0) (dy 0)
    (flags 0) (special 0) (tag 0)
    (sidenum (make-fxvector 2 -1))  ; sidenum[1] = -1 if one sided
    (bbox (make-fxvector 4 0))
    (slopetype 0)
    (frontsector #f) (backsector #f)
    (validcount 0)
    (specialdata #f))

  (define-struct subsector ()
    (index 0) (sector #f) (numlines 0) (firstline 0))

  (define-struct seg ()
    (index 0)
    (v1 #f) (v2 #f)
    (offset 0)
    (angle 0)
    (sidedef #f) (linedef #f)
    (frontsector #f)
    (backsector #f))            ; #f for one sided lines

  (define-struct node ()
    (x 0) (y 0) (dx 0) (dy 0)   ; partition line
    (bbox (vector (make-fxvector 4 0) (make-fxvector 4 0)))  ; per child
    (children (make-fxvector 2 0))))  ; NF_SUBSECTOR set: a subsector
