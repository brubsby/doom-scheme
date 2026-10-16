;;; p_setup.c: load a level from the WAD. Included by p_local.sls.
;;;
;;; Status: P_SetupLevel loads the map and spawns its things; deathmatch
;;; respawning and P_SpawnSpecials come with later milestones.

(define (s16 bv o) (bytevector-s16-ref bv o (endianness little)))
(define (u16 bv o) (bytevector-u16-ref bv o (endianness little)))
;; 8-byte NUL-padded name field.
(define (name8 bv o)
  (let loop ([i 0])
    (if (or (fx= i 8) (fx= (bytevector-u8-ref bv (fx+ o i)) 0))
        (let ([s (make-string i)])
          (do ([j 0 (fx+ j 1)]) ((fx= j i) s)
            (string-set! s j (integer->char (bytevector-u8-ref bv (fx+ o j))))))
        (loop (fx+ i 1)))))

;; Records of `size` bytes in `lump`: calls (f i offset) for each and
;; collects the results in a vector. Trailing partial records are ignored,
;; as C's length/sizeof does.
(define (load-records lump size f)
  (let* ([data (w-cache-lump-num lump)]
         [n (fxquotient (w-lump-length lump) size)]
         [v (make-vector n)])
    (do ([i 0 (fx+ i 1)]) ((fx= i n) v)
      (vector-set! v i (f data i (fx* i size))))))

;; short<<FRACBITS: fits in 32 bits, so no wrap is needed.
(define (fixed16 bv o) (fxsll (s16 bv o) FRACBITS))

(define (p-load-vertexes lump)
  (set! vertexes
        (load-records lump 4
          (lambda (d i o) (make-vertex (fixed16 d o) (fixed16 d (fx+ o 2))))))
  (set! numvertexes (vector-length vertexes)))

;; mapseg_t: v1 v2 angle linedef side offset (shorts).
(define (p-load-segs lump)
  (set! segs
        (load-records lump 12
          (lambda (d i o)
            (let* ([ldef (vector-ref lines (s16 d (fx+ o 6)))]
                   [side (s16 d (fx+ o 8))]
                   [sidedef (vector-ref sides (fxvector-ref (line-sidenum ldef) side))]
                   [li (make-seg)])
              (seg-index-set! li i)
              (seg-v1-set! li (vector-ref vertexes (s16 d o)))
              (seg-v2-set! li (vector-ref vertexes (s16 d (fx+ o 2))))
              ;; angle_t: the short's bits in the top half.
              (seg-angle-set! li (fxand (fxsll (s16 d (fx+ o 4)) 16) #xFFFFFFFF))
              (seg-offset-set! li (fxsll (s16 d (fx+ o 10)) 16))
              (seg-linedef-set! li ldef)
              (seg-sidedef-set! li sidedef)
              (seg-frontsector-set! li (side-sector sidedef))
              (seg-backsector-set!
               li (and (fxlogtest (line-flags ldef) ML_TWOSIDED)
                       (side-sector
                        (vector-ref sides (fxvector-ref (line-sidenum ldef)
                                                        (fxxor side 1))))))
              li))))
  (set! numsegs (vector-length segs)))

(define (p-load-subsectors lump)
  (set! subsectors
        (load-records lump 4
          (lambda (d i o)
            (let ([ss (make-subsector)])
              (subsector-index-set! ss i)
              (subsector-numlines-set! ss (s16 d o))
              (subsector-firstline-set! ss (s16 d (fx+ o 2)))
              ss))))
  (set! numsubsectors (vector-length subsectors)))

;; mapsector_t: floorheight ceilingheight floorpic[8] ceilingpic[8]
;; lightlevel special tag.
(define (p-load-sectors lump)
  (set! sectors
        (load-records lump 26
          (lambda (d i o)
            (let ([ss (make-sector)])
              (sector-index-set! ss i)
              (sector-floorheight-set! ss (fixed16 d o))
              (sector-ceilingheight-set! ss (fixed16 d (fx+ o 2)))
              (sector-floorpic-set! ss (r-flat-num-for-name (name8 d (fx+ o 4))))
              (sector-ceilingpic-set! ss (r-flat-num-for-name (name8 d (fx+ o 12))))
              (sector-lightlevel-set! ss (s16 d (fx+ o 20)))
              (sector-special-set! ss (s16 d (fx+ o 22)))
              (sector-tag-set! ss (s16 d (fx+ o 24)))
              ss))))
  (set! numsectors (vector-length sectors)))

;; mapnode_t: x y dx dy bbox[2][4] (shorts), children[2] (unsigned short).
(define (p-load-nodes lump)
  (set! nodes
        (load-records lump 28
          (lambda (d i o)
            (let ([no (make-node)])
              (node-x-set! no (fixed16 d o))
              (node-y-set! no (fixed16 d (fx+ o 2)))
              (node-dx-set! no (fixed16 d (fx+ o 4)))
              (node-dy-set! no (fixed16 d (fx+ o 6)))
              (do ([j 0 (fx+ j 1)]) ((fx= j 2))
                (fxvector-set! (node-children no) j (u16 d (fx+ o 24 (fx* j 2))))
                (do ([k 0 (fx+ k 1)]) ((fx= k 4))
                  (fxvector-set! (vector-ref (node-bbox no) j) k
                                 (fixed16 d (fx+ o 8 (fx* j 8) (fx* k 2))))))
              no))))
  (set! numnodes (vector-length nodes)))

;; mapthing_t: x y angle type options.
(define (p-load-things lump)
  (let* ([data (w-cache-lump-num lump)]
         [numthings (fxquotient (w-lump-length lump) 10)])
    (let loop ([i 0])
      (when (fx< i numthings)
        (let* ([o (fx* i 10)]
               [type (s16 data (fx+ o 6))])
          ;; Do not spawn cool, new monsters if !commercial. Note that this
          ;; `break`s out of the loop: no later thing spawns either.
          (unless (and (not (fx= gamemode commercial))
                       (memv type '(68 64 88 89 69 67 71 65 66 84)))
            (p-spawn-map-thing
             (make-mapthing (s16 data o) (s16 data (fx+ o 2)) (s16 data (fx+ o 4))
                            type (s16 data (fx+ o 8))))
            (loop (fx+ i 1))))))))

;; maplinedef_t: v1 v2 flags special tag sidenum[2] (shorts).
(define (p-load-linedefs lump)
  (set! lines
        (load-records lump 14
          (lambda (d i o)
            (let* ([ld (make-line)]
                   [v1 (vector-ref vertexes (s16 d (fx+ o 0)))]
                   [v2 (vector-ref vertexes (s16 d (fx+ o 2)))]
                   [dx (int32 (fx- (vertex-x v2) (vertex-x v1)))]
                   [dy (int32 (fx- (vertex-y v2) (vertex-y v1)))]
                   [bbox (line-bbox ld)]
                   [sidenum (line-sidenum ld)])
              (line-index-set! ld i)
              (line-flags-set! ld (s16 d (fx+ o 4)))
              (line-special-set! ld (s16 d (fx+ o 6)))
              (line-tag-set! ld (s16 d (fx+ o 8)))
              (line-v1-set! ld v1)
              (line-v2-set! ld v2)
              (line-dx-set! ld dx)
              (line-dy-set! ld dy)
              (line-slopetype-set! ld
                (cond
                  [(fx= dx 0) ST_VERTICAL]
                  [(fx= dy 0) ST_HORIZONTAL]
                  [(fx> (fixed-div dy dx) 0) ST_POSITIVE]
                  [else ST_NEGATIVE]))
              (if (fx< (vertex-x v1) (vertex-x v2))
                  (begin (fxvector-set! bbox BOXLEFT (vertex-x v1))
                         (fxvector-set! bbox BOXRIGHT (vertex-x v2)))
                  (begin (fxvector-set! bbox BOXLEFT (vertex-x v2))
                         (fxvector-set! bbox BOXRIGHT (vertex-x v1))))
              (if (fx< (vertex-y v1) (vertex-y v2))
                  (begin (fxvector-set! bbox BOXBOTTOM (vertex-y v1))
                         (fxvector-set! bbox BOXTOP (vertex-y v2)))
                  (begin (fxvector-set! bbox BOXBOTTOM (vertex-y v2))
                         (fxvector-set! bbox BOXTOP (vertex-y v1))))
              (fxvector-set! sidenum 0 (s16 d (fx+ o 10)))
              (fxvector-set! sidenum 1 (s16 d (fx+ o 12)))
              (line-frontsector-set! ld (and (not (fx= (fxvector-ref sidenum 0) -1))
                                             (side-sector (vector-ref sides (fxvector-ref sidenum 0)))))
              (line-backsector-set! ld (and (not (fx= (fxvector-ref sidenum 1) -1))
                                            (side-sector (vector-ref sides (fxvector-ref sidenum 1)))))
              ld))))
  (set! numlines (vector-length lines)))

;; mapsidedef_t: textureoffset rowoffset top[8] bottom[8] mid[8] sector.
(define (p-load-sidedefs lump)
  (set! sides
        (load-records lump 30
          (lambda (d i o)
            (let ([sd (make-side)])
              (side-index-set! sd i)
              (side-textureoffset-set! sd (fixed16 d o))
              (side-rowoffset-set! sd (fixed16 d (fx+ o 2)))
              (side-toptexture-set! sd (r-texture-num-for-name (name8 d (fx+ o 4))))
              (side-bottomtexture-set! sd (r-texture-num-for-name (name8 d (fx+ o 12))))
              (side-midtexture-set! sd (r-texture-num-for-name (name8 d (fx+ o 20))))
              (side-sector-set! sd (vector-ref sectors (s16 d (fx+ o 28))))
              sd))))
  (set! numsides (vector-length sides)))

(define (p-load-blockmap lump)
  (let* ([data (w-cache-lump-num lump)]
         [count (fxquotient (w-lump-length lump) 2)]
         [bm (make-fxvector count)])
    (do ([i 0 (fx+ i 1)]) ((fx= i count))
      (fxvector-set! bm i (s16 data (fx* i 2))))
    (set! blockmaplump bm)
    (set! bmaporgx (fxsll (fxvector-ref bm 0) FRACBITS))
    (set! bmaporgy (fxsll (fxvector-ref bm 1) FRACBITS))
    (set! bmapwidth (fxvector-ref bm 2))
    (set! bmapheight (fxvector-ref bm 3))
    ;; clear out mobj chains
    (set! blocklinks (make-vector (fx* bmapwidth bmapheight) #f))))

;; P_GroupLines: builds sector line lists and subsector sector numbers,
;; and finds block bounding boxes for sectors.
(define (p-group-lines)
  ;; look up sector number for each subsector
  (vector-for-each
   (lambda (ss)
     (subsector-sector-set!
      ss (side-sector (seg-sidedef (vector-ref segs (subsector-firstline ss))))))
   subsectors)
  ;; count number of lines in each sector
  (vector-for-each
   (lambda (li)
     (let ([front (line-frontsector li)] [back (line-backsector li)])
       (sector-linecount-set! front (fx+ (sector-linecount front) 1))
       (when (and back (not (eq? back front)))
         (sector-linecount-set! back (fx+ (sector-linecount back) 1)))))
   lines)
  ;; build line tables for each sector
  (let ([bbox (make-box)])
    (vector-for-each
     (lambda (sector)
       (m-clear-box bbox)
       (let ([found
              (let loop ([j 0] [acc '()])
                (if (fx= j numlines)
                    (reverse acc)
                    (let ([li (vector-ref lines j)])
                      (if (or (eq? (line-frontsector li) sector)
                              (eq? (line-backsector li) sector))
                          (begin
                            (m-add-to-box bbox (vertex-x (line-v1 li)) (vertex-y (line-v1 li)))
                            (m-add-to-box bbox (vertex-x (line-v2 li)) (vertex-y (line-v2 li)))
                            (loop (fx+ j 1) (cons li acc)))
                          (loop (fx+ j 1) acc)))))])
         (sector-lines-set! sector (list->vector found))
         (unless (fx= (length found) (sector-linecount sector))
           (i-error "P_GroupLines: miscounted")))
       ;; set the degenmobj_t to the middle of the bounding box
       (let ([org (sector-soundorg sector)]
             [top (fxvector-ref bbox BOXTOP)] [bottom (fxvector-ref bbox BOXBOTTOM)]
             [left (fxvector-ref bbox BOXLEFT)] [right (fxvector-ref bbox BOXRIGHT)]
             [blockbox (sector-blockbox sector)])
         (degenmobj-x-set! org (fxquotient (int32 (fx+ right left)) 2))
         (degenmobj-y-set! org (fxquotient (int32 (fx+ top bottom)) 2))
         ;; adjust bounding box to map blocks
         (let ([block (fxsra (int32 (fx+ (fx- top bmaporgy) MAXRADIUS)) MAPBLOCKSHIFT)])
           (fxvector-set! blockbox BOXTOP (if (fx>= block bmapheight) (fx- bmapheight 1) block)))
         (let ([block (fxsra (int32 (fx- (fx- bottom bmaporgy) MAXRADIUS)) MAPBLOCKSHIFT)])
           (fxvector-set! blockbox BOXBOTTOM (if (fx< block 0) 0 block)))
         (let ([block (fxsra (int32 (fx+ (fx- right bmaporgx) MAXRADIUS)) MAPBLOCKSHIFT)])
           (fxvector-set! blockbox BOXRIGHT (if (fx>= block bmapwidth) (fx- bmapwidth 1) block)))
         (let ([block (fxsra (int32 (fx- (fx- left bmaporgx) MAXRADIUS)) MAPBLOCKSHIFT)])
           (fxvector-set! blockbox BOXLEFT (if (fx< block 0) 0 block)))))
     sectors)))

(define (map-lump-name episode map)
  (if (fx= gamemode commercial)
      (format "map~2,'0d" map)
      ;; C builds "E?M?" with '0'+episode and '0'+map, single characters.
      (string #\E (integer->char (fx+ 48 episode)) #\M (integer->char (fx+ 48 map)))))

;; P_SetupLevel. `stop-after-group-lines` is for tests/level.ss, which
;; compares the structures against the oracle's dump taken at that point.
(define p-setup-level
  (case-lambda
    [(episode map playermask skill) (p-setup-level episode map playermask skill #f)]
    [(episode map playermask skill stop-after-group-lines)
     (set! totalkills 0)
     (set! totalitems 0)
     (set! totalsecret 0)
     ;; TODO(milestone 7): wminfo.maxfrags = 0, wminfo.partime = 180.
     (vector-for-each
      (lambda (p)
        (player-killcount-set! p 0)
        (player-secretcount-set! p 0)
        (player-itemcount-set! p 0))
      players)
     ;; Initial height of PointOfView will be set by player think.
     (player-viewz-set! (vector-ref players consoleplayer) 1)
     ;; TODO(milestone 8): S_Start (make sure all sounds are stopped).
     (p-init-thinkers)
     (let ([lumpnum (w-get-num-for-name (map-lump-name episode map))])
       (set! leveltime 0)
       ;; note: most of this ordering is important
       (p-load-blockmap (fx+ lumpnum ML_BLOCKMAP))
       (p-load-vertexes (fx+ lumpnum ML_VERTEXES))
       (p-load-sectors (fx+ lumpnum ML_SECTORS))
       (p-load-sidedefs (fx+ lumpnum ML_SIDEDEFS))
       (p-load-linedefs (fx+ lumpnum ML_LINEDEFS))
       (p-load-subsectors (fx+ lumpnum ML_SSECTORS))
       (p-load-nodes (fx+ lumpnum ML_NODES))
       (p-load-segs (fx+ lumpnum ML_SEGS))
       (set! rejectmatrix (w-cache-lump-num (fx+ lumpnum ML_REJECT)))
       (p-group-lines)
       (unless stop-after-group-lines
         (set! bodyqueslot 0)
         (set! deathmatch_p 0)
         (p-load-things (fx+ lumpnum ML_THINGS))
         ;; TODO(milestone 6): deathmatch respawn (G_DeathMatchSpawnPlayer).
         ;; TODO(milestone 4/5): iquehead = iquetail = 0; P_SpawnSpecials.
         ;; preload graphics
         (when precache (r-precache-level))))]))

;; P_Init. TODO(milestone 5): P_InitSwitchList, P_InitPicAnims.
(define (p-init)
  (r-init-sprites sprnames))
