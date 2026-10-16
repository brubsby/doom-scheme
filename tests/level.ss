;;; p_setup against the oracle: every shareware map's structures, as left by
;;; P_GroupLines, must print exactly like build/oracle/levels/E1Mn/level.txt
;;; (tools/oracle/oracle.c, oracle_level_dump).
(import (chezscheme) (doom w_wad) (doom r_data) (doom p_local) (doom r_state)
        (doom r_defs) (doom doomdef) (doom doomstat) (doom trace))
(load "tests/lib.ss")

(define (level-dump episode map lumpnum)
  (define vindex
    (let ([h (make-eq-hashtable)])
      (vector-for-each (lambda (v i) (hashtable-set! h v i))
                       vertexes (list->vector (iota numvertexes)))
      (lambda (v) (hashtable-ref h v -1))))
  (define (sec-idx s) (if s (sector-index s) -1))
  (define (u32 x) (logand x #xFFFFFFFF))
  (with-output-to-string
    (lambda ()
      (printf "level ~a ~a\n" episode map)
      (printf "vertexes ~a\n" numvertexes)
      (vector-for-each (lambda (v) (printf "v ~a ~a\n" (vertex-x v) (vertex-y v))) vertexes)
      (printf "sectors ~a\n" numsectors)
      (vector-for-each
       (lambda (s)
         (let ([bb (sector-blockbox s)] [org (sector-soundorg s)])
           (printf "s ~a ~a ~a ~a ~a ~a ~a bb ~a ~a ~a ~a org ~a ~a lines ~a:"
                   (sector-floorheight s) (sector-ceilingheight s) (sector-floorpic s)
                   (sector-ceilingpic s) (sector-lightlevel s) (sector-special s) (sector-tag s)
                   (fxvector-ref bb 0) (fxvector-ref bb 1) (fxvector-ref bb 2) (fxvector-ref bb 3)
                   (degenmobj-x org) (degenmobj-y org) (sector-linecount s))
           (vector-for-each (lambda (l) (printf " ~a" (line-index l))) (sector-lines s))
           (newline)))
       sectors)
      (printf "sides ~a\n" numsides)
      (vector-for-each
       (lambda (d)
         (printf "d ~a ~a ~a ~a ~a ~a\n" (side-textureoffset d) (side-rowoffset d)
                 (side-toptexture d) (side-bottomtexture d) (side-midtexture d)
                 (sec-idx (side-sector d))))
       sides)
      (printf "lines ~a\n" numlines)
      (vector-for-each
       (lambda (l)
         (let ([bb (line-bbox l)] [sn (line-sidenum l)])
           (printf "l ~a ~a ~a ~a ~a ~a ~a ~a ~a bb ~a ~a ~a ~a ~a ~a ~a\n"
                   (vindex (line-v1 l)) (vindex (line-v2 l)) (line-dx l) (line-dy l)
                   (line-flags l) (line-special l) (line-tag l)
                   (fxvector-ref sn 0) (fxvector-ref sn 1)
                   (fxvector-ref bb 0) (fxvector-ref bb 1) (fxvector-ref bb 2) (fxvector-ref bb 3)
                   (line-slopetype l) (sec-idx (line-frontsector l)) (sec-idx (line-backsector l)))))
       lines)
      (printf "subsectors ~a\n" numsubsectors)
      (vector-for-each
       (lambda (ss)
         (printf "ss ~a ~a ~a\n" (sec-idx (subsector-sector ss)) (subsector-numlines ss)
                 (subsector-firstline ss)))
       subsectors)
      (printf "nodes ~a\n" numnodes)
      (vector-for-each
       (lambda (n)
         (printf "n ~a ~a ~a ~a" (node-x n) (node-y n) (node-dx n) (node-dy n))
         (vector-for-each
          (lambda (bb) (do ([k 0 (+ k 1)]) ((= k 4)) (printf " ~a" (fxvector-ref bb k))))
          (node-bbox n))
         (printf " ~a ~a\n" (fxvector-ref (node-children n) 0) (fxvector-ref (node-children n) 1)))
       nodes)
      (printf "segs ~a\n" numsegs)
      (vector-for-each
       (lambda (g)
         (printf "g ~a ~a ~a ~a ~a ~a ~a ~a\n" (vindex (seg-v1 g)) (vindex (seg-v2 g))
                 (seg-offset g) (seg-angle g) (side-index (seg-sidedef g))
                 (line-index (seg-linedef g)) (sec-idx (seg-frontsector g))
                 (sec-idx (seg-backsector g))))
       segs)
      (printf "blockmap ~a ~a ~a ~a ~a\n" bmaporgx bmaporgy bmapwidth bmapheight
              (let loop ([i 0] [h FNV_INIT])
                (if (= i (fxvector-length blockmaplump))
                    h
                    (loop (+ i 1) (fnv h (fxvector-ref blockmaplump i))))))
      (printf "reject ~a ~a\n" (bytevector-length rejectmatrix)
              (let loop ([i 0] [h FNV_INIT])
                (if (= i (bytevector-length rejectmatrix))
                    h
                    (loop (+ i 1) (fnv h (bytevector-u8-ref rejectmatrix i))))))
      (printf "end\n"))))

(define (read-lines path)
  (call-with-input-file path
    (lambda (p) (let loop ([acc '()])
                  (let ([l (get-line p)])
                    (if (eof-object? l) (reverse acc) (loop (cons l acc))))))))

;; First differing line, or #f.
(define (first-diff expected actual)
  (let loop ([e expected] [a actual] [n 1])
    (cond
      [(and (null? e) (null? a)) #f]
      [(or (null? e) (null? a) (not (string=? (car e) (car a))))
       (list n (if (null? e) 'eof (car e)) (if (null? a) 'eof (car a)))]
      [else (loop (cdr e) (cdr a) (+ n 1))])))

(w-init-multiple-files (list wad-path))
(set! gamemode shareware)
(r-init-textures)
(r-init-flats)
(do ([m 1 (+ m 1)]) ((> m 9))
  (p-setup-level 1 m 0 sk_medium #t)
  (let* ([name (map-lump-name 1 m)]
         [ours (with-input-from-string (level-dump 1 m (w-get-num-for-name name))
                 (lambda () (let loop ([acc '()])
                              (let ([l (get-line (current-input-port))])
                                (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))]
         [theirs (read-lines (format "build/oracle/levels/~a/level.txt" name))])
    (check (format "~a structures match oracle (~a lines)" name (length theirs))
           #f (first-diff theirs ours))))
(finish)
