;;; w_wad: the WAD lump directory and lump cache.
;;; Lumps are bytevectors. A cached lump is the same bytevector on every call,
;;; so in-place edits persist as they do in C. Zone purge tags are accepted
;;; but ignored because the GC owns lifetime.
(library (doom w_wad)
  (export w-init-multiple-files w-add-file w-num-lumps w-check-num-for-name
          w-get-num-for-name w-lump-length w-lump-name w-read-lump
          w-cache-lump-num w-cache-lump-name)
  (import (chezscheme) (doom i_system))

  ;; lumpinfo_t: name is the lump's name up to the first NUL (C keeps it
  ;; strncpy-padded, which compares the same).
  (define-record-type (lumpinfo-t make-lumpinfo lumpinfo?)
    (fields (immutable name lumpinfo-name) (immutable data lumpinfo-data)
            (immutable position lumpinfo-position) (immutable size lumpinfo-size)))

  (define lumpinfo (make-vector 0))   ; vector of lumpinfo
  (define lumpcache (make-vector 0))  ; per-lump cached bytevector or #f
  ;; name -> highest lump index with that name. W_CheckNumForName scans
  ;; backwards, so the last lump added wins (PWADs override the IWAD).
  (define lumpnames (make-hashtable string-hash string=?))

  (define (w-num-lumps) (vector-length lumpinfo))

  (define (read-file path)
    (call-with-port (open-file-input-port path) get-bytevector-all))

  (define (name8 bv off)
    ;; 8-byte, NUL-padded name field.
    (let loop ([i 0] [acc '()])
      (if (or (fx= i 8) (fx= (bytevector-u8-ref bv (fx+ off i)) 0))
          (list->string (reverse acc))
          (loop (fx+ i 1) (cons (integer->char (bytevector-u8-ref bv (fx+ off i))) acc)))))

  ;; ExtractFileBase: up to 8 chars of the basename before the extension,
  ;; uppercased.
  (define (extract-file-base path)
    (let* ([base (path-root (path-last path))]
           [base (if (> (string-length base) 8) (substring base 0 8) base)])
      (string-upcase base)))

  (define (w-add-file filename)
    (cond
      [(not (file-exists? filename))
       (printf " couldn't open ~a\n" filename)]
      [else
       (printf " adding ~a\n" filename)
       (let* ([bv (read-file filename)]
              [n (string-length filename)]
              [new
               (if (not (and (>= n 3) (string-ci=? (substring filename (- n 3) n) "wad")))
                   ;; Single lump file.
                   (list (make-lumpinfo (extract-file-base filename) bv 0 (bytevector-length bv)))
                   (let ([id (utf8->string (bytevector-copy/range bv 0 4))])
                     (unless (or (string=? id "IWAD") (string=? id "PWAD"))
                       (i-error "Wad file ~a doesn't have IWAD or PWAD id\n" filename))
                     (let ([numlumps (bytevector-s32-ref bv 4 (endianness little))]
                           [infotableofs (bytevector-s32-ref bv 8 (endianness little))])
                       (let loop ([i 0] [acc '()])
                         (if (= i numlumps)
                             (reverse acc)
                             (let ([o (+ infotableofs (* i 16))])
                               (loop (+ i 1)
                                     (cons (make-lumpinfo
                                            (name8 bv (+ o 8)) bv
                                            (bytevector-s32-ref bv o (endianness little))
                                            (bytevector-s32-ref bv (+ o 4) (endianness little)))
                                           acc))))))))]
              [start (vector-length lumpinfo)])
         (set! lumpinfo (list->vector (append (vector->list lumpinfo) new)))
         (set! lumpcache (list->vector (append (vector->list lumpcache) (map (lambda (_) #f) new))))
         (let loop ([i start] [l new])
           (unless (null? l)
             (hashtable-set! lumpnames (lumpinfo-name (car l)) i)
             (loop (+ i 1) (cdr l)))))]))

  (define (bytevector-copy/range bv start end)
    (let ([out (make-bytevector (- end start))])
      (bytevector-copy! bv start out 0 (- end start))
      out))

  (define (w-init-multiple-files filenames)
    (set! lumpinfo (make-vector 0))
    (set! lumpcache (make-vector 0))
    (hashtable-clear! lumpnames)
    (for-each w-add-file filenames)
    (when (= (w-num-lumps) 0)
      (i-error "W_InitFiles: no files found")))

  ;; Case-insensitive on the query (strupr), first 8 chars only. The stored
  ;; names are compared as-is, as in C.
  (define (w-check-num-for-name name)
    (let* ([name (if (> (string-length name) 8) (substring name 0 8) name)]
           [name (string-upcase name)])
      (hashtable-ref lumpnames name -1)))

  (define (w-get-num-for-name name)
    (let ([i (w-check-num-for-name name)])
      (when (= i -1) (i-error "W_GetNumForName: ~a not found!" name))
      i))

  (define (check-lump who lump)
    (unless (and (fixnum? lump) (fx>= lump 0) (fx< lump (w-num-lumps)))
      (i-error "~a: ~a >= numlumps" who lump)))

  (define (w-lump-length lump)
    (check-lump "W_LumpLength" lump)
    (lumpinfo-size (vector-ref lumpinfo lump)))

  (define (w-lump-name lump)
    (check-lump "W_LumpName" lump)
    (lumpinfo-name (vector-ref lumpinfo lump)))

  ;; W_ReadLump: a fresh copy of the lump's bytes.
  (define (w-read-lump lump)
    (check-lump "W_ReadLump" lump)
    (let* ([l (vector-ref lumpinfo lump)] [pos (lumpinfo-position l)])
      (bytevector-copy/range (lumpinfo-data l) pos (+ pos (lumpinfo-size l)))))

  (define w-cache-lump-num
    (case-lambda
      [(lump) (w-cache-lump-num lump 'PU_CACHE)]
      [(lump tag)
       (check-lump "W_CacheLumpNum" lump)
       (or (vector-ref lumpcache lump)
           (let ([bv (w-read-lump lump)])
             (vector-set! lumpcache lump bv)
             bv))]))

  (define w-cache-lump-name
    (case-lambda
      [(name) (w-cache-lump-num (w-get-num-for-name name))]
      [(name tag) (w-cache-lump-num (w-get-num-for-name name) tag)])))
