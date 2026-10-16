;;; The playsim against the oracle: synthetic -nomonsters demos
;;; (tools/oracle/demos.py) played by both, compared tic by tic.
;;;
;;; Compared: gametic, gamestate, leveltime, prndindex, the player's
;;; position, angle and momentum, health, armor, the count and hash of all
;;; mobjs (every mobj's position, momentum, type, state, tics, health and
;;; flags), and the sector hash (heights, light, special). Not yet: rndindex,
;;; framehash and palettehash (the front end and sound aren't ported).
;;;
;;; A demo is compared up to the tic before the port first calls a
;;; stand-in for an unported function (p_unported.ss), e.g. the first item
;;; pickup or special line crossed.
(import (chezscheme) (doom w_wad) (doom r_local) (doom p_local) (doom d_main) (doom i_system)
        (doom doomdef) (doom doomstat) (doom trace))
(load "tests/lib.ss")

(define compared-fields
  '(gametic gamestate leveltime prndindex px py pz pangle pmomx pmomy health armor nmobjs mobjhash
    sectorhash))

(define (field-index name)
  (let loop ([l trace-field-names] [i 0])
    (if (eq? (car l) name) i (loop (cdr l) (+ i 1)))))

(define (select fields)
  (map (lambda (name) (list-ref fields (field-index name))) compared-fields))

(define (read-trace path)
  (call-with-input-file path
    (lambda (p)
      (let loop ([acc '()])
        (let ([line (get-line p)])
          (if (eof-object? line)
              (list->vector (reverse acc))
              (loop (cons (with-input-from-string (string-append "(" line ")") read) acc))))))))

(define (read-file path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define total-compared 0)

;; Play one demo; returns (values compared-tics first-mismatch-or-#f).
;; DEMO1-3 come from the IWAD (traces from make oracle-traces); the rest are
;; files in build/oracle/demos.
(define (iwad-demo? name) (member name '("demo1" "demo2" "demo3")))
(define (run-demo name)
  (let ([theirs (read-trace (if (iwad-demo? name)
                                (format "build/oracle/~a.trace" name)
                                (format "build/oracle/demos/~a.trace" name)))])
    (set! gametic 0)
    (set! unported-calls '())
    (set! unported-first-tic #f)
    (g-play-demo-bytes (if (iwad-demo? name)
                           (w-cache-lump-name (string-upcase name))
                           (read-file (format "build/oracle/demos/~a.lmp" name))))
    ;; The demo ends as a timedemo does: G_CheckDemoStatus raises the
    ;; "timed" error at the end marker, before that tic runs.
    (let loop ([i 0])
      (cond
        [(not (guard (c [(and (doom-error? c) (string=? "timed" (substring (condition-message c) 0 5))) #f])
                (g-ticker)
                #t))
         (values i #f)]
        [else
         (set! gametic (+ gametic 1))
         ;; A stand-in ran during this tic: stop before it.
         (if unported-first-tic
             (values i #f)
             (let ([ours (select (trace-fields 0 0))])
               (cond
                 [(= i (vector-length theirs))
                  (values i (list 'oracle-trace-ended-at i))]
                 [(equal? ours (select (vector-ref theirs i))) (loop (+ i 1))]
                 [else
                  (values i (list 'tic (+ i 1) 'fields
                                  (filter (lambda (x) x)
                                          (map (lambda (name o t) (and (not (equal? o t)) (list name o t)))
                                               compared-fields ours (select (vector-ref theirs i))))))])))]))))

(w-init-multiple-files (list wad-path))
(set! gamemode shareware)
(r-init 11 0)
(p-init)
(set! timingdemo #t)

(for-each
 (lambda (name)
   (let-values ([(n mismatch) (run-demo name)])
     (set! total-compared (+ total-compared n))
     (check (cond [mismatch (format "~a: diverges after ~a tics" name n)]
                  [(null? unported-calls) (format "~a: ~a tics match (whole demo)" name n)]
                  [else (format "~a: ~a tics match, then ~a" name n (car unported-calls))])
            #f mismatch)))
 (or (let ([only (getenv "DEMOS")]) (and only (with-input-from-string (string-append "(" only ")")
                                                (lambda () (map symbol->string (read))))))
     (append '("demo1" "demo2" "demo3")
             (list-sort string<? (map path-root (filter (lambda (f) (string=? (path-extension f) "lmp"))
                                                        (directory-list "build/oracle/demos")))))))
(printf "     (~a tics compared)\n" total-compared)
(finish)
