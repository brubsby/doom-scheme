;;; The playsim against the oracle: synthetic -nomonsters demos
;;; (tools/oracle/demos.py) played by both, compared tic by tic.
;;;
;;; Compared: gametic, gamestate, leveltime, the player's position, angle
;;; and momentum, health, armor, and the count and hash of all mobjs (every
;;; mobj's position, momentum, type, state, tics, health and flags). Not
;;; yet: prndindex and sectorhash (the light thinkers aren't ported), and
;;; rndindex, framehash and palettehash (the front end and sound aren't).
;;;
;;; A demo is compared up to the tic before the port first calls a
;;; stand-in for an unported function (p_unported.ss), e.g. the first item
;;; pickup or special line crossed.
(import (chezscheme) (doom w_wad) (doom r_local) (doom p_local) (doom g_game)
        (doom doomdef) (doom doomstat) (doom trace))
(load "tests/lib.ss")

(define compared-fields
  '(gametic gamestate leveltime px py pz pangle pmomx pmomy health armor nmobjs mobjhash))

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
(define (run-demo name)
  (let ([theirs (read-trace (format "build/oracle/demos/~a.trace" name))])
    (set! gametic 0)
    (set! unported-calls '())
    (set! unported-first-tic #f)
    (g-play-demo-bytes (read-file (format "build/oracle/demos/~a.lmp" name)))
    (let loop ([i 0])
      (cond
        [(not (g-ticker))
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

(for-each
 (lambda (name)
   (let-values ([(n mismatch) (run-demo name)])
     (set! total-compared (+ total-compared n))
     (check (cond [mismatch (format "~a: diverges after ~a tics" name n)]
                  [(null? unported-calls) (format "~a: ~a tics match (whole demo)" name n)]
                  [else (format "~a: ~a tics match, then ~a" name n (car unported-calls))])
            #f mismatch)))
 (let ([names (map path-root (filter (lambda (f) (string=? (path-extension f) "lmp"))
                                     (directory-list "build/oracle/demos")))])
   (list-sort string<? names)))
(printf "     (~a tics compared)\n" total-compared)
(finish)
