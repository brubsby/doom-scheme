;;; p_saveg's struct layouts against the oracle's compiler: linuxdoom
;;; savegames are memcpy'd 32-bit structs, so every field's offset and
;;; every struct's size must be the C's (build/oracle/saveg_probe prints
;;; them). An array field is checked at its first element. (The savegames
;;; themselves are compared in tests/frames.ss.)
(import (chezscheme) (doom p_local))
(load "tests/lib.ss")

(define probe
  (let ([p (car (process "build/oracle/saveg_probe"))])
    (let loop ([acc '()])
      (let ([line (get-line p)])
        (if (eof-object? line)
            (reverse acc)
            (loop (cons (with-input-from-string (string-append "(" line ")") read) acc)))))))

(check "the probe ran" #t (> (length probe) 100))

(for-each
 (lambda (e)
   (let* ([s (car e)] [layout (cdr e)] [offsets (layout-offsets layout)]
          [lines (filter (lambda (l) (eq? (car l) s)) probe)])
     (check (format "~a: ~a fields at C's offsets, sizeof ~a" s (- (length lines) 1) (layout-size layout))
            (map (lambda (l) (if (eq? (cadr l) 'sizeof) (list 'sizeof (layout-size layout)) (cdr l))) lines)
            (map (lambda (l)
                   (if (eq? (cadr l) 'sizeof)
                       (list 'sizeof (layout-size layout))
                       (let ([o (or (assq (cadr l) offsets)
                                    (assq (string->symbol (format "~a[0]" (cadr l))) offsets))])
                         (list (cadr l) (and o (cdr o))))))
                 lines))))
 saveg-layouts)

(finish)
