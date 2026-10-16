;;; define-struct: a C struct as a mutable record with defaults.
;;;
;;;   (define-struct vertex () (x 0) (y 0))
;;;   (define-struct mobj (degenmobj) (snext #f) (angle 0) ...)
;;;
;;; defines the record type NAME (a subtype of PARENT if given), the predicate
;;; NAME?, accessors NAME-FIELD and mutators NAME-FIELD-SET! for every field,
;;; and a constructor MAKE-NAME that takes either no arguments (every field,
;;; parents' included, gets its default, like a zeroed C struct; default
;;; expressions are evaluated per instance) or one argument per field of NAME
;;; itself (parent fields still get their defaults).
(library (doom struct)
  (export define-struct)
  (import (chezscheme))

  (define-syntax define-struct
    (lambda (x)
      (define (id ctx . parts)
        (datum->syntax ctx
          (string->symbol
           (apply string-append
                  (map (lambda (p) (if (string? p) p (symbol->string (syntax->datum p))))
                       parts)))))
      (syntax-case x ()
        [(_ name () (field default) ...)
         (with-syntax ([make (id #'name "make-" #'name)]
                       [pred (id #'name #'name "?")]
                       [(arg ...) (generate-temporaries #'(field ...))])
           #'(define-record-type (name make pred)
               (fields (mutable field) ...)
               (protocol
                (lambda (p)
                  (case-lambda
                    [() (p default ...)]
                    [(arg ...) (p arg ...)])))))]
        [(_ name (super) (field default) ...)
         (with-syntax ([make (id #'name "make-" #'name)]
                       [pred (id #'name #'name "?")]
                       [(arg ...) (generate-temporaries #'(field ...))])
           #'(define-record-type (name make pred)
               (parent super)
               (fields (mutable field) ...)
               (protocol
                (lambda (n)
                  (case-lambda
                    [() ((n) default ...)]
                    [(arg ...) ((n) arg ...)])))))]))))
