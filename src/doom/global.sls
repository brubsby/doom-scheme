;;; define-global: a module-level variable that other libraries can both read
;;; and set!, like a C global. R6RS forbids assigning imported variables, so
;;; the value lives in a box behind an identifier macro:
;;;
;;;   (define-global usegamma 0)   ; in v_video, exported as usegamma
;;;   usegamma                     ; anywhere: reads the box
;;;   (set! usegamma 1)            ; anywhere: writes the box
(library (doom global)
  (export define-global)
  (import (chezscheme))

  (define-syntax define-global
    (lambda (x)
      (syntax-case x ()
        [(_ name init)
         (with-syntax ([cell (datum->syntax
                              #'name
                              (string->symbol
                               (string-append (symbol->string (syntax->datum #'name))
                                              "/cell")))])
           #'(begin
               (define cell (box init))
               (define-syntax name
                 (identifier-syntax
                  [id (unbox cell)]
                  [(set! id e) (set-box! cell e)]))))]))))
