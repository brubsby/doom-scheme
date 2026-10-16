;;; d_think: thinker_t, the base of everything on the thinker list (mobjs,
;;; doors, lights, ...). Subtypes are child record types, mirroring C's
;;; "struct starts with a thinker_t" layout. `func` is the think procedure,
;;; #f for none (NULL), or 'removed (C's function.acv == -1).
(library (doom d_think)
  (export thinker make-thinker thinker?
          thinker-prev thinker-prev-set! thinker-next thinker-next-set!
          thinker-func thinker-func-set!)
  (import (chezscheme) (doom struct))

  (define-struct thinker () (prev #f) (next #f) (func #f)))
