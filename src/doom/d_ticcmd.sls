;;; d_ticcmd: ticcmd_t, one tic of player input.
(library (doom d_ticcmd)
  (export ticcmd make-ticcmd ticcmd?
          ticcmd-forwardmove ticcmd-forwardmove-set! ticcmd-sidemove ticcmd-sidemove-set!
          ticcmd-angleturn ticcmd-angleturn-set! ticcmd-consistancy ticcmd-consistancy-set!
          ticcmd-chatchar ticcmd-chatchar-set! ticcmd-buttons ticcmd-buttons-set!)
  (import (chezscheme) (doom struct))

  ;; forwardmove/sidemove are signed char, angleturn/consistancy short,
  ;; chatchar/buttons byte.
  (define-struct ticcmd ()
    (forwardmove 0) (sidemove 0) (angleturn 0) (consistancy 0) (chatchar 0) (buttons 0)))
