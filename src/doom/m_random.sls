;;; m_random: the 256-entry pseudo-random table. P_Random drives the
;;; playsim (demo-synced); M_Random is for everything else.
(library (doom m_random)
  (export rndtable rndindex prndindex p-random m-random m-clear-random)
  (import (chezscheme) (doom global) (doom m_random_data))

  (define-global rndindex 0)
  (define-global prndindex 0)

  (define (p-random)
    (set! prndindex (fxand (fx+ prndindex 1) #xff))
    (bytevector-u8-ref rndtable prndindex))

  (define (m-random)
    (set! rndindex (fxand (fx+ rndindex 1) #xff))
    (bytevector-u8-ref rndtable rndindex))

  (define (m-clear-random)
    (set! rndindex 0)
    (set! prndindex 0)))
