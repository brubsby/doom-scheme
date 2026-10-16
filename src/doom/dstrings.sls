;;; dstrings: user-visible strings (dstrings.h / d_englsh.h). Filled in as
;;; modules that use them are ported.
(library (doom dstrings)
  (export AMSTR_FOLLOWON AMSTR_FOLLOWOFF AMSTR_GRIDON AMSTR_GRIDOFF
          AMSTR_MARKEDSPOT AMSTR_MARKSCLEARED)
  (import (chezscheme))

  (define AMSTR_FOLLOWON "Follow Mode ON")
  (define AMSTR_FOLLOWOFF "Follow Mode OFF")
  (define AMSTR_GRIDON "Grid ON")
  (define AMSTR_GRIDOFF "Grid OFF")
  (define AMSTR_MARKEDSPOT "Marked Spot")
  (define AMSTR_MARKSCLEARED "All Marks Cleared"))
