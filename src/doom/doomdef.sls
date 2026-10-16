;;; doomdef: global constants.
(library (doom doomdef)
  (export SCREENWIDTH SCREENHEIGHT)
  (import (chezscheme))

  ;; Fixed at vanilla's 320x200 for now. Everything derives from these two;
  ;; see DESIGN.md "Future: hi-res and uncapped".
  (define SCREENWIDTH 320)
  (define SCREENHEIGHT 200))
