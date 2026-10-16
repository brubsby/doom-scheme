;;; Entry point: DOOM, with the command line it was given, e.g.
;;;   make run ARGS="-warp 1 3 -skill 4"
;;;   make run ARGS="-timedemo demo1"
;;; Without arguments it starts in the title loop (title, demos); until the
;;; menu exists (milestone 7), any key there starts E1M1 on skill 3, and
;;; ESC quits. Keys in the game are DOOM's defaults: arrows, ctrl fire,
;;; space use, shift run, alt strafe, , and . strafe, 1-7 weapons, TAB map.
(import (chezscheme) (doom d_main))
(exit (d-run (command-line-arguments)))
