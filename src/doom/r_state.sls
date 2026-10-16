;;; r_state: the loaded level's geometry (r_state.h). p_setup fills these
;;; in; the renderer, playsim and automap read them.
(library (doom r_state)
  (export numvertexes vertexes numsegs segs numsectors sectors
          numsubsectors subsectors numnodes nodes numlines lines numsides sides)
  (import (chezscheme) (doom global))

  (define-global numvertexes 0)
  (define-global vertexes (vector))
  (define-global numsegs 0)
  (define-global segs (vector))
  (define-global numsectors 0)
  (define-global sectors (vector))
  (define-global numsubsectors 0)
  (define-global subsectors (vector))
  (define-global numnodes 0)
  (define-global nodes (vector))
  (define-global numlines 0)
  (define-global lines (vector))
  (define-global numsides 0)
  (define-global sides (vector)))
