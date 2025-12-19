#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dots : (Listof Complex)
  (list 7+1i 11+1i 11+7i 9+7i 9+5i 2+5i 2+3i 7+3i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (plot-cartesian (list (lines dots)
                        (lines dots #:offset -2+3i)
                        (lines dots #:offset -1/2i #:scale 1/2 #:close? #true))))
