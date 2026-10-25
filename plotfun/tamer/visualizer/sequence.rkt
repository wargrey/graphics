#lang typed/racket/base

(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expt3-ones : (-> Integer Integer)
  (lambda [x]
    (remainder (expt 3 (max 0 x)) 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vtree : (Listof Plot-Visualizer)
  (list (points (for/list ([n (in-range 0 11)])
                  (make-rectangular n (expt3-ones n))))))

(define sequence
  (plot-cartesian #:x-range (cons 0 10) #:y-range (cons 0 10)
                  vtree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  sequence)
