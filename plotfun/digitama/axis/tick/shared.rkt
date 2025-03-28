#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-range-refine : (-> Real Real Real (Values Real Real Integer))
  (lambda [left right step]
    (define start (* (ceiling (/ left step)) step))
    (define end (* (floor (/ right step)) step))
    (define num (+ 1 (exact-round (/ (- end start) step))))

    (values start end num)))
