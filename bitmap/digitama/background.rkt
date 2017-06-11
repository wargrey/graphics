#lang typed/racket/base

(provide (all-defined-out))

(define generic-pen-width-map : (-> Symbol Flonum Flonum)
  (lambda [width inherited-width]
    (cond [(eq? width 'thin) 1.0]
          [(eq? width 'medium) 3.0]
          [(eq? width 'thick) 5.0]
          [else inherited-width])))
