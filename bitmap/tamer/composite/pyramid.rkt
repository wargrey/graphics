#lang racket

(require math/number-theory)

(require "../../constructor.rkt")
(require "../../composite.rkt")

(require "../../digitama/unsafe/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number->node
  (λ [i]
    (define datum (bitmap-text (number->string i)))
    (define sidelength (* (bitmap-height datum) 1.0))
    (define frame (bitmap-square sidelength))
    
    (bitmap-cc-superimpose frame datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list ([n (in-range 0 9)])
    (define pascal-triangle
      (apply append
             (for/list ([row (in-range n)])
               (for/list ([col (in-range (add1 row))])
                 (number->node (binomial row col))))))
    
    (bitmap-frame (bitmap-pyramid pascal-triangle))))
