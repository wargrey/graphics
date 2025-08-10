#lang racket

(require math/number-theory)

(require geofun/vector)
(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number->node
  (λ [i]
    (define datum (geo-text (number->string i)))
    (define sidelength (* (geo-height datum) 2.0))
    (define frame (geo-square sidelength))
    
    (geo-freeze (geo-cc-superimpose frame datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list ([n (in-range 0 9)])
    (define pascal-triangle
      (apply append
             (for/list ([row (in-range n)])
               (for/list ([col (in-range (add1 row))])
                 (number->node (binomial row col))))))
    
    (bitmap-frame (bitmap-pyramid pascal-triangle))))
