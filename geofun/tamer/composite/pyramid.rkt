#lang racket

(require math/number-theory)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number->node
  (λ [i]
    (define datum (geo-text (number->string i)))
    (define frame (geo-square 20.0))
    
    (geo-cc-superimpose frame datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list ([n (in-range 0 9)])
    (define pascal-triangle
      (append (apply append
                     (for/list ([row (in-range n)])
                       (for/list ([col (in-range (add1 row))])
                         (number->node (binomial row col)))))
              (list (number->node 0))))
    
    (geo-frame (geo-pyramid pascal-triangle #:extra (and (even? n) (geo-blank))))))
