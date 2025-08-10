#lang racket

(require geofun/vector)
(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define index->node
  (λ [i]
    (define datum (geo-text (number->string (add1 i))))
    (define r (geo-intrinsic-height datum))
    (define frame (geo-circle r))
    
    (geo-freeze (geo-cc-superimpose frame datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define nodes (build-list 48 index->node))
  (define subsize 16.0)
  
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)

  (bitmap-frame
   (time (bitmap-heap nodes subsize (* subsize 1.618))))

  (bitmap-frame
   (time (bitmap-heap nodes subsize (* subsize 0.618)))))
