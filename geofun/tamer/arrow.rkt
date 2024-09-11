#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 64)

(for/list : (Listof Geo) ([theta (in-range 0 361 30)])
  (geo-cc-superimpose (geo-arrowhead r theta #:radian? #false #:fill 'azure)
                      (geo-text theta)))
