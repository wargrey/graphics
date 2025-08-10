#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 64)

(for/list : (Listof Geo) ([theta (in-range 0.0 361.0 10.0)])
  (geo-frame
   (geo-arrow 4 r (degrees->radians theta) #:fill theta #:stroke #false)))

(for/list : (Listof Geo) ([theta (in-range 0.0 361.0 10.0)])
  (geo-frame
   (geo-cc-superimpose (geo-arrowhead r (degrees->radians theta) #:stroke theta)
                       (geo-text theta))))

(for/list : (Listof Geo) ([theta (in-range 0 361 30)])
  (geo-frame
   (geo-cc-superimpose (geo-arrow r 64 (degrees->radians theta) #:fill 'azure)
                       (geo-text theta))))

