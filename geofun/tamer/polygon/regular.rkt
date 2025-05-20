#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 64)

(define star-step : (-> Integer Integer)
  (lambda [n]
    (or (for/or : (Option Integer) ([k (in-range 2 (quotient n 2))])
          (and (> n k)
               (= (gcd n k) 1)
               k))
        2)))

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:fill 'azure)
                      (geo-star-polygon n (star-step n) r #:fill 'snow #:stroke 'grey)
                      (geo-circle r)
                      (geo-text n)))

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:inscribed? #true #:fill 'azure)
                      (geo-circle r #:fill 'ghostwhite)
                      (geo-star-polygon n (star-step n) r #:inscribed? #true #:stroke 'grey)
                      (geo-text n)))
