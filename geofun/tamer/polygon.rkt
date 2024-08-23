#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 128)

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:fill 'azure)
                      (geo-circle r)
                      (geo-text n)))

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:inscribed? #true #:fill 'azure)
                      (geo-circle r #:fill 'ghostwhite)
                      (geo-text n)))
