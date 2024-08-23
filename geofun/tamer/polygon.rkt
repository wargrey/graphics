#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 64)

(geo-frame (geo-polyline #:stroke (desc-stroke #:color 'royalblue #:width 2.0) '(0+0i -10+20i 60 -10-20i) #:window -1.0-1.0i))

(geo-polygon #:stroke 'green #:fill 'burlywood '(0+0i -10+20i 60 -10-20i) #:window -1.0-1.0i)

(geo-polygon #:fill 'silver '((0 . 0) (100 . 0)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (99.5 . 0.5) (0.5 . 0.5)))
(geo-polygon #:fill 'silver '((0 . 0) (100 . 0) (95 . 5) (5 . 5)))

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:fill 'azure)
                      (geo-circle r)
                      (geo-text n)))

(for/list : (Listof Geo) ([n (in-range 17)])
  (geo-cc-superimpose (geo-regular-polygon n r #:inscribed? #true #:fill 'azure)
                      (geo-circle r #:fill 'ghostwhite)
                      (geo-text n)))
