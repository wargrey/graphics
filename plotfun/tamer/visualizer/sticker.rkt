#lang typed/racket

(require geofun/vector)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vtree : (Listof Plot-Visualizer)
  (list (sticker #:scale? #f (geo-gear 16 32)  100+300i 'lt)
        (sticker #:scale? #t (geo-gear  8 32)  600+100i 'lt)
        (sticker #:scale? #t (geo-star-polygon 5 2 32 -1.57) 600+500i)
        (sticker #:scale? #f (geo-art-text "Art Text") 400+300i)))

(define sticker-screen
  (plot-cartesian #:x-range (cons 0 800)
                  #:y-range (cons 0 600)
                  #:screen? #true
                  vtree))

(define sticker-cart
  (plot-cartesian #:x-range (cons 0 800)
                  #:y-range (cons 0 600)
                  vtree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  sticker-screen
  sticker-cart)
