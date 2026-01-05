#lang typed/racket/base

(require exprfun/matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gapsize : Nonnegative-Flonum 16.0)
(define cellsize : Nonnegative-Flonum 32.0)
(define matrix : (Listof Byte) (build-list 16 (λ [[i : Index]] (remainder (random 256) #xFF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define blank
  ((inst $array Byte) #:border 'RoyalBlue #:padding 8.0 #:margin 8.0
                      null cellsize))

(define row-header
  ((inst $array Byte) #:row-desc #("1" "2" "Three") #:gap gapsize
                      #:ncols 4
                      matrix cellsize))

(define hole-data
  ((inst $array Byte) #:ncols 4 #:gap gapsize
                      #:hole? odd?
                      matrix cellsize))

(define col-header@top
  ((inst $array Byte) #:col-desc '("1" "2" "Three" "4") #:gap gapsize
                      #:col-header-top? #true
                      #:mask? >
                      matrix cellsize))

(define col-header@bottom
  ((inst $array Byte) #:col-desc #("1" "2" "Three" "4") #:gap gapsize
                      #:col-header-top? #false
                      #:mask? <
                      matrix cellsize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  blank

  (geo-hc-append #:gapsize (* gapsize 2.0)
                 row-header
                 hole-data)

  (geo-hc-append #:gapsize (* gapsize 2.0)
                 col-header@top
                 col-header@bottom))
