#lang typed/racket/base

(require diafun/matrix)
(require geofun/vector)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gapsize : Nonnegative-Flonum 24.0)
(define cellsize : Nonnegative-Flonum 42.0)
(define matrix : (Listof Byte) (build-list 16 (λ [[i : Index]] (remainder (random 256) #xFF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define blank
  ((inst dia-array Byte) #:border 'RoyalBlue #:padding 8.0 #:margin 8.0
                         null))

(define row-header
  ((inst dia-array Byte) #:row-desc #("1" "2" "Three")
                         #:ncols 4 #:gap gapsize
                         matrix))

(define hole-data
  ((inst dia-array Byte) #:ncols 4 #:gap gapsize
                         #:hole? odd?
                         matrix))

(define col-header@top
  ((inst dia-array Byte) #:col-desc #("1" "2" "Three" "4" "5")
                         #:col-header-rotate -1.57 #:gap gapsize
                         #:mask? > #:ncols 4
                         matrix cellsize))

(define col-header@bottom
  ((inst dia-array Byte) #:col-desc #("1" "2" "Three" "4" "5") #:col-header-top? #false
                         #:col-header-rotate -1.57 #:gap gapsize
                         #:mask? < #:ncols 4
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
