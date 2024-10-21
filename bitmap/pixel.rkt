#lang typed/racket/base

(provide (all-defined-out) ARGB-Map)

(require "digitama/unsafe/image.rkt")
(require "digitama/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-map : (-> Bitmap ARGB-Map Bitmap)
  (lambda [src λargb-map]
    (λbitmap_map (bitmap-surface src) (bitmap-density src) #false λargb-map)))
