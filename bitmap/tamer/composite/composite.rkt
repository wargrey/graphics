#lang typed/racket

;;; https://www.cairographics.org/operators

(require "../../draw.rkt")
(require "../../constructor.rkt")
(require "../../composite.rkt")
(require "../../digitama/composite.rkt")
(require "../../digitama/unsafe/convert.rkt")

(define src : Bitmap (bitmap-rectangle 120 90 #:border #false #:fill (rgba 0.0 0.0 0.9 0.4)))
(define dest : Bitmap (bitmap-rectangle 120 90 #:border #false #:fill (rgba 0.7 0.0 0.0 0.8)))

(for/list : (Listof (Pairof Symbol Bitmap)) ([op (in-list bitmap-blend-modes)])
  (cons op (bitmap-composite dest 40.0 30.0 src op)))
