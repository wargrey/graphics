#lang typed/racket

;;; https://www.cairographics.org/operators

(require "../../constructor.rkt")
(require "../../composite.rkt")
(require "../../digitama/composite.rkt")
(require "../../digitama/base.rkt")
(require "../../digitama/unsafe/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define src : Bitmap (bitmap-rectangle 120 90 #:border #false #:fill (rgba 0.0 0.0 0.9 0.4)))
(define dest : Bitmap (bitmap-rectangle 120 90 #:border #false #:fill (rgba 0.7 0.0 0.0 0.8)))

(for/list : (Listof (Pairof Symbol Bitmap)) ([op (in-list bitmap-composition-operators)])
  (cons op (bitmap-composite op dest 40.0 30.0 src)))
