#lang typed/racket/base

(require bitmap)

(define stadium (bitmap-stadium 32 32 #:border (desc-stroke solid #:width 0.0) #:fill orange))
(bitmap-frame stadium #:border (desc-stroke dot-frame #:width 0.0))
