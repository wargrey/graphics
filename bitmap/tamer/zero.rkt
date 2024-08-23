#lang typed/racket/base

(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stadium (bitmap-stadium 32 32 #:stroke (desc-stroke #:width 0.0 #:dash 'solid) #:fill 'orange))
(bitmap-frame stadium #:border (desc-border #:width 0.0 #:style 'dotted))
