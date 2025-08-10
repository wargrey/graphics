#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/base)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stadium (geo-stadium 32 32 #:stroke (desc-stroke #:width 0.0 #:dash 'solid) #:fill 'orange))
(define frame (bitmap-frame (geo-freeze stadium) #:border (desc-border #:width 2.0 #:style 'dotted)))

(module+ main
  stadium
  frame)
