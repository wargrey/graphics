#lang typed/racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/background.rkt")
(require "color.rkt")

(require "digitama/unsafe/source.rkt")

(define-type CSS-Gradient-Stop-Color (Pairof Real FlRGBA))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-stroke : (Parameterof Stroke) (make-parameter (stroke transparent (generic-pen-width-map 'medium 3.0) 'none)))

(define desc-stroke : (->* () (Stroke #:color (Option Color) #:width (U Real Symbol False) #:style (Option Symbol)) Stroke)
  (lambda [[base (default-stroke)] #:color [color #false] #:width [width #false] #:style [style #false]]
    (stroke (if color (rgb* color) (stroke-color base))
            (cond [(memq style '(hidden none)) 0.0]
                  [(and (real? width) (positive? width)) (real->double-flonum width)]
                  [(symbol? width) (generic-pen-width-map width (stroke-width base))]
                  [else (stroke-width base)])
            (or style (stroke-style base)))))
