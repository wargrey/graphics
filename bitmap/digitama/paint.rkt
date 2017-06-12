#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "misc.rkt")

;;; https://svgwg.org/svg2-draft/painting.html

(define-type Color (U Symbol Integer FlColor))

(struct: flcolor : FlColor ())

(struct: rgba : FlRGBA flcolor
  ([red : Flonum]
   [green : Flonum]
   [blue : Flonum]
   [alpha : Flonum]))

(struct: stroke : Stroke
  ([color : FlRGBA]
   [opacity : Flonum]
   [width : Flonum]
   [cap : Symbol]
   [join : Symbol]
   [dash : (Vectorof Flonum)]
   [offset : Flonum]))

(struct: fill : Fill
  ([color : FlRGBA]
   [opacity : Flonum]
   [rule : Symbol]))