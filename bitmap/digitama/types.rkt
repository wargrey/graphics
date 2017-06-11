#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "misc.rkt")

(define-type Bitmap (Instance Bitmap%))
(define-type Color (U Symbol Integer FlColor))

(struct: flcolor : FlColor ())
(struct: rgba : FlRGBA flcolor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum]))
(struct: stroke : Stroke ([color : FlRGBA] [width : Flonum] [dasharray : (Vectorof Flonum)] [dashoffset : Flonum]))
(struct: fill : Fill ([color : FlRGBA] [width : Flonum] [style : Symbol]))
