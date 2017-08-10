#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../misc.rkt")
(require "../digitama/unsafe/convert.rkt")

(unsafe-require/typed
 "cairo/gradient.rkt"
 [cairo-gradient (-> Flonum Flonum Flonum (Listof Flonum) (Listof Flonum) Flonum Bitmap)])

(define ball (cairo-gradient 128.0 128.0 76.8 '(1.0 1.0 1.0 1.0) '(0.0 0.0 0.0 1.0) 1.0))

ball
(bitmap-alter-density ball 2.0)
