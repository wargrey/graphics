#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require bitmap)

(unsafe-require/typed
 geofun/tamer/pangocairo/gradient
 [cairo-gradient (-> Flonum Flonum Flonum (Listof Flonum) (Listof Flonum) Flonum Bitmap)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ball (cairo-gradient 128.0 128.0 76.8 '(1.0 1.0 1.0 1.0) '(0.0 0.0 0.0 1.0) 2.0))

ball
(bitmap-alter-density ball 1.0)
