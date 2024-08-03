#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (require pangocairo/digitama/unsafe/pangocairo)
  (require "surface.rkt")
  
  (define (bitmap_invalid flwidth flheight density)
    (define-values (img cr) (create-invalid-bitmap flwidth flheight density #true))
    (cairo_destroy cr)
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_invalid (-> Flonum Flonum Flonum Bitmap)])
