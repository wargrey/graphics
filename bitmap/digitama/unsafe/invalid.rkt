#lang typed/racket/base

(provide (all-defined-out))

(require "convert.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  
  (define (bitmap_invalid density)
    (define-values (img cr) (make-invalid-cairo-image 1.0 1.0 density #true))
    (cairo_destroy cr)
    img))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_invalid (-> Flonum Bitmap)])
