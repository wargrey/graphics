#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../source.rkt")
(require "../visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../pangocairo.rkt")
  (require "../paint.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_blank create-surface width height density)
    (define-values (img cr) (create-surface width height density #true))
    (cairo_destroy cr)
    img)

  (define (dc_pattern create-surface width height background density)
    (define-values (img cr) (create-surface width height density #true))
    (cairo-render-background cr background)
    (cairo_destroy cr)
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_blank (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Flonum S))]
 [dc_pattern (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Fill-Source Flonum S))])
