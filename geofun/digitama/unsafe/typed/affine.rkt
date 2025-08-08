#lang typed/racket/base

(provide (all-defined-out))

(require "c.rkt")

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/draw/unsafe/cairo)

  (define cairo-transform
    (lambda [ctx xx yx xy yy x0 y0]
      (define m (make-cairo_matrix_t xx yx xy yy x0 y0))

      (cairo_transform ctx m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [cairo-transform (-> Cairo-Ctx Flonum Flonum Flonum Flonum Flonum Flonum Void)])
