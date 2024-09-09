#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../visual/ctype.rkt")

(require "../../base.rkt")
(require "../../geometry/footprint.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../pangocairo.rkt")
  (require "../paint.rkt")
  
  (require (submod "../path.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_edge create-surface flwidth flheight footprints dx dy x-stroke? y-stroke? stroke density)
    (define-values (flw flh tx ty) (dc-path-window flwidth flheight dx dy stroke x-stroke? y-stroke?))
    (define-values (sfc cr) (create-surface flw flh density #true))
    
    (cairo_new_sub_path cr)
    (cairo_path cr footprints tx ty)
    
    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_edge (All (S) (-> (Cairo-Surface-Create S) 
                       Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Print) Flonum Flonum Boolean Boolean
                       (Option Paint) Positive-Flonum
                       S))])
