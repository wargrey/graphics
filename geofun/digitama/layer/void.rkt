#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")

(require "../self.rkt")
(require "../convert.rkt")
(require "../geometry/ink.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-void-geo : Geo
  (let ([zero-ext (λ [self] (values 0.0 0.0 geo-zero-ink))])
    (create-geometry-object geo #:with [(string->unreadable-symbol "the-void-geo") void zero-ext geo-zero-pads])))

(define the-void-layer : (GLayerof Geo) (glayer the-void-geo 0.0 0.0 0.0 0.0))
(define the-void-layers : (GLayer-Groupof Geo) (glayer-group 0.0 0.0 (list the-void-layer)))
;(define the-void-group : Geo:Group (make-geo:group (string->unreadable-symbol "the-void-group") #false #false the-void-layers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-void? : (-> Any Boolean : #:+ Geo)
  (lambda [v]
    (and (geo? v)
         (eq? v the-void-geo))))

(define geo-void-layer? : (-> Any Boolean : #:+ (GLayerof Geo))
  (lambda [v]
    (and (glayer? v)
         (eq? v the-void-layer))))
