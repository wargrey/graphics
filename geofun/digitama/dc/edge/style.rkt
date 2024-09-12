#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")

(require "../../geometry/anchor.rkt")

(require "../../../font.rkt")
(require "../../../paint.rkt")
(require "../../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Geo-Edge-Style-Make* S) (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (Option S)))
(define-type Geo-Edge-Style-Make (Geo-Edge-Style-Make* Geo-Edge-Style))

(struct geo-edge-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Maybe-Edge-Shape]
   [target-shape : Maybe-Edge-Shape])
  #:type-name Geo-Edge-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-edge-base-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Option-Edge-Shape]
   [target-shape : Option-Edge-Shape])
  #:type-name Geo-Edge-Base-Style
  #:transparent)

(define make-null-edge-style : (-> Geo-Edge-Base-Style)
  (lambda []
    (geo-edge-base-style #false #false (void) #false #false)))

(define default-geo-edge-base-style : (Parameterof (-> Geo-Edge-Base-Style))
  (make-parameter make-null-edge-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-select-line-paint : (-> (U Geo-Edge-Style Maybe-Stroke-Paint) Maybe-Stroke-Paint)
  (lambda [this-style]
    (define paint : Maybe-Stroke-Paint (if (geo-edge-style? this-style) (geo-edge-style-line-paint this-style) this-style))
    (define fallback-paint : Maybe-Stroke-Paint (geo-edge-base-style-line-paint ((default-geo-edge-base-style))))
    (cond [(void? paint) fallback-paint]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define geo-edge-select-source-shape : (-> Geo-Edge-Style Option-Edge-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Shape (geo-edge-style-source-shape this-style))
    (if (void? shape) (geo-edge-base-style-source-shape ((default-geo-edge-base-style))) shape)))

(define geo-edge-select-target-shape : (-> Geo-Edge-Style Option-Edge-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Shape (geo-edge-style-target-shape this-style))
    (if (void? shape) (geo-edge-base-style-target-shape ((default-geo-edge-base-style))) shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) geo-edge-style-construct : (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (Option (Geo-Edge-Style-Make* S)) (-> S) S)
  (lambda [source target mk-style mk-fallback-style]
    (or (and mk-style (mk-style source target))
        (mk-fallback-style))))
