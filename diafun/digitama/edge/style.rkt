#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")

(require geofun/digitama/geometry/anchor)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Edge-Style-Make* S) (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (Option S)))
(define-type Dia-Edge-Style-Make (Dia-Edge-Style-Make* Dia-Edge-Style))

(struct dia-edge-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Maybe-Edge-Shape]
   [target-shape : Maybe-Edge-Shape])
  #:type-name Dia-Edge-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-edge-base-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Option-Edge-Shape]
   [target-shape : Option-Edge-Shape])
  #:type-name Dia-Edge-Base-Style
  #:transparent)

(define make-null-edge-style : (-> Dia-Edge-Base-Style)
  (lambda []
    (dia-edge-base-style #false #false (void) #false #false)))

(define default-dia-edge-base-style : (Parameterof (-> Dia-Edge-Base-Style))
  (make-parameter make-null-edge-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-select-line-paint : (-> (U Dia-Edge-Style Maybe-Stroke-Paint) Maybe-Stroke-Paint)
  (lambda [this-style]
    (define paint : Maybe-Stroke-Paint (if (dia-edge-style? this-style) (dia-edge-style-line-paint this-style) this-style))
    (define fallback-paint : Maybe-Stroke-Paint (dia-edge-base-style-line-paint ((default-dia-edge-base-style))))
    (cond [(void? paint) fallback-paint]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define dia-edge-select-source-shape : (-> Dia-Edge-Style Option-Edge-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Shape (dia-edge-style-source-shape this-style))
    (if (void? shape) (dia-edge-base-style-source-shape ((default-dia-edge-base-style))) shape)))

(define dia-edge-select-target-shape : (-> Dia-Edge-Style Option-Edge-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Shape (dia-edge-style-target-shape this-style))
    (if (void? shape) (dia-edge-base-style-target-shape ((default-dia-edge-base-style))) shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-edge-style-construct : (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (Option (Dia-Edge-Style-Make* S)) (-> S) S)
  (lambda [source target mk-style mk-fallback-style]
    (or (and mk-style (mk-style source target))
        (mk-fallback-style))))
