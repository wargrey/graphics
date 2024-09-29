#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")

(require geofun/digitama/geometry/anchor)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Free-Edge-Endpoint (U Geo-Anchor-Name Float-Complex))

(define-type (Dia-Edge-Style-Make* Src Tgt S) (-> Src Tgt (U False Void S)))
(define-type Dia-Edge-Style-Make (Dia-Edge-Style-Make* Geo-Anchor-Name (Option Geo-Anchor-Name) Dia-Edge-Style))
(define-type Dia-Free-Edge-Style-Make (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style))

(struct dia-edge-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Maybe-Edge-Tip-Shape]
   [target-shape : Maybe-Edge-Tip-Shape])
  #:type-name Dia-Edge-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-edge-base-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-shape : Option-Edge-Tip-Shape]
   [target-shape : Option-Edge-Tip-Shape])
  #:type-name Dia-Edge-Base-Style
  #:transparent)

(define make-null-edge-style : (-> Dia-Edge-Base-Style)
  (lambda []
    (dia-edge-base-style #false #false (void) #false #false)))

(define default-dia-edge-base-style : (Parameterof (-> Dia-Edge-Base-Style))
  (make-parameter make-null-edge-style))

(define dia-edge-id-merge : (-> (Option Dia-Free-Edge-Endpoint) (Option Dia-Free-Edge-Endpoint) Boolean (Option Symbol))
  (lambda [source-id target-id directed?]
    (define src-id : (Option String) (and source-id (if (complex? source-id) #false (geo-anchor->string source-id))))
    (define tgt-id : (Option String) (and target-id (if (complex? target-id) #false (geo-anchor->string target-id))))

    (and src-id
         (string->symbol
          (cond [(not tgt-id) (string-append src-id "-.")]
                [(not directed?) (string-append src-id "->" tgt-id)]
                [else (string-append src-id "->" tgt-id)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-select-line-paint : (-> (U Dia-Edge-Style Maybe-Stroke-Paint) Maybe-Stroke-Paint)
  (lambda [this-style]
    (define paint : Maybe-Stroke-Paint (if (dia-edge-style? this-style) (dia-edge-style-line-paint this-style) this-style))
    (define fallback-paint : Maybe-Stroke-Paint (dia-edge-base-style-line-paint ((default-dia-edge-base-style))))
    (cond [(void? paint) fallback-paint]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define dia-edge-select-source-shape : (-> Dia-Edge-Style Option-Edge-Tip-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Tip-Shape (dia-edge-style-source-shape this-style))
    (if (void? shape) (dia-edge-base-style-source-shape ((default-dia-edge-base-style))) shape)))

(define dia-edge-select-target-shape : (-> Dia-Edge-Style Option-Edge-Tip-Shape)
  (lambda [this-style]
    (define shape : Maybe-Edge-Tip-Shape (dia-edge-style-target-shape this-style))
    (if (void? shape) (dia-edge-base-style-target-shape ((default-dia-edge-base-style))) shape)))

(define dia-edge-select-font : (-> Dia-Edge-Style (Option Font))
  (lambda [this-style]
    (or (dia-edge-style-font this-style)
        (dia-edge-base-style-font ((default-dia-edge-base-style))))))

(define dia-edge-select-font-paint : (-> Dia-Edge-Style Option-Fill-Paint)
  (lambda [this-style]
    (or (dia-edge-style-font-paint this-style)
        (dia-edge-base-style-font-paint ((default-dia-edge-base-style))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Src Tgt S) dia-edge-style-construct : (-> Src Tgt (Option (Dia-Edge-Style-Make* Src Tgt S)) (-> S) S)
  (lambda [source target mk-style mk-fallback-style]
    (define maybe-style (and mk-style (mk-style source target)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
