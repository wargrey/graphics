#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")

(require racket/symbol)

(require geofun/digitama/geometry/anchor)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Edge-Style-Make* S) (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (U False Void S)))
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

(define dia-edge-id-merge : (-> Symbol (Option Symbol) Boolean Symbol)
  (lambda [source-id target-id directed?]
    (define src-id : String (symbol->immutable-string source-id))
    (define tgt-id : (Option String) (and target-id (symbol->immutable-string target-id)))
    
    (string->symbol
     (cond [(not tgt-id) (string-append src-id "-.")]
           [(not directed?) (string-append src-id "->" tgt-id)]
           [else (string-append src-id "->" tgt-id)]))))

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

(define dia-edge-select-font : (-> Dia-Edge-Style (Option Font))
  (lambda [this-style]
    (or (dia-edge-style-font this-style)
        (dia-edge-base-style-font ((default-dia-edge-base-style))))))

(define dia-edge-select-font-paint : (-> Dia-Edge-Style Option-Fill-Paint)
  (lambda [this-style]
    (or (dia-edge-style-font-paint this-style)
        (dia-edge-base-style-font-paint ((default-dia-edge-base-style))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-edge-style-construct : (-> Geo-Anchor-Name (Option Geo-Anchor-Name) (Option (Dia-Edge-Style-Make* S)) (-> S) S)
  (lambda [source target mk-style mk-fallback-style]
    (define maybe-style (and mk-style (mk-style source target)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
