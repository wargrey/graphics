#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/path/self)
(require geofun/digitama/edge/tip/self)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/geometry/anchor)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Free-Edge-Endpoint (U Geo-Anchor-Name Float-Complex))

(define-type (Dia-Edge-Style-Make* Src Tgt S) (-> Src Tgt (Listof Geo-Path-Labels) (U S Void False)))
(define-type (Dia-Edge-Style-Make S) (Dia-Edge-Style-Make* Geo (Option Geo) S))
(define-type (Dia-Free-Edge-Style-Make S) (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint S))

(struct dia-edge-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [width : (Option Flonum)]
   [color : (U Color Void False)]
   [dash : (Option Stroke-Dash+Offset)]
   [source-tip : Maybe-Geo-Tip]
   [target-tip : Maybe-Geo-Tip]
   [label-rotate? : (U Boolean Void)]
   [label-inline? : (U Boolean Void)]
   [label-distance : (U Flonum Void)])
  #:type-name Dia-Edge-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-edge-base-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-tip : Option-Geo-Tip]
   [target-tip : Option-Geo-Tip]
   [label-rotate? : Boolean]
   [label-inline? : Boolean]
   [label-distance : (Option Flonum)])
  #:type-name Dia-Edge-Base-Style
  #:transparent)

(define make-null-edge-style : (-> Dia-Edge-Base-Style)
  (lambda []
    (dia-edge-base-style #false #false (void)
                         #false #false #false #false
                         #false)))

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
(define dia-edge-select-line-paint : (-> (U Dia-Edge-Style Maybe-Stroke-Paint) Option-Stroke-Paint)
  (lambda [s]
    (define fallback-paint : (Option Stroke) (stroke-paint->source* (dia-edge-base-style-line-paint ((default-dia-edge-base-style)))))
    
    (cond [(void? s) fallback-paint]
          [(or (not s) (stroke? s)) s]
          [(dia-edge-style? s)
           (let*-values ([(c) (dia-edge-style-color s)]
                         [(d+o) (dia-edge-style-dash s)]
                         [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
             (desc-stroke #:color (and (not (void? c)) c)
                          #:width (dia-edge-style-width s)
                          #:dash dash #:offset offset
                          (if (stroke? fallback-paint) fallback-paint (default-stroke))))]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color s)]
          [else s])))

(define dia-edge-select-source-tip : (-> Dia-Edge-Style Option-Geo-Tip)
  (lambda [s]
    (define shape : Maybe-Geo-Tip (dia-edge-style-source-tip s))
    (if (void? shape) (dia-edge-base-style-source-tip ((default-dia-edge-base-style))) shape)))

(define dia-edge-select-target-tip : (-> Dia-Edge-Style Option-Geo-Tip)
  (lambda [s]
    (define shape : Maybe-Geo-Tip (dia-edge-style-target-tip s))
    (if (void? shape) (dia-edge-base-style-target-tip ((default-dia-edge-base-style))) shape)))

(define dia-edge-select-font : (-> Dia-Edge-Style (Option Font))
  (lambda [s]
    (or (dia-edge-style-font s)
        (dia-edge-base-style-font ((default-dia-edge-base-style))))))

(define dia-edge-select-font-paint : (-> Dia-Edge-Style Option-Fill-Paint)
  (lambda [s]
    (or (dia-edge-style-font-paint s)
        (dia-edge-base-style-font-paint ((default-dia-edge-base-style))))))

(define dia-edge-select-label-rotate? : (-> Dia-Edge-Style Boolean)
  (lambda [s]
    (define b : (U Boolean Void) (dia-edge-style-label-rotate? s))
    
    (if (void? b) (dia-edge-base-style-label-rotate? ((default-dia-edge-base-style))) b)))

(define dia-edge-select-label-inline? : (-> Dia-Edge-Style Boolean)
  (lambda [s]
    (define b : (U Boolean Void) (dia-edge-style-label-inline? s))
    
    (if (void? b) (dia-edge-base-style-label-inline? ((default-dia-edge-base-style))) b)))

(define dia-edge-select-label-distance : (-> Dia-Edge-Style (Option Flonum))
  (lambda [s]
    (define fl : (U Flonum Void) (dia-edge-style-label-distance s))
    
    (if (void? fl) (dia-edge-base-style-label-distance ((default-dia-edge-base-style))) fl)))

(define dia-edge-swap-dash-style : (-> Dia-Edge-Style Stroke-Dash-Datum Dia-Edge-Style)
  (lambda [s dash-datum]
    (struct-copy dia-edge-style s
                 [dash dash-datum])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Src Tgt S) dia-edge-style-construct : (-> Src Tgt (Listof Geo-Path-Labels) (Option (Dia-Edge-Style-Make* Src Tgt S)) (-> S) S)
  (lambda [source target label mk-style mk-fallback-style]
    (define maybe-style (and mk-style (mk-style source target label)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
