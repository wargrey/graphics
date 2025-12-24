#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/path/label)
(require geofun/digitama/path/tip/self)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/geometry/anchor)

(require geofun/font)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Free-Track-Endpoint (U Geo-Anchor-Name Float-Complex))

(define-type (Dia-Track-Style-Make* Src Tgt S) (-> Src Tgt (Listof Geo-Path-Labels) (U S Void False)))
(define-type (Dia-Track-Style-Make S) (Dia-Track-Style-Make* Geo (Option Geo) S))
(define-type (Dia-Free-Track-Style-Make S) (Dia-Track-Style-Make* Dia-Free-Track-Endpoint Dia-Free-Track-Endpoint S))

(struct dia-track-style
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
  #:type-name Dia-Track-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-track-base-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [line-paint : Maybe-Stroke-Paint]
   [source-tip : Option-Geo-Tip]
   [target-tip : Option-Geo-Tip]
   [label-rotate? : Boolean]
   [label-inline? : Boolean]
   [label-distance : (Option Flonum)])
  #:type-name Dia-Track-Base-Style
  #:transparent)

(define make-null-track-style : (-> Dia-Track-Base-Style)
  (lambda []
    (dia-track-base-style #false #false (void)
                         #false #false #false #false
                         #false)))

(define default-dia-track-base-style : (Parameterof (-> Dia-Track-Base-Style))
  (make-parameter make-null-track-style))

(define dia-track-id-merge : (-> (Option Dia-Free-Track-Endpoint) (Option Dia-Free-Track-Endpoint) Boolean (Option Symbol))
  (lambda [source-id target-id directed?]
    (define src-id : (Option String) (and source-id (if (complex? source-id) #false (geo-anchor->string source-id))))
    (define tgt-id : (Option String) (and target-id (if (complex? target-id) #false (geo-anchor->string target-id))))

    (and src-id
         (string->symbol
          (cond [(not tgt-id) (string-append src-id "-.")]
                [(not directed?) (string-append src-id "->" tgt-id)]
                [else (string-append src-id "->" tgt-id)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-select-line-paint : (-> (U Dia-Track-Style Maybe-Stroke-Paint) Option-Stroke-Paint)
  (lambda [s]
    (define fallback-paint : (Option Pen) (stroke-paint->source* (dia-track-base-style-line-paint ((default-dia-track-base-style)))))
    
    (cond [(void? s) fallback-paint]
          [(or (not s) (pen? s)) s]
          [(dia-track-style? s)
           (let*-values ([(c) (dia-track-style-color s)]
                         [(d+o) (dia-track-style-dash s)]
                         [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
             (desc-stroke #:color (and (not (void? c)) c)
                          #:width (dia-track-style-width s)
                          #:dash dash #:offset offset
                          (if (pen? fallback-paint) fallback-paint (default-stroke))))]
          [(pen? fallback-paint) (desc-stroke fallback-paint #:color s)]
          [else s])))

(define dia-track-select-source-tip : (-> Dia-Track-Style Option-Geo-Tip)
  (lambda [s]
    (define shape : Maybe-Geo-Tip (dia-track-style-source-tip s))
    (if (void? shape) (dia-track-base-style-source-tip ((default-dia-track-base-style))) shape)))

(define dia-track-select-target-tip : (-> Dia-Track-Style Option-Geo-Tip)
  (lambda [s]
    (define shape : Maybe-Geo-Tip (dia-track-style-target-tip s))
    (if (void? shape) (dia-track-base-style-target-tip ((default-dia-track-base-style))) shape)))

(define dia-track-select-font : (-> Dia-Track-Style (Option Font))
  (lambda [s]
    (or (dia-track-style-font s)
        (dia-track-base-style-font ((default-dia-track-base-style))))))

(define dia-track-select-font-paint : (-> Dia-Track-Style Option-Fill-Paint)
  (lambda [s]
    (or (dia-track-style-font-paint s)
        (dia-track-base-style-font-paint ((default-dia-track-base-style))))))

(define dia-track-select-label-rotate? : (-> Dia-Track-Style Boolean)
  (lambda [s]
    (define b : (U Boolean Void) (dia-track-style-label-rotate? s))
    
    (if (void? b) (dia-track-base-style-label-rotate? ((default-dia-track-base-style))) b)))

(define dia-track-select-label-inline? : (-> Dia-Track-Style Boolean)
  (lambda [s]
    (define b : (U Boolean Void) (dia-track-style-label-inline? s))
    
    (if (void? b) (dia-track-base-style-label-inline? ((default-dia-track-base-style))) b)))

(define dia-track-select-label-distance : (-> Dia-Track-Style (Option Flonum))
  (lambda [s]
    (define fl : (U Flonum Void) (dia-track-style-label-distance s))
    
    (if (void? fl) (dia-track-base-style-label-distance ((default-dia-track-base-style))) fl)))

(define dia-track-swap-dash-style : (-> Dia-Track-Style Stroke-Dash-Datum Dia-Track-Style)
  (lambda [s dash-datum]
    (struct-copy dia-track-style s
                 [dash dash-datum])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Src Tgt S) dia-track-style-construct : (-> Src Tgt (Listof Geo-Path-Labels) (Option (Dia-Track-Style-Make* Src Tgt S)) (-> S) S)
  (lambda [source target label mk-style mk-fallback-style]
    (define maybe-style (and mk-style (mk-style source target label)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
