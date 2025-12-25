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

(define-type (Dia-Track-Style-Layers* S) (Pairof S Dia-Track-Backstop-Style))
(define-type Dia-Track-Style-Layers (Dia-Track-Style-Layers* Dia-Track-Style))

(struct dia-track-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [width : (Option Flonum)]
   [color : (U Color Void False)]
   [dash : (Option Stroke-Dash+Offset)]
   [opacity : (Option Flonum)]
   [source-tip : Maybe-Geo-Tip]
   [target-tip : Maybe-Geo-Tip]
   [label-rotate? : (U Boolean Void)]
   [label-inline? : (U Boolean Void)]
   [label-distance : (U Flonum Void)])
  #:type-name Dia-Track-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-track-backstop-style
  ([font : Font]
   [font-paint : Fill-Paint]
   [line-paint : Stroke-Paint]
   [source-tip : Option-Geo-Tip]
   [target-tip : Option-Geo-Tip]
   [label-rotate? : Boolean]
   [label-inline? : Boolean]
   [label-distance : (Option Flonum)])
  #:type-name Dia-Track-Backstop-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-resolve-line-paint : (-> Dia-Track-Style-Layers Stroke-Paint)
  (lambda [self]
    (define style (car self))
    (define c (dia-track-style-color style))
    (define d+o (dia-track-style-dash style))
    (define-values (dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false)))

    (desc-stroke #:color (and (not (void? c)) c)
                 #:width (dia-track-style-width style)
                 #:dash dash #:offset offset
                 #:opacity (dia-track-style-opacity style)
                 (stroke-paint->source (dia-track-backstop-style-line-paint (cdr self))))))

(define dia-track-resolve-source-tip : (-> Dia-Track-Style-Layers Option-Geo-Tip)
  (lambda [self]
    (define shape : Maybe-Geo-Tip (dia-track-style-source-tip (car self)))
    (if (void? shape) (dia-track-backstop-style-source-tip (cdr self)) shape)))

(define dia-track-resolve-target-tip : (-> Dia-Track-Style-Layers Option-Geo-Tip)
  (lambda [self]
    (define shape : Maybe-Geo-Tip (dia-track-style-target-tip (car self)))
    (if (void? shape) (dia-track-backstop-style-target-tip (cdr self)) shape)))

(define dia-track-resolve-font : (-> Dia-Track-Style-Layers (Option Font))
  (lambda [self]
    (or (dia-track-style-font (car self))
        (dia-track-backstop-style-font (cdr self)))))

(define dia-track-resolve-font-paint : (-> Dia-Track-Style-Layers Fill-Paint)
  (lambda [self]
    (or (dia-track-style-font-paint (car self))
        (dia-track-backstop-style-font-paint (cdr self)))))

(define dia-track-resolve-label-rotate? : (-> Dia-Track-Style-Layers Boolean)
  (lambda [self]
    (define b : (U Boolean Void) (dia-track-style-label-rotate? (car self)))
    (if (void? b) (dia-track-backstop-style-label-rotate? (cdr self)) b)))

(define dia-track-resolve-label-inline? : (-> Dia-Track-Style-Layers Boolean)
  (lambda [self]
    (define b : (U Boolean Void) (dia-track-style-label-inline? (car self)))
    (if (void? b) (dia-track-backstop-style-label-inline? (cdr self)) b)))

(define dia-track-resolve-label-distance : (-> Dia-Track-Style-Layers (Option Flonum))
  (lambda [self]
    (define fl : (U Flonum Void) (dia-track-style-label-distance (car self)))
    (if (void? fl) (dia-track-backstop-style-label-distance (cdr self)) fl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-id-merge : (-> (Option Dia-Free-Track-Endpoint) (Option Dia-Free-Track-Endpoint) Boolean (Option Symbol))
  (lambda [source-id target-id directed?]
    (define src-id : (Option String) (and source-id (if (complex? source-id) #false (geo-anchor->string source-id))))
    (define tgt-id : (Option String) (and target-id (if (complex? target-id) #false (geo-anchor->string target-id))))

    (and src-id
         (string->symbol
          (cond [(not tgt-id) (string-append src-id "-.")]
                [(not directed?) (string-append src-id "->" tgt-id)]
                [else (string-append src-id "->" tgt-id)])))))

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
