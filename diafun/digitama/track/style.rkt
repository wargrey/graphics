#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/stroke)
(require geofun/fill)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/track/anchor)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/path/label)
(require geofun/digitama/path/tip/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Free-Track-Endpoint (U Geo-Anchor-Name Float-Complex))

(define-type (Dia-Track-Theme-Adjuster* S Src Tgt) (-> (Dia-Track-Style S) Src Tgt (Listof Geo-Path-Label-Datum) (U (Dia-Track-Style S) False Void)))
(define-type (Dia-Track-Theme-Adjuster S) (Dia-Track-Theme-Adjuster* S Geo (Option Geo)))

(define-type Dia-Track-Font-Style (U Font Font:Tweak))

(define-struct dia-track-style : Dia-Track-Style
  #:forall ([phantom-type : T])
  ([font : (Option Dia-Track-Font-Style)]
   [font-paint : Option-Fill-Paint]
   [width : (Option Length+%)]
   [color : (U Color Void False)]
   [dash : (Option Stroke-Dash+Offset)]
   [source-tip : Maybe-Geo-Tip]
   [target-tip : Maybe-Geo-Tip]
   [label-rotate? : (U Boolean Void)]
   [label-inline? : (U Boolean Void)]
   [label-distance : (U Flonum Void)])
  #:transparent)

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

(define-struct #:forall (S) dia-track-style-spec : Dia-Track-Style-Spec
  ([custom : (Dia-Track-Style S)]
   [backstop : Dia-Track-Backstop-Style]
   [scale : (Option Nonnegative-Flonum) #false]
   [opacity : (Option Nonnegative-Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-resolve-line-paint : (-> (Dia-Track-Style-Spec S) Pen)
  (lambda [self]
    (define c (dia-track-style-color (dia-track-style-spec-custom self)))
    (define d+o (dia-track-style-dash (dia-track-style-spec-custom self)))
    (define-values (dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false)))
    
    (define width (dia-track-style-width (dia-track-style-spec-custom self)))
    (define backstop (dia-track-backstop-style-line-paint (dia-track-style-spec-backstop self)))
    (define scale (dia-track-style-spec-scale self))
    (define opacity (dia-track-style-spec-opacity self))

    (desc-stroke* #:width width #:dash dash #:offset offset
                  #:color (and (not (void? c)) c)
                  #:opacity (and opacity (< opacity 1.0) opacity)
                  #:scale (and (rational? scale) (not (= scale 1.0)) scale)
                  (stroke-paint->source backstop))))

(define #:forall (S) dia-track-resolve-source-tip : (-> (Dia-Track-Style-Spec S) Option-Geo-Tip)
  (lambda [self]
    (define shape : Maybe-Geo-Tip (dia-track-style-source-tip (dia-track-style-spec-custom self)))
    (if (void? shape) (dia-track-backstop-style-source-tip (dia-track-style-spec-backstop self)) shape)))

(define #:forall (S) dia-track-resolve-target-tip : (-> (Dia-Track-Style-Spec S) Option-Geo-Tip)
  (lambda [self]
    (define shape : Maybe-Geo-Tip (dia-track-style-target-tip (dia-track-style-spec-custom self)))
    (if (void? shape) (dia-track-backstop-style-target-tip (dia-track-style-spec-backstop self)) shape)))

(define #:forall (S) dia-track-resolve-font : (-> (Dia-Track-Style-Spec S) Font)
  (lambda [self]
    (define s (dia-track-style-spec-custom self))
    (define f (dia-track-style-font s))

    (cond [(font? f) f]
          [(not f) (dia-track-backstop-style-font (dia-track-style-spec-backstop self))]
          [else (desc-font* #:tweak f
                            (dia-track-backstop-style-font (dia-track-style-spec-backstop self)))])))

(define #:forall (S) dia-track-resolve-font-paint : (-> (Dia-Track-Style-Spec S) Brush)
  (lambda [self]
    (desc-brush #:opacity (dia-track-style-spec-opacity self)
                (fill-paint->source (or (dia-track-style-font-paint (dia-track-style-spec-custom self))
                                        (dia-track-backstop-style-font-paint (dia-track-style-spec-backstop self)))))))

(define #:forall (S) dia-track-resolve-label-rotate? : (-> (Dia-Track-Style-Spec S) Boolean)
  (lambda [self]
    (define b : (U Boolean Void) (dia-track-style-label-rotate? (dia-track-style-spec-custom self)))
    (if (void? b) (dia-track-backstop-style-label-rotate? (dia-track-style-spec-backstop self)) b)))

(define #:forall (S) dia-track-resolve-label-inline? : (-> (Dia-Track-Style-Spec S) Boolean)
  (lambda [self]
    (define b : (U Boolean Void) (dia-track-style-label-inline? (dia-track-style-spec-custom self)))
    (if (void? b) (dia-track-backstop-style-label-inline? (dia-track-style-spec-backstop self)) b)))

(define #:forall (S) dia-track-resolve-label-distance : (-> (Dia-Track-Style-Spec S) (Option Flonum))
  (lambda [self]
    (define fl : (U Flonum Void) (dia-track-style-label-distance (dia-track-style-spec-custom self)))
    (if (void? fl) (dia-track-backstop-style-label-distance (dia-track-style-spec-backstop self)) fl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-id-merge : (-> (Option Dia-Free-Track-Endpoint) (Option Dia-Free-Track-Endpoint) Boolean (Option Symbol))
  (lambda [source-id target-id directed?]
    (define src-id : (Option String) (and source-id (if (complex? source-id) #false (geo-anchor->string source-id))))
    (define tgt-id : (Option String) (and target-id (if (complex? target-id) #false (geo-anchor->string target-id))))

    (and src-id
         (string->symbol
          (cond [(not tgt-id) (string-append src-id "-.")]
                [(not directed?) (string-append src-id "->" tgt-id)]
                [else (string-append src-id "->" tgt-id)])))))

(define #:forall (S) dia-track-swap-dash-style : (-> (Dia-Track-Style S) Stroke-Dash-Datum (Dia-Track-Style S))
  (lambda [s dash-datum]
    (struct-copy dia-track-style s
                 [dash dash-datum])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S Src Tgt) dia-track-theme-adjust : (-> Src Tgt (Listof Geo-Path-Label-Datum)
                                                          (-> (Dia-Track-Style S)) (-> (Option (Dia-Track-Theme-Adjuster* S Src Tgt)))
                                                          (Option (Dia-Track-Style S)))
  (lambda [source target label mk-style mk-maybe-adjust]
    (define-values (the-style maybe-adjust) (values (mk-style) (mk-maybe-adjust)))
    
    (if (and maybe-adjust)
        (let ([maybe-adjusted-style (maybe-adjust the-style source target label)])
          (cond [(void? maybe-adjusted-style) the-style]
                [else maybe-adjusted-style]))
        the-style)))
