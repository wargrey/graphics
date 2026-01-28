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
(define-type Dia-Track-Endpoint (U Geo-Anchor-Name Float-Complex))

(define-type (Dia-Track-Theme-Adjuster* S Src Tgt) (-> (Dia-Track-Style S) Src Tgt (Listof Geo-Path-Label-Datum) (U (Dia-Track-Style S) False Void)))
(define-type (Dia-Track-Theme-Adjuster S) (Dia-Track-Theme-Adjuster* S Geo (Option Geo)))

(define-struct dia-track-style : Dia-Track-Style
  #:forall ([phantom-type : T])
  ([font : (Option Font+Tweak)]
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
   [root : (Option (Dia-Track-Style S)) #false]
   [scale : (Option Nonnegative-Flonum) #false]
   [opacity : (Option Nonnegative-Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-resolve-line-paint : (-> (Dia-Track-Style-Spec S) Pen)
  (lambda [self]
    (define rt (dia-track-style-spec-root self))
    (define me (dia-track-style-spec-custom self))
    
    (define d+o (or (dia-track-style-dash me) (and rt (dia-track-style-dash rt))))
    (define-values (dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false)))

    (define c : Maybe-Color
      (let ([c (dia-track-style-color me)])
        (cond [(void? c) (if rt (dia-track-style-color rt) (void))]
              [else c])))
    
    (desc-stroke* #:width (or (dia-track-style-width me) (and rt (dia-track-style-width rt)))
                  #:dash dash #:offset offset
                  #:color (if (void? c) #false c)
                  #:opacity (dia-track-style-spec-opacity self)
                  #:scale (dia-track-style-spec-scale self)
                  (stroke-paint->source
                   (dia-track-backstop-style-line-paint
                    (dia-track-style-spec-backstop self))))))

(define #:forall (S) dia-track-resolve-tips : (-> (Dia-Track-Style-Spec S) (Values Option-Geo-Tip Option-Geo-Tip))
  (lambda [self]
    (define rt (dia-track-style-spec-root self))
    (define bs (dia-track-style-spec-backstop self))
    (define me (dia-track-style-spec-custom self))
    
    (define src : Maybe-Geo-Tip
      (let ([tip (dia-track-style-source-tip me)])
        (cond [(void? tip) (if rt (dia-track-style-source-tip rt) (void))]
              [else tip])))
    
    (define tgt : Maybe-Geo-Tip
      (let ([tip (dia-track-style-target-tip me)])
        (cond [(void? tip) (if rt (dia-track-style-target-tip rt) (void))]
              [else tip])))

    (values (if (void? src) (dia-track-backstop-style-source-tip bs) src)
            (if (void? tgt) (dia-track-backstop-style-target-tip bs) tgt))))

(define #:forall (S) dia-track-resolve-font : (-> (Dia-Track-Style-Spec S) Font)
  (lambda [self]
    (define bsf : Font (let ([b (dia-track-style-spec-backstop self)]) (dia-track-backstop-style-font b)))
    (define rtf : (Option Font+Tweak) (let ([rt (dia-track-style-spec-root self)]) (and rt (dia-track-style-font rt))))
    (define myf : (Option Font+Tweak) (dia-track-style-font (dia-track-style-spec-custom self)))

    (cond [(not myf)
           (cond [(not rtf) bsf]
                 [(font? rtf) rtf]
                 [else (desc-font* bsf #:tweak rtf)])]
          [(font? myf) myf]
          [(not rtf) (desc-font* bsf #:tweak myf)]
          [(font? rtf) (desc-font* rtf #:tweak myf)]
          [else (desc-font* #:tweak rtf
                            #:family (font:tweak-family myf)
                            #:size (font:tweak-size myf)
                            #:style (font:tweak-style myf)
                            #:weight (font:tweak-weight myf)
                            bsf)])))

(define #:forall (S) dia-track-resolve-font-paint : (-> (Dia-Track-Style-Spec S) Brush)
  (lambda [self]
    (desc-brush #:opacity (dia-track-style-spec-opacity self)
                (fill-paint->source (or (dia-track-style-font-paint (dia-track-style-spec-custom self))
                                        (let ([rt (dia-track-style-spec-root self)])
                                          (and rt (dia-track-style-font-paint rt)))
                                        (dia-track-backstop-style-font-paint (dia-track-style-spec-backstop self)))))))

(define #:forall (S) dia-track-resolve-label-placement : (-> (Dia-Track-Style-Spec S) (Values Boolean Boolean (Option Flonum)))
  (lambda [self]
    (define rt (dia-track-style-spec-root self))
    (define bs (dia-track-style-spec-backstop self))
    (define me (dia-track-style-spec-custom self))
    
    (define r? : (U Boolean Void)
      (let ([? (dia-track-style-label-rotate? me)])
        (if (void? ?) (if rt (dia-track-style-label-rotate? rt) (void)) ?)))

    (define i? : (U Boolean Void)
      (let ([? (dia-track-style-label-inline? me)])
        (if (void? ?) (if rt (dia-track-style-label-inline? rt) (void)) ?)))

    (define d : (U Flonum Void)
      (let ([d (dia-track-style-label-distance me)])
        (if (void? d) (if rt (dia-track-style-label-distance rt) (void)) d)))
    
    (values (if (void? r?) (dia-track-backstop-style-label-rotate? bs) r?)
            (if (void? i?) (dia-track-backstop-style-label-inline? bs) i?)
            (if (void? d) (dia-track-backstop-style-label-distance bs) d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-id-merge : (-> (Option Dia-Track-Endpoint) (Option Dia-Track-Endpoint) Boolean (Option Symbol))
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
