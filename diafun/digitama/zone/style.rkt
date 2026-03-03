#lang typed/racket/base

(provide (all-defined-out) Geo-Standard-Insets)

(require digimon/measure)
(require digimon/struct)
(require racket/string)

(require geofun/font)
(require geofun/stroke)
(require geofun/fill)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/richtext/self)
(require geofun/digitama/richtext/realize)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/track/anchor)
(require geofun/digitama/geometry/sides)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Zone-Theme-Adjuster Style Metadata)
  (-> (Dia-Zone-Style Style) Geo-Anchor-Name Metadata
      (U (Dia-Zone-Style Style) False Void)))

(define-type (Dia-Zone-Info Style Metadata) (List (Option String) Metadata (Option (Dia-Zone-Style Style))))

(define-type Dia-Zone-Option-Size (Option Length+%))
(define-type Dia-Zone-Padding Geo-Insets-Datum+%)
(define-type Dia-Zone-Option-Padding (Option Dia-Zone-Padding))

(define-struct dia-zone-style : Dia-Zone-Style
  #:forall ([phantom-type : T])
  ([padding : Dia-Zone-Option-Padding]
   [font : (Option Font+Tweak)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Length+%)]
   [stroke-color : Maybe-Color]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint])
  #:transparent)

(struct dia-zone-backstop-style
  ([padding : Dia-Zone-Padding]
   [font : Font]
   [font-paint : Fill-Paint]
   [stroke-paint : Option-Stroke-Paint]
   [fill-paint : Option-Fill-Paint])
  #:type-name Dia-Zone-Backstop-Style
  #:transparent)

(define-struct #:forall (S) dia-zone-style-spec : Dia-Zone-Style-Spec
  ([custom : (Dia-Zone-Style S)]
   [backstop : Dia-Zone-Backstop-Style]
   [scale : (Option Nonnegative-Flonum) #false]
   [opacity : (Option Nonnegative-Flonum) #false])
  #:transparent)

(define #:forall (S) dia-zone-style-type-object : (-> (Dia-Zone-Style-Spec S) S)
  (lambda [self]
    (dia-zone-style-phantom-type (dia-zone-style-spec-custom self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-zone-text->title
  (lambda [#:id [id : (Option Geo-Anchor-Name) #false]
           #:color [alt-color : Option-Fill-Paint #false]
           #:font [alt-font : Option-Font #false]
           #:alignment [alignment : Geo-Text-Alignment 'center]
           #:max-width [max-width : (Option Nonnegative-Flonum) #false]
           #:max-height [max-height : (Option Nonnegative-Flonum) #false]
           #:trim? [trim? : Boolean #true]
           [desc : Geo-Rich-Text]
           [style : (Dia-Zone-Style-Spec S)]] : (Option Geo)
    (define font : Font (or alt-font (dia-zone-resolve-font style)))
    (define paint : Option-Fill-Paint (or alt-color (dia-zone-resolve-font-paint style)))
    
    (define text : Geo-Rich-Text
      (cond [(not trim?) desc]
            [(string? desc) (string-trim desc)]
            [(bytes? desc) (regexp-replace* #px"((^\\s*)|(\\s*$))" desc #"")]
            [else desc]))
    
    (and (cond [(string? text) (> (string-length text) 0)]
               [(bytes? text)  (> (bytes-length text) 0)]
               [else #true])
         (geo-rich-text-realize #:id (dia-zone-title-id (or id (gensym 'dia:zone:title:)))
                                #:max-width max-width #:max-height max-height
                                #:alignment alignment
                                text font paint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-zone-resolve-font : (-> (Dia-Zone-Style-Spec S) Font)
  (lambda [self]
    (define bsf : Font (let ([b (dia-zone-style-spec-backstop self)]) (dia-zone-backstop-style-font b)))
    (define myf : (Option Font+Tweak) (dia-zone-style-font (dia-zone-style-spec-custom self)))

    (cond [(not myf) bsf]
          [(font? myf) myf]
          [else (desc-font* bsf #:tweak myf)])))

(define #:forall (S) dia-zone-resolve-padding : (->* ((Dia-Zone-Style-Spec S) Nonnegative-Flonum  Nonnegative-Flonum)
                                                     (#:padding Dia-Zone-Option-Padding)
                                                     Geo-Standard-Insets)
  (lambda [style width height #:padding [alt-padding #false]]
    (geo-insets-scale (geo-insets*->insets (or alt-padding
                                               (dia-zone-style-padding (dia-zone-style-spec-custom style))
                                               (dia-zone-backstop-style-padding (dia-zone-style-spec-backstop style)))
                                           width)
                      (or (dia-zone-style-spec-scale style) 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-zone-resolve-stroke-width : (-> (Dia-Zone-Style-Spec S) Nonnegative-Flonum)
  (lambda [self]
    (define width (dia-zone-style-stroke-width (dia-zone-style-spec-custom self)))
    
    (let ([paint (stroke-paint->source (dia-zone-backstop-style-stroke-paint (dia-zone-style-spec-backstop self)))])
      (cond [(not width) (pen-width paint)]
            [else (~dimension width (pen-width paint))]))))

(define #:forall (S) dia-zone-resolve-stroke-paint : (-> (Dia-Zone-Style-Spec S) (Option Pen))
  (lambda [self]
    (define me (dia-zone-style-spec-custom self))
    (define c : Maybe-Color (dia-zone-style-stroke-color me))

    (and c (let*-values ([(d+o) (dia-zone-style-stroke-dash me)]
                         [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
             (desc-stroke* ; yes, don't scale stroke width by default
              #:width (dia-zone-style-stroke-width me)
              #:dash dash #:offset offset #:color (if (void? c) #false c)
              #:opacity (dia-zone-style-spec-opacity self)
              (stroke-paint->source
               (dia-zone-backstop-style-stroke-paint
                (dia-zone-style-spec-backstop self))))))))

(define #:forall (S) dia-zone-resolve-fill-paint : (-> (Dia-Zone-Style-Spec S) (Option Brush))
  (lambda [self]
    (define paint : Maybe-Fill-Paint (dia-zone-style-fill-paint (dia-zone-style-spec-custom self)))
    
    (try-desc-brush* #:opacity (dia-zone-style-spec-opacity self)
                     (fill-paint->source* (cond [(void? paint) (dia-zone-backstop-style-fill-paint (dia-zone-style-spec-backstop self))]
                                                [else paint])))))

(define #:forall (S) dia-zone-resolve-font-paint : (-> (Dia-Zone-Style-Spec S) Brush)
  (lambda [self]
    (desc-brush* #:opacity (dia-zone-style-spec-opacity self)
                 (fill-paint->source (or (dia-zone-style-font-paint (dia-zone-style-spec-custom self))
                                         (dia-zone-backstop-style-font-paint (dia-zone-style-spec-backstop self)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-zone-title-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-zone-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) dia-zone-theme-adjust : (-> (Dia-Zone-Style S) Geo-Anchor-Name (Option (Dia-Zone-Theme-Adjuster S M)) M
                                                   (Option (Dia-Zone-Style S)))
  (lambda [the-style anchor maybe-adjuster property]
    (if (and maybe-adjuster)
        (let ([maybe-adjusted-style (maybe-adjuster the-style anchor property)])
          (cond [(void? maybe-adjusted-style) the-style]
                [else maybe-adjusted-style]))
        the-style)))

(define #:forall (S M) dia-zone-theme-adjust* : (-> (Dia-Zone-Style S) Geo-Anchor-Name (Option (Dia-Zone-Theme-Adjuster S M)) M (-> S)
                                                    (Option (Dia-Zone-Style S)))
  (lambda [the-style anchor maybe-adjuster property mk-type-value]
    (define replaced-style (remake-dia-zone-style* the-style #:phantom-type (mk-type-value)))
    
    (if (and maybe-adjuster)
        (let ([maybe-adjusted-style (maybe-adjuster replaced-style anchor property)])
          (cond [(void? maybe-adjusted-style) replaced-style]
                [else maybe-adjusted-style]))
        replaced-style)))

(define #:forall (S M) dia-zone-info : (-> Geo-Anchor-Name (Option String) (-> (Dia-Zone-Style S)) (-> (Option (Dia-Zone-Theme-Adjuster S M))) M
                                           (Dia-Zone-Info S M))
  (lambda [anchor text mk-style maybe-adjuster datum]
    (list text datum
          ((inst dia-zone-theme-adjust S M)
           (mk-style) anchor (maybe-adjuster) datum))))

(define #:forall (S M) dia-zone-info* : (-> Geo-Anchor-Name (Option String) (-> (Dia-Zone-Style S)) (-> (Option (Dia-Zone-Theme-Adjuster S M))) (-> S) M
                                            (Dia-Zone-Info S M))
  (lambda [anchor text mk-style maybe-adjuster mk-type-value datum]
    (list text datum
          ((inst dia-zone-theme-adjust* S M)
           (mk-style) anchor (maybe-adjuster) datum mk-type-value))))
