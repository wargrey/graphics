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
(require geofun/digitama/geometry/insets)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (dia-block-theme-case stx)
  (syntax-parse stx #:literals [:]
    [(_ style [(pred? ...) adjust ...] ...)
     (syntax/loc stx
       (let ([self style])
         (cond [(or (dia-block-style?? self pred?) ...) adjust ...]
               ...)))]))

(define-syntax (dia-block-case stx)
  (syntax-parse stx #:literals [:]
    [(_ style [(pred? ...) build ...] ...)
     (syntax/loc stx
       (let ([self (dia-block-style-spec-custom style)])
         (cond [(or (dia-block-style?? self pred?) ...) build ...]
               ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Block-Theme-Adjuster Style Metadata)
  (-> (Dia-Block-Style Style) Geo-Anchor-Name Metadata
      (U (Dia-Block-Style Style) False Void)))

(define-type (Dia-Block-Info Style Metadata) (List String Metadata (Option (Dia-Block-Style Style))))

(define-type Dia-Block-Option-Size (Option Length+%))
(define-type Dia-Block-Padding Geo-Insets-Datum+%)
(define-type Dia-Block-Option-Padding (Option Dia-Block-Padding))
(define-type Dia-Block-Font-Style (U Font Font:Tweak))

(define-struct dia-block-style : Dia-Block-Style
  #:forall ([phantom-type : T])
  ([width : Dia-Block-Option-Size]
   [height : Dia-Block-Option-Size]
   [padding : Dia-Block-Option-Padding]
   [font : (Option Dia-Block-Font-Style)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Length+%)]
   [stroke-color : Maybe-Color]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint]
   [text-alignment : (Option Geo-Text-Alignment)]
   [text-trim? : (U Void Boolean)])
  #:transparent)

(struct dia-block-backstop-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [padding : Dia-Block-Padding]
   [font : Font]
   [font-paint : Fill-Paint]
   [stroke-paint : Option-Stroke-Paint]
   [fill-paint : Option-Fill-Paint]
   [text-alignment : Geo-Text-Alignment]
   [text-trim? : Boolean])
  #:type-name Dia-Block-Backstop-Style
  #:transparent)

(define-struct #:forall (S) dia-block-style-spec : Dia-Block-Style-Spec
  ([custom : (Dia-Block-Style S)]
   [backstop : Dia-Block-Backstop-Style]
   [scale : Nonnegative-Flonum 1.0]
   [opacity : (Option Nonnegative-Flonum) #false])
  #:transparent)

(define #:forall (S) dia-block-style-type-object : (-> (Dia-Block-Style-Spec S) S)
  (lambda [self]
    (dia-block-style-phantom-type (dia-block-style-spec-custom self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-text->caption : (->* (Geo-Rich-Text (Dia-Block-Style-Spec S))
                                                    (#:id Geo-Anchor-Name #:color Option-Fill-Paint #:font (Option Font))
                                                    (Option Geo))
  (lambda [desc style #:id [id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define-values (alignment trim?) (dia-block-resolve-text-preference style))
    (define font : Font (or alt-font (dia-block-resolve-font style)))
    (define paint : Option-Fill-Paint (or alt-color (dia-block-resolve-font-paint style)))
    
    (define text : Geo-Rich-Text
      (cond [(not trim?) desc]
            [(string? desc) (string-trim desc)]
            [(bytes? desc) (regexp-replace* #px"((^\\s*)|(\\s*$))" desc #"")]
            [else desc]))
    
    (and (cond [(string? text) (> (string-length text) 0)]
               [(bytes? text)  (> (bytes-length text) 0)]
               [else #true])
         (geo-rich-text-realize #:id (dia-block-caption-id (or id (gensym 'dia:block:caption:)))
                                #:alignment alignment
                                text font paint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-resolve-font : (-> (Dia-Block-Style-Spec S) Font)
  (lambda [self]
    (define s (dia-block-style-spec-custom self))
    (define b (dia-block-style-spec-backstop self))
    (define f (dia-block-style-font s))

    (cond [(font? f) f]
          [(or f) (desc-font* (dia-block-backstop-style-font b) #:tweak f)]
          [else (dia-block-backstop-style-font b)])))

(define #:forall (S) dia-block-resolve-text-preference : (-> (Dia-Block-Style-Spec S) (Values Geo-Text-Alignment Boolean))
  (lambda [self]
    (define s (dia-block-style-spec-custom self))
    (define b (dia-block-style-spec-backstop self))
    (define maybe-trim? (dia-block-style-text-trim? s))

    (values (or (dia-block-style-text-alignment s)
                (dia-block-backstop-style-text-alignment b))
            (if (void? maybe-trim?)
                (dia-block-backstop-style-text-trim? b)
                maybe-trim?))))

(define #:forall (S) dia-block-resolve-size : (->* ((Dia-Block-Style-Spec S))
                                                   (#:width Dia-Block-Option-Size #:height Dia-Block-Option-Size)
                                                   (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [style #:width [alt-width #false] #:height [alt-height #false]]
    (define maybe-width  (or alt-width  (dia-block-style-width  (dia-block-style-spec-custom style))))
    (define maybe-height (or alt-height (dia-block-style-height (dia-block-style-spec-custom style))))
    (define Width  (dia-block-backstop-style-width  (dia-block-style-spec-backstop style)))
    (define Height (dia-block-backstop-style-height (dia-block-style-spec-backstop style)))
    (define s (dia-block-style-spec-scale style))
    
    (values (* (if (or  maybe-width) (~dimension  maybe-width  Width)  Width) s)
            (* (if (or maybe-height) (~dimension maybe-height Height) Height) s))))

(define #:forall (S) dia-block-resolve-width : (-> (Dia-Block-Style-Spec S) Nonnegative-Flonum)
  (lambda [style]
    (define maybe-width  (dia-block-style-width  (dia-block-style-spec-custom style)))
    (define Width  (dia-block-backstop-style-width  (dia-block-style-spec-backstop style)))
    
    (* (if (or maybe-width) (~dimension maybe-width Width) Width)
       (dia-block-style-spec-scale style))))

(define #:forall (S) dia-block-resolve-padding : (->* ((Dia-Block-Style-Spec S))
                                                      (#:padding Dia-Block-Option-Padding)
                                                      Geo-Standard-Insets)
  (lambda [style #:padding [alt-padding #false]]
    (geo-insets-scale (geo-insets*->insets (or alt-padding
                                               (dia-block-style-padding (dia-block-style-spec-custom style))
                                               (dia-block-backstop-style-padding (dia-block-style-spec-backstop style)))
                                           (dia-block-resolve-width style))
                      (dia-block-style-spec-scale style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-resolve-stroke-width : (-> (Dia-Block-Style-Spec S) Nonnegative-Flonum)
  (lambda [self]
    (define backstop (dia-block-backstop-style-stroke-paint (dia-block-style-spec-backstop self)))
    (define width (dia-block-style-stroke-width (dia-block-style-spec-custom self)))
    
    (let ([paint (stroke-paint->source backstop)])
      (cond [(not width) (pen-width paint)]
            [else (~dimension width (pen-width paint))]))))

(define #:forall (S) dia-block-resolve-stroke-paint : (-> (Dia-Block-Style-Spec S) (Option Pen))
  (lambda [self]
    (let ([c (dia-block-style-stroke-color (dia-block-style-spec-custom self))])
      (and c (let*-values ([(d+o) (dia-block-style-stroke-dash (dia-block-style-spec-custom self))]
                           [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))]
                           [(opacity) (dia-block-style-spec-opacity self)])
               (desc-stroke* #:dash dash #:offset offset #:color (and (not (void? c)) c)
                             #:opacity (and opacity (< opacity 1.0) opacity)
                             ; yes, don't scale stroke width by default
                             #:width (dia-block-style-stroke-width (dia-block-style-spec-custom self))
                             (stroke-paint->source (dia-block-backstop-style-stroke-paint (dia-block-style-spec-backstop self)))))))))

(define #:forall (S) dia-block-resolve-fill-paint : (-> (Dia-Block-Style-Spec S) (Option Brush))
  (lambda [self]
    (let ([paint (dia-block-style-fill-paint (dia-block-style-spec-custom self))]
          [opacity (dia-block-style-spec-opacity self)])                
      (try-desc-brush* #:opacity (and opacity (< opacity 1.0) opacity)
                       (fill-paint->source* (cond [(not (void? paint)) paint]
                                                  [else (dia-block-backstop-style-fill-paint (dia-block-style-spec-backstop self))]))))))

(define #:forall (S) dia-block-resolve-font-paint : (-> (Dia-Block-Style-Spec S) Brush)
  (lambda [self]
    (define opacity (dia-block-style-spec-opacity self))
    
    (desc-brush* #:opacity (and opacity (< opacity 1.0) opacity)
                 (fill-paint->source (or (dia-block-style-font-paint (dia-block-style-spec-custom self))
                                         (dia-block-backstop-style-font-paint (dia-block-style-spec-backstop self)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-caption-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-block-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) dia-block-theme-adjust : (-> (Dia-Block-Style S) Geo-Anchor-Name (Option (Dia-Block-Theme-Adjuster S M)) M
                                                    (Option (Dia-Block-Style S)))
  (lambda [the-style anchor maybe-adjuster property]
    (if (and maybe-adjuster)
        (let ([maybe-adjusted-style (maybe-adjuster the-style anchor property)])
          (cond [(void? maybe-adjusted-style) the-style]
                [else maybe-adjusted-style]))
        the-style)))

(define #:forall (S M) dia-block-theme-adjust* : (-> (Dia-Block-Style S) Geo-Anchor-Name (Option (Dia-Block-Theme-Adjuster S M)) M (-> S)
                                                     (Option (Dia-Block-Style S)))
  (lambda [the-style anchor maybe-adjuster property mk-type-value]
    (define replaced-style (remake-dia-block-style* the-style #:phantom-type (mk-type-value)))
    
    (if (and maybe-adjuster)
        (let ([maybe-adjusted-style (maybe-adjuster replaced-style anchor property)])
          (cond [(void? maybe-adjusted-style) replaced-style]
                [else maybe-adjusted-style]))
        replaced-style)))

(define #:forall (S M) dia-block-info : (-> Geo-Anchor-Name String (-> (Dia-Block-Style S)) (-> (Option (Dia-Block-Theme-Adjuster S M))) M
                                            (Dia-Block-Info S M))
  (lambda [anchor text mk-style maybe-adjuster datum]
    (list text datum
          ((inst dia-block-theme-adjust S M)
           (mk-style) anchor (maybe-adjuster) datum))))

(define #:forall (S M) dia-block-info* : (-> Geo-Anchor-Name String (-> (Dia-Block-Style S)) (-> (Option (Dia-Block-Theme-Adjuster S M))) (-> S) M
                                             (Dia-Block-Info S M))
  (lambda [anchor text mk-style maybe-adjuster mk-type-value datum]
    (list text datum
          ((inst dia-block-theme-adjust* S M)
           (mk-style) anchor (maybe-adjuster) datum mk-type-value))))
