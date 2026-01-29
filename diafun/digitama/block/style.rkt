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

(define-struct dia-block-style : Dia-Block-Style
  #:forall ([phantom-type : T])
  ([width : Dia-Block-Option-Size]
   [height : Dia-Block-Option-Size]
   [padding : Dia-Block-Option-Padding]
   [font : (Option Font+Tweak)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Length+%)]
   [stroke-color : Maybe-Color]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint])
  #:transparent)

(struct dia-block-backstop-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [padding : Dia-Block-Padding]
   [font : Font]
   [font-paint : Fill-Paint]
   [stroke-paint : Option-Stroke-Paint]
   [fill-paint : Option-Fill-Paint])
  #:type-name Dia-Block-Backstop-Style
  #:transparent)

(define-struct #:forall (S) dia-block-style-spec : Dia-Block-Style-Spec
  ([custom : (Dia-Block-Style S)]
   [backstop : Dia-Block-Backstop-Style]
   [root : (Option (Dia-Block-Style S)) #false]
   [scale : (Option Nonnegative-Flonum) #false]
   [opacity : (Option Nonnegative-Flonum) #false])
  #:transparent)

(define #:forall (S) dia-block-style-type-object : (-> (Dia-Block-Style-Spec S) S)
  (lambda [self]
    (dia-block-style-phantom-type (dia-block-style-spec-custom self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-text->caption
  (lambda [#:id [id : (Option Geo-Anchor-Name) #false]
           #:color [alt-color : Option-Fill-Paint #false]
           #:font [alt-font : Option-Font #false]
           #:alignment [alignment : Geo-Text-Alignment 'center]
           #:trim? [trim? : Boolean #true]
           [desc : Geo-Rich-Text]
           [style : (Dia-Block-Style-Spec S)]] : (Option Geo)
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
    (define bsf : Font (let ([b (dia-block-style-spec-backstop self)]) (dia-block-backstop-style-font b)))
    (define rtf : (Option Font+Tweak) (let ([rt (dia-block-style-spec-root self)]) (and rt (dia-block-style-font rt))))
    (define myf : (Option Font+Tweak) (dia-block-style-font (dia-block-style-spec-custom self)))

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

(define #:forall (S) dia-block-resolve-size : (->* ((Dia-Block-Style-Spec S))
                                                   (#:width Dia-Block-Option-Size #:height Dia-Block-Option-Size)
                                                   (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [style #:width [alt-width #false] #:height [alt-height #false]]
    (define rt (dia-block-style-spec-root style))
    (define maybe-width  (or alt-width  (dia-block-style-width  (dia-block-style-spec-custom style)) (and rt (dia-block-style-width  rt))))
    (define maybe-height (or alt-height (dia-block-style-height (dia-block-style-spec-custom style)) (and rt (dia-block-style-height rt))))
    (define Width  (dia-block-backstop-style-width  (dia-block-style-spec-backstop style)))
    (define Height (dia-block-backstop-style-height (dia-block-style-spec-backstop style)))
    (define scale (or (dia-block-style-spec-scale style) 1.0))

    (values (* scale (if (not  maybe-width)  Width (~dimension  maybe-width  Width)))
            (* scale (if (not maybe-height) Height (~dimension maybe-height Height))))))

(define #:forall (S) dia-block-resolve-width : (-> (Dia-Block-Style-Spec S) Nonnegative-Flonum)
  (lambda [style]
    (define rt (dia-block-style-spec-root style))
    (define maybe-width (or (dia-block-style-width  (dia-block-style-spec-custom style)) (and rt (dia-block-style-width  rt))))
    (define Width  (dia-block-backstop-style-width (dia-block-style-spec-backstop style)))
    
    (* (if (or maybe-width) (~dimension maybe-width Width) Width)
       (or (dia-block-style-spec-scale style) 1.0))))

(define #:forall (S) dia-block-resolve-padding : (->* ((Dia-Block-Style-Spec S))
                                                      (#:padding Dia-Block-Option-Padding)
                                                      Geo-Standard-Insets)
  (lambda [style #:padding [alt-padding #false]]
    (geo-insets-scale (geo-insets*->insets (or alt-padding
                                               (dia-block-style-padding (dia-block-style-spec-custom style))
                                               (let ([rt (dia-block-style-spec-root style)])
                                                 (and rt (dia-block-style-padding rt)))
                                               (dia-block-backstop-style-padding (dia-block-style-spec-backstop style)))
                                           (dia-block-resolve-width style))
                      (or (dia-block-style-spec-scale style) 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-resolve-stroke-width : (-> (Dia-Block-Style-Spec S) Nonnegative-Flonum)
  (lambda [self]
    (define width
      (or (dia-block-style-stroke-width (dia-block-style-spec-custom self))
          (let ([rt (dia-block-style-spec-root self)])
            (and rt (dia-block-style-stroke-width rt)))))

    (let ([paint (stroke-paint->source (dia-block-backstop-style-stroke-paint (dia-block-style-spec-backstop self)))])
      (cond [(not width) (pen-width paint)]
            [else (~dimension width (pen-width paint))]))))

(define #:forall (S) dia-block-resolve-stroke-paint : (-> (Dia-Block-Style-Spec S) (Option Pen))
  (lambda [self]
    (define rt (dia-block-style-spec-root self))
    (define me (dia-block-style-spec-custom self))
    
    (define c : Maybe-Color
      (let ([c (dia-block-style-stroke-color me)])
        (cond [(void? c) (if rt (dia-block-style-stroke-color rt) (void))]
              [else c])))

    (and c (let*-values ([(d+o) (or (dia-block-style-stroke-dash me) (and rt (dia-block-style-stroke-dash rt)))]
                         [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
             (desc-stroke* ; yes, don't scale stroke width by default
                           #:width (or (dia-block-style-stroke-width me) (and rt (dia-block-style-stroke-width rt)))
                           #:dash dash #:offset offset #:color (if (void? c) #false c)
                           #:opacity (dia-block-style-spec-opacity self)
                           (stroke-paint->source
                            (dia-block-backstop-style-stroke-paint
                             (dia-block-style-spec-backstop self))))))))

(define #:forall (S) dia-block-resolve-fill-paint : (-> (Dia-Block-Style-Spec S) (Option Brush))
  (lambda [self]
    (define paint : Maybe-Fill-Paint
      (let ([p (dia-block-style-fill-paint (dia-block-style-spec-custom self))])
        (cond [(void? p) (let ([rt (dia-block-style-spec-root self)]) (if rt (dia-block-style-fill-paint rt) (void)))]
              [else p])))
    
    (try-desc-brush* #:opacity (dia-block-style-spec-opacity self)
                     (fill-paint->source* (cond [(void? paint) (dia-block-backstop-style-fill-paint (dia-block-style-spec-backstop self))]
                                                [else paint])))))

(define #:forall (S) dia-block-resolve-font-paint : (-> (Dia-Block-Style-Spec S) Brush)
  (lambda [self]
    (desc-brush* #:opacity (dia-block-style-spec-opacity self)
                 (fill-paint->source (or (dia-block-style-font-paint (dia-block-style-spec-custom self))
                                         (let ([rt (dia-block-style-spec-root self)])
                                           (and rt (dia-block-style-font-paint rt)))
                                         (dia-block-backstop-style-font-paint (dia-block-style-spec-backstop self)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-caption-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-block-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

(define dia-block-caption-split-for-stereotype : (-> String (Values String (Option Symbol)))
  (lambda [text]
    (define maybe (regexp-match #px"(.+)#(\\w+)$" text))

    (cond [(not maybe) (values text #false)]
          [else (values (or (cadr maybe) text)
                        (let ([stype (caddr maybe)])
                          (and (string? stype)
                               (string->symbol stype))))])))

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
