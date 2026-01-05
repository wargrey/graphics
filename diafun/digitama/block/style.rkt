#lang typed/racket/base

(provide (all-defined-out))

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
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/spacing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Block-Style-Make* T S Property) (-> T Property (U S False Void)))
(define-type (Dia-Block-Style-Make S) (Dia-Block-Style-Make* Geo-Anchor-Name S (Option Symbol)))

(define-type (Dia-Block-Style-Layers* S) (Pairof S Dia-Block-Backstop-Style))
(define-type Dia-Block-Style-Layers (Dia-Block-Style-Layers* Dia-Block-Style))

(struct dia-block-style
  ([width : (Option Flonum)]
   [height : (Option Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint]
   [opacity : (Option Real)])
  #:type-name Dia-Block-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-block-backstop-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [font : Font]
   [font-paint : Fill-Paint]
   [stroke-paint : Option-Stroke-Paint]
   [fill-paint : Option-Fill-Paint]
   [opacity : (Option Real)])
  #:type-name Dia-Block-Backstop-Style
  #:transparent)

(define default-dia-block-margin : (Parameterof Geo-Spacing) (make-parameter 8.0))
(define default-dia-block-text-alignment : (Parameterof Geo-Text-Alignment) (make-parameter 'center))
(define default-dia-block-text-trim? : (Parameterof Boolean) (make-parameter #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-text-caption : (->* (Geo-Rich-Text Dia-Block-Style-Layers)
                                      (#:id Geo-Anchor-Name #:color Option-Fill-Paint #:font (Option Font))
                                      (Option Geo))
  (lambda [desc style #:id [id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define maybe-font : (Option Font) (or alt-font (dia-block-style-font (car style))))
    (define maybe-paint : Option-Fill-Paint (or alt-color (dia-block-style-font-paint (car style))))

    (define text : Geo-Rich-Text
      (cond [(string? desc) (if (default-dia-block-text-trim?) (string-trim desc) desc)]
            [(bytes? desc) (if (default-dia-block-text-trim?) (regexp-replace* #px"((^\\s*)|(\\s*$))" desc #"") desc)]
            [else desc]))
    
    (and (cond [(string? text) (> (string-length text) 0)]
               [(bytes? text) (> (bytes-length text) 0)]
               [else #true])
         (geo-rich-text-realize #:id (dia-block-caption-id (or id (gensym 'dia:block:caption:)))
                                #:alignment (default-dia-block-text-alignment)
                                text
                                (or maybe-font (dia-block-backstop-style-font (cdr style)))
                                (or maybe-paint (dia-block-backstop-style-font-paint (cdr style)))))))

(define dia-block-smart-size : (->* ((Option Geo) Dia-Block-Style-Layers)
                                    (#:width (Option Real) #:height (Option Real))
                                    (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [caption style #:width [alt-width #false] #:height [alt-height #false]]
    (define maybe-width  (if (not alt-width)  (dia-block-style-width (car style))  (real->double-flonum alt-width)))
    (define maybe-height (if (not alt-height) (dia-block-style-height (car style)) (real->double-flonum alt-height)))
    
    (define width  (or maybe-width  (dia-block-backstop-style-width  (cdr style))))
    (define height (or maybe-height (dia-block-backstop-style-height (cdr style))))

    (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
          [(not caption) (values 0.0 0.0)]
          [else (let-values ([(w h) (geo-flsize caption)])
                  (values (cond [(> width 0.0)  width]  [(< width 0.0)  (* w (abs width))]  [else w])
                          (cond [(> height 0.0) height] [(< height 0.0) (* h (abs height))] [else h])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-resolve-stroke-width : (-> Dia-Block-Style-Layers Nonnegative-Flonum)
  (let ([pens : (Weak-HashTable Any Nonnegative-Flonum) (make-weak-hash)])
   (lambda [self]
     (hash-ref! pens self
                (λ [] (let ([paint (stroke-paint->source (dia-block-backstop-style-stroke-paint (cdr self)))]
                            [width (dia-block-style-stroke-width (car self))])
                        (cond [(not width) (pen-width paint)]
                              [(>= width 0.0) width]
                              [(< width 0.0) (abs (* (pen-width paint) width))]
                              [else (pen-width paint)])))))))

(define dia-block-resolve-stroke-paint : (-> Dia-Block-Style-Layers (Option Pen))
   (let ([pens : (Weak-HashTable Any (Option Pen)) (make-weak-hash)])
     (lambda [self]
       (hash-ref! pens self
                  (λ [] (let ([c (dia-block-style-stroke-color (car self))])
                          (and c (let*-values ([(d+o) (dia-block-style-stroke-dash (car self))]
                                               [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
                                   (desc-stroke #:color (and (not (void? c)) c) #:opacity (dia-block-resolve-opacity self)
                                                #:width (dia-block-style-stroke-width (car self))
                                                #:dash dash #:offset offset
                                                (stroke-paint->source (dia-block-backstop-style-stroke-paint (cdr self))))))))))))

(define #:forall (S B) dia-block-resolve-fill-paint
  : (case-> [Dia-Block-Style-Layers -> (Option Brush)]
            [(Dia-Block-Style-Layers* (∩ S Dia-Block-Style)) (-> S Maybe-Fill-Paint) (-> Any Boolean : B) (-> B Option-Fill-Paint) -> (Option Brush)])
  (let ([brushs : (Weak-HashTable Any (Option Brush)) (make-weak-hash)])
   (case-lambda
     [(self)
      (hash-ref! brushs self
                 (λ [] (let ([paint (dia-block-style-fill-paint (car self))])                
                         (try-desc-brush #:opacity (dia-block-resolve-opacity self)
                                         (fill-paint->source* (cond [(not (void? paint)) paint]
                                                                    [else (dia-block-backstop-style-fill-paint (cdr self))]))))))]
     [(self S->paint style? B->paint)
      (define master (cdr self))
      (and (style? master)
           (hash-ref! brushs (list self S->paint B->paint)
                      (λ [] (try-desc-brush #:opacity (dia-block-resolve-opacity self)
                                            (fill-paint->source* (let ([paint (S->paint (car self))])
                                                                   (cond [(not (void? paint)) paint]
                                                                         [else (B->paint master)])))))))])))

(define dia-block-resolve-font-paint : (-> Dia-Block-Style-Layers Brush)
  (let ([brushs : (Weak-HashTable Any Brush) (make-weak-hash)])
    (lambda [self]
      (hash-ref! brushs self
                 (λ [] (desc-brush #:opacity (dia-block-resolve-opacity self)
                                   (fill-paint->source (or (dia-block-style-font-paint (car self))
                                                           (dia-block-backstop-style-font-paint (cdr self))))))))))

(define dia-block-resolve-font : (-> Dia-Block-Style-Layers Font)
  (lambda [self]
    (or (dia-block-style-font (car self)) (dia-block-backstop-style-font (cdr self)))))

(define dia-block-resolve-opacity : (-> Dia-Block-Style-Layers (Option Real))
  (lambda [self]
    (or (dia-block-style-opacity (car self))
        (dia-block-backstop-style-opacity (cdr self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-caption-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-block-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T S P) dia-block-style-construct : (-> T (Option (Dia-Block-Style-Make* T S P)) (-> S) P S)
  (lambda [anchor mk-style mk-fallback-style property]
    (define maybe-style (and mk-style (mk-style anchor property)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
