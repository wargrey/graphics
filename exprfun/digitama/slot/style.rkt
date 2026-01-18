#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/font)
(require geofun/stroke)
(require geofun/fill)

(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/track/anchor)
(require geofun/digitama/geometry/insets)

(require geofun/digitama/richtext/self)
(require geofun/digitama/richtext/realize)

(require geofun/digitama/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Expr-Slot-Style-Make D S Property) (-> Symbol D Property (U S False Void)))

(define-type (Expr-Slot-Style-Layers* S) (Pairof S Expr-Slot-Backstop-Style))
(define-type Expr-Slot-Style-Layers (Expr-Slot-Style-Layers* Expr-Slot-Style))

(struct expr-slot-style
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint]
   [opacity : (Option Real)])
  #:type-name Expr-Slot-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct expr-slot-backstop-style
  ([font : Font]
   [font-paint : Fill-Paint]
   [stroke-paint : Option-Stroke-Paint]
   [fill-paint : Option-Fill-Paint]
   [opacity : (Option Real)])
  #:type-name Expr-Slot-Backstop-Style
  #:transparent)

(define default-expr-slot-margin : (Parameterof Geo-Insets-Datum) (make-parameter 4.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expr-slot-text-term : (->* (Geo-Rich-Text Expr-Slot-Style-Layers)
                                   (#:id Geo-Anchor-Name #:color Option-Fill-Paint #:font (Option Font))
                                   (Option Geo))
  (lambda [term style #:id [id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define maybe-font : (Option Font) (or alt-font (expr-slot-style-font (car style))))
    (define maybe-paint : Option-Fill-Paint (or alt-color (expr-slot-style-font-paint (car style))))

    (define text : Geo-Rich-Text
      (cond [(string? term) (string-trim term)]
            [(bytes? term) (regexp-replace* #px"((^\\s*)|(\\s*$))" term #"")]
            [else term]))
    
    (and (cond [(string? text) (> (string-length text) 0)]
               [(bytes? text) (> (bytes-length text) 0)]
               [else #true])
         (geo-rich-text-realize #:id (expr-slot-term-id (or id (gensym 'dia:block:caption:)))
                                #:alignment 'center
                                text
                                (or maybe-font (expr-slot-backstop-style-font (cdr style)))
                                (or maybe-paint (expr-slot-backstop-style-font-paint (cdr style)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expr-slot-resolve-stroke-width : (-> Expr-Slot-Style-Layers Nonnegative-Flonum)
  (let ([pens : (Weak-HashTable Any Nonnegative-Flonum) (make-weak-hash)])
   (lambda [self]
     (hash-ref! pens self
                (λ [] (let ([paint (stroke-paint->source (expr-slot-backstop-style-stroke-paint (cdr self)))]
                            [width (expr-slot-style-stroke-width (car self))])
                        (cond [(not width) (pen-width paint)]
                              [(>= width 0.0) width]
                              [(< width 0.0) (abs (* (pen-width paint) width))]
                              [else (pen-width paint)])))))))

(define expr-slot-resolve-stroke-paint : (-> Expr-Slot-Style-Layers (Option Pen))
   (let ([pens : (Weak-HashTable Any (Option Pen)) (make-weak-hash)])
     (lambda [self]
       (hash-ref! pens self
                  (λ [] (let ([c (expr-slot-style-stroke-color (car self))])
                          (and c (let*-values ([(d+o) (expr-slot-style-stroke-dash (car self))]
                                               [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
                                   (desc-stroke #:color (and (not (void? c)) c) #:opacity (expr-slot-resolve-opacity self)
                                                #:width (expr-slot-style-stroke-width (car self))
                                                #:dash dash #:offset offset
                                                (stroke-paint->source (expr-slot-backstop-style-stroke-paint (cdr self))))))))))))

(define #:forall (S B) expr-slot-resolve-fill-paint
  : (case-> [Expr-Slot-Style-Layers -> (Option Brush)]
            [(Expr-Slot-Style-Layers* (∩ S Expr-Slot-Style)) (-> S Maybe-Fill-Paint) (-> Any Boolean : B) (-> B Option-Fill-Paint) -> (Option Brush)])
  (let ([brushs : (Weak-HashTable Any (Option Brush)) (make-weak-hash)])
   (case-lambda
     [(self)
      (hash-ref! brushs self
                 (λ [] (let ([paint (expr-slot-style-fill-paint (car self))])                
                         (try-desc-brush #:opacity (expr-slot-resolve-opacity self)
                                         (fill-paint->source* (cond [(not (void? paint)) paint]
                                                                    [else (expr-slot-backstop-style-fill-paint (cdr self))]))))))]
     [(self S->paint style? B->paint)
      (define master (cdr self))
      (and (style? master)
           (hash-ref! brushs (list self S->paint B->paint)
                      (λ [] (try-desc-brush #:opacity (expr-slot-resolve-opacity self)
                                            (fill-paint->source* (let ([paint (S->paint (car self))])
                                                                   (cond [(not (void? paint)) paint]
                                                                         [else (B->paint master)])))))))])))

(define expr-slot-resolve-font-paint : (-> Expr-Slot-Style-Layers Brush)
  (let ([brushs : (Weak-HashTable Any Brush) (make-weak-hash)])
    (lambda [self]
      (hash-ref! brushs self
                 (λ [] (desc-brush #:opacity (expr-slot-resolve-opacity self)
                                   (fill-paint->source (or (expr-slot-style-font-paint (car self))
                                                           (expr-slot-backstop-style-font-paint (cdr self))))))))))

(define expr-slot-resolve-font : (-> Expr-Slot-Style-Layers Font)
  (lambda [self]
    (or (expr-slot-style-font (car self)) (expr-slot-backstop-style-font (cdr self)))))

(define expr-slot-resolve-opacity : (-> Expr-Slot-Style-Layers (Option Real))
  (lambda [self]
    (or (expr-slot-style-opacity (car self))
        (expr-slot-backstop-style-opacity (cdr self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expr-slot-term-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define expr-slot-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

(define expr-slot-cell-id : (-> Symbol Symbol Index Index Symbol)
  (lambda [id type row col]
    (string->symbol (format "~a:~a:~a:~a" id type row col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (D S P) expr-slot-style-construct : (-> Symbol D (Option (Expr-Slot-Style-Make D S P)) (-> S) P S)
  (lambda [id datum mk-style mk-fallback-style property]
    (define maybe-style (and mk-style (mk-style id datum property)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
