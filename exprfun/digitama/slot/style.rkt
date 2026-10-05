#lang typed/racket/base

(provide (all-defined-out) Geo-Insets-Datum)

(require digimon/struct)
(require racket/string)

(require geofun/font)
(require geofun/stroke)
(require geofun/fill)

(require geofun/digitama/base)
(require geofun/digitama/self)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/track/anchor)
(require geofun/digitama/geometry/sides)

(require geofun/digitama/richtext/self)
(require geofun/digitama/richtext/realize)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (expr-slot-case stx)
  (syntax-parse stx #:literals [:]
    [(_ style [(pred? ...) adjust ...] ...)
     (syntax/loc stx
       (let ([self (expr-slot-style-spec-custom style)])
         (cond [(or (expr-slot-style?? self pred?) ...) adjust ...]
               ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Expr-Slot-Theme-Adjuster Datum Style Metadata)
  (-> (Expr-Slot-Style Style) Symbol Datum Metadata
      (U (Expr-Slot-Style Style) False Void)))

(define-struct expr-slot-style : Expr-Slot-Style
  #:forall ([phantom-type : T])
  ([font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint]
   [opacity : (Option Real)])
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

(define-struct #:forall (S) expr-slot-style-spec : Expr-Slot-Style-Spec
  ([custom : (Expr-Slot-Style S)]
   [backstop : Expr-Slot-Backstop-Style])
  #:transparent)

(define default-expr-slot-margin : (Parameterof Geo-Insets-Datum) (make-parameter 4.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) expr-slot-text-term : (->* (Geo-Rich-Text (Expr-Slot-Style-Spec S))
                                                (#:id Geo-Anchor-Name #:color Option-Fill-Paint #:font (Option Font))
                                                (Option Geo))
  (lambda [term style #:id [id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define self : (Expr-Slot-Style S) (expr-slot-style-spec-custom style))
    (define fallback : Expr-Slot-Backstop-Style (expr-slot-style-spec-backstop style))
    
    (define maybe-font : (Option Font) (or alt-font (expr-slot-style-font self)))
    (define maybe-paint : Option-Fill-Paint (or alt-color (expr-slot-style-font-paint self)))

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
                                (or maybe-font (expr-slot-backstop-style-font fallback))
                                (or maybe-paint (expr-slot-backstop-style-font-paint fallback))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) expr-slot-resolve-stroke-width : (-> (Expr-Slot-Style-Spec S) Nonnegative-Flonum)
  (lambda [self]
    (define paint (stroke-paint->source (expr-slot-backstop-style-stroke-paint (expr-slot-style-spec-backstop self))))
    (define width (expr-slot-style-stroke-width (expr-slot-style-spec-custom self)))

    (cond [(not width) (pen-width paint)]
          [(>= width 0.0) width]
          [(< width 0.0) (abs (* (pen-width paint) width))]
          [else (pen-width paint)])))

(define #:forall (S) expr-slot-resolve-stroke-paint : (-> (Expr-Slot-Style-Spec S) (Option Pen))
  (lambda [self]
    (define fb (expr-slot-style-spec-backstop self))
    (define s (expr-slot-style-spec-custom self))
    (define c (expr-slot-style-stroke-color s))

    (and c (let*-values ([(d+o) (expr-slot-style-stroke-dash s)]
                         [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
             (desc-stroke #:color (and (not (void? c)) c) #:opacity (expr-slot-resolve-opacity self)
                          #:width (expr-slot-style-stroke-width s)
                          #:dash dash #:offset offset
                          (stroke-paint->source (expr-slot-backstop-style-stroke-paint fb)))))))

(define #:forall (S) expr-slot-resolve-fill-paint : (-> (Expr-Slot-Style-Spec S) (Option Brush))
  (lambda [self]
    (define paint (expr-slot-style-fill-paint (expr-slot-style-spec-custom self)))

    (try-desc-brush #:opacity (expr-slot-resolve-opacity self)
                    (fill-paint->source* (cond [(not (void? paint)) paint]
                                               [else (expr-slot-backstop-style-fill-paint (expr-slot-style-spec-backstop self))])))))

(define #:forall (S) expr-slot-resolve-font-paint : (-> (Expr-Slot-Style-Spec S) Brush)
  (let ([brushs : (Weak-HashTable Any Brush) (make-weak-hash)])
    (lambda [self]
      (hash-ref! brushs self
                 (λ [] (desc-brush #:opacity (expr-slot-resolve-opacity self)
                                   (fill-paint->source (or (expr-slot-style-font-paint (expr-slot-style-spec-custom self))
                                                           (expr-slot-backstop-style-font-paint (expr-slot-style-spec-backstop self))))))))))

(define #:forall (S) expr-slot-resolve-font : (-> (Expr-Slot-Style-Spec S) Font)
  (lambda [self]
    (or (expr-slot-style-font (expr-slot-style-spec-custom self))
        (expr-slot-backstop-style-font (expr-slot-style-spec-backstop self)))))

(define #:forall (S) expr-slot-resolve-opacity : (-> (Expr-Slot-Style-Spec S) (Option Real))
  (lambda [self]
    (or (expr-slot-style-opacity (expr-slot-style-spec-custom self))
        (expr-slot-backstop-style-opacity (expr-slot-style-spec-backstop self)))))

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
(define #:forall (D S P) expr-slot-theme-adjust : (-> (Expr-Slot-Style S) Symbol D (Option (Expr-Slot-Theme-Adjuster D S P)) P
                                                      (Expr-Slot-Style S))
  (lambda [the-style id datum maybe-adjuster property]
    (if (and maybe-adjuster)
        (let ([maybe-adjusted-style (maybe-adjuster the-style id datum property)])
          (cond [(void? maybe-adjusted-style) the-style]
                [(not maybe-adjusted-style) the-style]
                [else maybe-adjusted-style]))
        the-style)))
