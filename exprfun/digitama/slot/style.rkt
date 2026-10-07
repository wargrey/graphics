#lang typed/racket/base

;;; the plt RAM module depends on this module

(provide (all-defined-out) Geo-Insets-Datum)

(require digimon/struct)
(require digimon/measure)

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
(define-type Expr-Slot-Option-Size (Option Length+%))
(define-type Expr-Slot-Padding Geo-Insets-Datum+%)
(define-type Expr-Slot-Option-Padding (Option Expr-Slot-Padding))

(define-type (Expr-Slot-Theme-Adjuster Datum Style Metadata)
  (-> (Expr-Slot-Style Style) Symbol Datum Metadata
      (U (Expr-Slot-Style Style) False Void)))

(define-struct expr-slot-style : Expr-Slot-Style
  #:forall ([phantom-type : T])
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint]
   [opacity : (Option Real)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct expr-slot-backstop-style
  ([padding : Expr-Slot-Padding]
   [font : Font]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) expr-slot-text-term : (->* (Geo-Rich-Text (Expr-Slot-Style-Spec S))
                                                (#:id Geo-Anchor-Name #:color Option-Fill-Paint #:font (Option Font)
                                                 #:trim? Boolean)
                                                (Option Geo))
  (lambda [term style #:id [id #false] #:color [alt-color #false] #:font [alt-font #false] #:trim? [trim? #true]]
    (define font : Font (expr-slot-resolve-font style))
    (define paint : Option-Fill-Paint (expr-slot-resolve-font-paint style))
    
    (geo-rich-text-try-realize #:id (expr-slot-term-id (or id (gensym 'dia:block:caption:)))
                               #:alignment 'center #:trim? trim?
                               term font paint)))

(define #:forall (S) expr-slot-resolve-padding : (->* ((Expr-Slot-Style-Spec S) Nonnegative-Flonum Nonnegative-Flonum)
                                                      (#:padding Expr-Slot-Option-Padding)
                                                      Geo-Standard-Insets)
  (lambda [style width height #:padding [alt-padding #false]]
    (geo-insets*->insets (or alt-padding
                             (expr-slot-style-padding (expr-slot-style-spec-custom style))
                             (expr-slot-backstop-style-padding (expr-slot-style-spec-backstop style)))
                         width)))

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
  (lambda [self]
    (desc-brush #:opacity (expr-slot-resolve-opacity self)
                (fill-paint->source (or (expr-slot-style-font-paint (expr-slot-style-spec-custom self))
                                        (expr-slot-backstop-style-font-paint (expr-slot-style-spec-backstop self)))))))

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
