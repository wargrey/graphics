#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/dc/text)
(require geofun/digitama/unsafe/dc/text-layout)

(require geofun/digitama/base)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Block-Style-Make* S Urgent) (-> Geo-Anchor-Name Urgent (U S False Void)))
(define-type (Dia-Block-Style-Make S) (Dia-Block-Style-Make* S (Option Symbol)))

(struct dia-block-style
  ([width : (Option Flonum)]
   [height : (Option Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash+Offset)]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Dia-Block-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-block-base-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Dia-Block-Base-Style
  #:transparent)

(define make-null-block-style : (-> Dia-Block-Base-Style)
  (lambda []
    (dia-block-base-style 0.0 0.0 #false #false (void) (void))))

(define default-dia-block-margin : (Parameterof Nonnegative-Flonum) (make-parameter 8.0))
(define default-dia-block-text-alignment : (Parameterof Geo-Text-Alignment) (make-parameter 'center))
(define default-dia-block-text-trim? : (Parameterof Boolean) (make-parameter #true))
(define default-dia-block-base-style : (Parameterof (-> Dia-Block-Base-Style)) (make-parameter make-null-block-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-text-brief : (->* (Geo-Anchor-Name DC-Markup-Text Dia-Block-Style)
                                    (#:id (Option Symbol) #:color Option-Fill-Paint #:font (Option Font))
                                    (Option Geo))
  (lambda [anchor desc s #:id [alt-id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define maybe-font : (Option Font) (or alt-font (dia-block-style-font s)))
    (define maybe-paint : Option-Fill-Paint (or alt-color (dia-block-style-font-paint s)))

    (define fallback-style : Dia-Block-Base-Style
      (if (and maybe-font maybe-paint)
          (make-null-block-style) ; useless but a bit more efficient
          ((default-dia-block-base-style))))

    (define font : (Option Font) (or maybe-font (dia-block-base-style-font fallback-style)))
    (define paint : Option-Fill-Paint (or maybe-paint (dia-block-base-style-font-paint fallback-style)))
    (define text-id : Symbol (or alt-id (dia-block-brief-id anchor)))
    (define text : DC-Markup-Text
      (cond [(string? desc) (if (default-dia-block-text-trim?) (string-trim desc) desc)]
            [(bytes? desc) (if (default-dia-block-text-trim?) (regexp-replace* #px"((^\\s*)|(\\s*$))" desc #"") desc)]
            [else desc]))
    
    (and (cond [(string? text) (> (string-length text) 0)]
               [(bytes? text) (> (bytes-length text) 0)]
               [else #true])
         (geo-markup #:id text-id #:color paint #:alignment (default-dia-block-text-alignment)
                     #:error-color 'GhostWhite #:error-background 'Firebrick
                     text font))))

(define dia-block-smart-size : (->* ((Option Geo) Dia-Block-Style)
                                    (#:width (Option Real) #:height (Option Real))
                                    (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [brief s #:width [alt-width #false] #:height [alt-height #false]]
    (define maybe-width  (if (not alt-width)  (dia-block-style-width s)  (real->double-flonum alt-width)))
    (define maybe-height (if (not alt-height) (dia-block-style-height s) (real->double-flonum alt-height)))

    (define fallback-style : Dia-Block-Base-Style
      (if (and maybe-width maybe-height)
          (make-null-block-style) ; useless but a bit more efficient
          ((default-dia-block-base-style))))
    
    (define width  (or maybe-width  (dia-block-base-style-width  fallback-style)))
    (define height (or maybe-height (dia-block-base-style-height fallback-style)))
    
    (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
          [(not brief) (values 0.0 0.0)]
          [else (let-values ([(w h) (geo-flsize brief)])
                  (values (cond [(> width 0.0)  width]  [(< width 0.0)  (* w (abs width))]  [else w])
                          (cond [(> height 0.0) height] [(< height 0.0) (* h (abs height))] [else h])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-select-stroke-width : (-> Dia-Block-Style Nonnegative-Flonum)
  (lambda [self]
    (define paint (stroke-paint->source (dia-block-base-style-stroke-paint ((default-dia-block-base-style)))))
    (define width (dia-block-style-stroke-width self))

    (cond [(not width) (pen-width paint)]
          [(>= width 0.0) width]
          [(< width 0.0) (abs (* (pen-width paint) width))]
          [else (pen-width paint)])))

(define dia-block-select-stroke-paint : (-> Dia-Block-Style Maybe-Stroke-Paint)
  (lambda [self]
    (define fallback-paint : (Option Pen) (stroke-paint->source* (dia-block-base-style-stroke-paint ((default-dia-block-base-style)))))
    (define c (dia-block-style-stroke-color self))

    (and c
         (let*-values ([(d+o) (dia-block-style-stroke-dash self)]
                       [(dash offset) (if (pair? d+o) (values (car d+o) (cdr d+o)) (values d+o #false))])
           (desc-stroke #:color (and (not (void? c)) c)
                        #:width (dia-block-style-stroke-width self)
                        #:dash dash #:offset offset
                        (if (pen? fallback-paint) fallback-paint (default-stroke)))))))

(define #:forall (T) dia-block-select-fill-paint : (case-> [Dia-Block-Style -> Maybe-Fill-Paint]
                                                           [Maybe-Fill-Paint (-> Dia-Block-Base-Style Maybe-Fill-Paint) -> Maybe-Fill-Paint]
                                                           [Maybe-Fill-Paint (-> Any Boolean : T) (-> T Maybe-Fill-Paint) -> Maybe-Fill-Paint])
  (case-lambda
    [(self)
     (let ([paint (dia-block-style-fill-paint self)])
       (cond [(not (void? paint)) paint]
             [else (dia-block-base-style-fill-paint ((default-dia-block-base-style)))]))]
    [(self style-fill-paint)
     (cond [(not (void? self)) self]
           [else (style-fill-paint ((default-dia-block-base-style)))])]
    [(self fallback? fallback-fill-paint)
     (cond [(not (void? self)) self]
           [else (let ([base ((default-dia-block-base-style))])
                   (and (fallback? base)
                        (fallback-fill-paint base)))])]))

(define #:forall (T) dia-block-select-font : (case-> [Dia-Block-Style -> (Option Font)]
                                                     [(Option Font) (-> Dia-Block-Base-Style (Option Font)) -> (Option Font)]
                                                     [(Option Font) (-> Any Boolean : T) (-> T (Option Font)) -> (Option Font)])
  (case-lambda
    [(self) (or (dia-block-style-font self) (dia-block-base-style-font ((default-dia-block-base-style))))]
    [(self style-font) (or self (style-font ((default-dia-block-base-style))))]
    [(self fallback? fallback-font)
     (or self
         (let ([base ((default-dia-block-base-style))])
           (and (fallback? base)
                (fallback-font base))))]))

(define #:forall (T) dia-block-select-font-paint : (case-> [Dia-Block-Style -> Option-Fill-Paint]
                                                           [Option-Fill-Paint (-> Dia-Block-Base-Style Option-Fill-Paint) -> Option-Fill-Paint]
                                                           [Option-Fill-Paint (-> Any Boolean : T) (-> T Option-Fill-Paint) -> Option-Fill-Paint])
  (case-lambda
    [(self) (or (dia-block-style-font-paint self) (dia-block-base-style-font-paint ((default-dia-block-base-style))))]
    [(self style-font-paint) (or self (style-font-paint ((default-dia-block-base-style))))]
    [(self fallback? fallback-font-paint)
     (or self
         (let ([base ((default-dia-block-base-style))])
           (and (fallback? base)
                (fallback-font-paint base))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-brief-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-block-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

(define dia-block-cell-id : (-> Symbol Symbol Index Index Symbol)
  (lambda [id type row col]
    (string->symbol (format "~a:~a:~a:~a" id type row col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S U) dia-block-style-construct : (-> Geo-Anchor-Name (Option (Dia-Block-Style-Make* S U)) (-> S) U S)
  (lambda [anchor mk-style mk-fallback-style urgent]
    (define maybe-style (and mk-style (mk-style anchor urgent)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
