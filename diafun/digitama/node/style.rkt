#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/dc/text)
(require geofun/digitama/unsafe/dc/text-layout)

(require geofun/digitama/convert)
(require geofun/digitama/source)
(require geofun/digitama/base)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Node-Style-Make* S Hint) (-> Geo-Anchor-Name Hint (U S False Void)))
(define-type (Dia-Node-Style-Make Hint) (Dia-Node-Style-Make* Dia-Node-Style Hint))
(define-type (Dia-Path-Node-Style-Make S) (Dia-Node-Style-Make* S (Option Symbol)))

(struct dia-node-style
  ([width : (Option Flonum)]
   [height : (Option Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-width : (Option Flonum)]
   [stroke-color : (U Color Void False)]
   [stroke-dash : (Option Stroke-Dash-Datum)]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Dia-Node-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-node-base-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Dia-Node-Base-Style
  #:transparent)

(define make-null-node-style : (-> Dia-Node-Base-Style)
  (lambda []
    (dia-node-base-style 0.0 0.0 #false #false (void) (void))))

(define default-dia-node-margin : (Parameterof Nonnegative-Flonum) (make-parameter 8.0))
(define default-dia-node-text-alignment : (Parameterof Geo-Text-Alignment) (make-parameter 'center))
(define default-dia-node-text-trim? : (Parameterof Boolean) (make-parameter #true))
(define default-dia-node-base-style : (Parameterof (-> Dia-Node-Base-Style)) (make-parameter make-null-node-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-node-text-label : (->* (Geo-Anchor-Name String Dia-Node-Style)
                                   (#:id (Option Symbol) #:color Option-Fill-Paint #:font (Option Font))
                                   (Option Geo))
  (lambda [anchor desc s #:id [alt-id #false] #:color [alt-color #false] #:font [alt-font #false]]
    (define maybe-font : (Option Font) (or alt-font (dia-node-style-font s)))
    (define maybe-paint : Option-Fill-Paint (or alt-color (dia-node-style-font-paint s)))

    (define fallback-style : Dia-Node-Base-Style
      (if (and maybe-font maybe-paint)
          (make-null-node-style) ; useless but a bit more efficient
          ((default-dia-node-base-style))))

    (define font : (Option Font) (or maybe-font (dia-node-base-style-font fallback-style)))
    (define paint : Option-Fill-Paint (or maybe-paint (dia-node-base-style-font-paint fallback-style)))
    (define text : String (if (default-dia-node-text-trim?) (string-trim desc) desc))
    (define text-id : Symbol (or alt-id (dia-node-label-id anchor)))
    
    (and (non-empty-string? text)
         (geo-markup #:id text-id #:color paint #:alignment (default-dia-node-text-alignment)
                     #:error-color 'GhostWhite #:error-background 'Firebrick
                     text font))))

(define dia-node-smart-size : (-> (Option Geo) Dia-Node-Style (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [label s]
    (define maybe-width  (dia-node-style-width s))
    (define maybe-height (dia-node-style-height s))

    (define fallback-style : Dia-Node-Base-Style
      (if (and maybe-width maybe-height)
          (make-null-node-style) ; useless but a bit more efficient
          ((default-dia-node-base-style))))
    
    (define width  (or maybe-width  (dia-node-base-style-width  fallback-style)))
    (define height (or maybe-height (dia-node-base-style-height fallback-style)))
    
    (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
          [(not label) (values 0.0 0.0)]
          [else (let-values ([(w h) (geo-flsize label)])
                  (values (cond [(> width 0.0)  width]  [(< width 0.0)  (* w (abs width))]  [else w])
                          (cond [(> height 0.0) height] [(< height 0.0) (* h (abs height))] [else h])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-node-select-stroke-width : (-> Dia-Node-Style Nonnegative-Flonum)
  (lambda [self]
    (define paint (stroke-paint->source (dia-node-base-style-stroke-paint ((default-dia-node-base-style)))))
    (define width (dia-node-style-stroke-width self))

    (cond [(not width) (stroke-width paint)]
          [(>= width 0.0) width]
          [(< width 0.0) (abs (* (stroke-width paint) width))]
          [else (stroke-width paint)])))

(define dia-node-select-stroke-paint : (-> Dia-Node-Style Maybe-Stroke-Paint)
  (lambda [self]
    (define fallback-paint : (Option Stroke) (stroke-paint->source* (dia-node-base-style-stroke-paint ((default-dia-node-base-style)))))
    (define c (dia-node-style-stroke-color self))

    (and c
         (desc-stroke #:color (and (not (void? c)) c)
                      #:width (dia-node-style-stroke-width self)
                      #:dash (dia-node-style-stroke-dash self)
                      (if (stroke? fallback-paint) fallback-paint (default-stroke))))))

(define #:forall (T) dia-node-select-fill-paint : (case-> [Dia-Node-Style -> Maybe-Fill-Paint]
                                                          [Maybe-Fill-Paint (-> Dia-Node-Base-Style Maybe-Fill-Paint) -> Maybe-Fill-Paint]
                                                          [Maybe-Fill-Paint (-> Any Boolean : T) (-> T Maybe-Fill-Paint) -> Maybe-Fill-Paint])
  (case-lambda
    [(self)
    (let ([paint (dia-node-style-fill-paint self)])
      (cond [(not (void? paint)) paint]
            [else (dia-node-base-style-fill-paint ((default-dia-node-base-style)))]))]
    [(self style-fill-paint)
     (cond [(not (void? self)) self]
           [else (style-fill-paint ((default-dia-node-base-style)))])]
    [(self fallback? fallback-fill-paint)
     (cond [(not (void? self)) self]
           [else (let ([base ((default-dia-node-base-style))])
                   (and (fallback? base)
                        (fallback-fill-paint base)))])]))

(define #:forall (T) dia-node-select-font : (case-> [Dia-Node-Style -> (Option Font)]
                                                    [(Option Font) (-> Dia-Node-Base-Style (Option Font)) -> (Option Font)]
                                                    [(Option Font) (-> Any Boolean : T) (-> T (Option Font)) -> (Option Font)])
  (case-lambda
    [(self) (or (dia-node-style-font self) (dia-node-base-style-font ((default-dia-node-base-style))))]
    [(self style-font) (or self (style-font ((default-dia-node-base-style))))]
    [(self fallback? fallback-font)
     (or self
         (let ([base ((default-dia-node-base-style))])
           (and (fallback? base)
                (fallback-font base))))]))

(define #:forall (T) dia-node-select-font-paint : (case-> [Dia-Node-Style -> Option-Fill-Paint]
                                                          [Option-Fill-Paint (-> Dia-Node-Base-Style Option-Fill-Paint) -> Option-Fill-Paint]
                                                          [Option-Fill-Paint (-> Any Boolean : T) (-> T Option-Fill-Paint) -> Option-Fill-Paint])
  (case-lambda
    [(self) (or (dia-node-style-font-paint self) (dia-node-base-style-font-paint ((default-dia-node-base-style))))]
    [(self style-font-paint) (or self (style-font-paint ((default-dia-node-base-style))))]
    [(self fallback? fallback-font-paint)
     (or self
         (let ([base ((default-dia-node-base-style))])
           (and (fallback? base)
                (fallback-font-paint base))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-node-label-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "~" (geo-anchor->string anchor)))))

(define dia-node-shape-id : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (string->symbol (string-append "&" (geo-anchor->string anchor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S H) dia-node-style-construct : (-> Geo-Anchor-Name (Option (Dia-Node-Style-Make* S H)) (-> S) H S)
  (lambda [anchor mk-style mk-fallback-style hint]
    (define maybe-style (and mk-style (mk-style anchor hint)))

    (if (or (not maybe-style) (void? maybe-style))
        (mk-fallback-style)
        maybe-style)))
