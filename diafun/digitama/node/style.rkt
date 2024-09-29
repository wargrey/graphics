#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Node-Style-Make* S H) (-> Geo-Anchor-Name H (U S False Void)))
(define-type (Dia-Node-Style-Make H) (Dia-Node-Style-Make* Dia-Node-Style H))
(define-type Dia-Node-Id->String (U (HashTable Geo-Anchor-Name String) (-> Geo-Anchor-Name (U String Void False))))

(struct dia-node-style
  ([width : (Option Flonum)]
   [height : (Option Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
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
(define default-dia-node-base-style : (Parameterof (-> Dia-Node-Base-Style)) (make-parameter make-null-node-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-node-text-label : (-> Geo-Anchor-Name String Dia-Node-Style (Option Geo))
  (lambda [anchor desc this-style]
    (define fallback-style : Dia-Node-Base-Style ((default-dia-node-base-style)))
    (define font : (Option Font) (or (dia-node-style-font this-style) (dia-node-base-style-font fallback-style)))
    (define paint : Option-Fill-Paint (or (dia-node-style-font-paint this-style) (dia-node-base-style-font-paint fallback-style)))
    (define text : String (string-trim desc))
    (define text-id : Symbol (dia-node-label-id anchor))
    
    (cond [(regexp-match? #px"[\r\n]+" text)
           (geo-vc-append* #:id text-id
                           (for/list : (Listof Geo:Text)
                             ([l (in-lines (open-input-string text))])
                             (geo-text #:color paint (string-trim l) font)))]
          [(non-empty-string? text) (geo-text #:id text-id #:color paint text font)]
          [else #false])))

(define dia-node-smart-size : (-> (Option Geo) Dia-Node-Style (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [label this-style]
    (define fallback-style : Dia-Node-Base-Style ((default-dia-node-base-style)))
    
    (define-values (width height)
      (values (or (dia-node-style-width this-style)  (dia-node-base-style-width  fallback-style))
              (or (dia-node-style-height this-style) (dia-node-base-style-height fallback-style))))
    
    (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
          [(not label) (values 0.0 0.0)]
          [else (let-values ([(w h) (geo-flsize label)])
                  (values (cond [(> width 0.0)  width]  [(< width 0.0)  (* w (abs width))]  [else w])
                          (cond [(> height 0.0) height] [(< height 0.0) (* h (abs height))] [else h])))])))

(define dia-node-select-stroke-paint : (-> Dia-Node-Style Maybe-Stroke-Paint)
  (lambda [self]
    (define paint : Maybe-Stroke-Paint (dia-node-style-stroke-paint self))
    (define fallback-paint : Maybe-Stroke-Paint (dia-node-base-style-stroke-paint ((default-dia-node-base-style))))
    (cond [(void? paint) fallback-paint]
          [(not paint) #false]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define dia-node-select-fill-paint : (-> Dia-Node-Style Maybe-Fill-Paint)
  (lambda [self]
    (define paint : Maybe-Fill-Paint (dia-node-style-fill-paint self))

    (cond [(not (void? paint)) paint]
          [else (dia-node-base-style-fill-paint ((default-dia-node-base-style)))])))

(define dia-node-id-merge : (-> Symbol (Option Symbol) Symbol)
  (lambda [node-key hint]
    (if (or hint)
        (string->symbol (string-append (symbol->immutable-string node-key) ":"
                                       (symbol->immutable-string hint)))
        node-key)))

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
