#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Node-Style-Make* S) (-> Symbol Geo-Anchor-Name (U False Void S)))
(define-type Dia-Node-Style-Make (Dia-Node-Style-Make* Dia-Node-Style))
(define-type Dia-Node-Id->String (-> Symbol (U String Void False)))

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

(define default-dia-node-base-style : (Parameterof (-> Dia-Node-Base-Style))
  (make-parameter make-null-node-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-node-extent : (-> Symbol String Dia-Node-Style (Values (Option Geo) Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [node-key text0 this-style]
    (define fallback-style : Dia-Node-Base-Style ((default-dia-node-base-style)))
    (define font : (Option Font) (or (dia-node-style-font this-style) (dia-node-base-style-font fallback-style)))
    (define paint : Option-Fill-Paint (or (dia-node-style-font-paint this-style) (dia-node-base-style-font-paint fallback-style)))
    (define tid : Symbol (string->symbol (string-append "~" (geo-anchor->string node-key))))
    (define text : String (string-trim text0))
    
    (define label : (Option Geo)
      (cond [(regexp-match? #px"[\r\n]+" text)
             (geo-vc-append* #:id tid
                             (for/list : (Listof Geo:Text)
                               ([l (in-lines (open-input-string text))])
                               (geo-text #:color paint (string-trim l) font)))]
            [(non-empty-string? text) (geo-text #:id tid #:color paint text font)]
            [else #false]))
    
    (define-values (width height)
      (values (or (dia-node-style-width this-style)  (dia-node-base-style-width  fallback-style))
              (or (dia-node-style-height this-style) (dia-node-base-style-height fallback-style))))
    
    (define-values (used-width used-height)
      (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
            [(not label) (values 0.0 0.0)]
            [else (let-values ([(w h) (geo-flsize label)])
                    (values (cond [(> width 0.0)  width]  [(< width 0.0)  (* w (abs width))]  [else w])
                            (cond [(> height 0.0) height] [(< height 0.0) (* h (abs height))] [else h])))]))

    (values label used-width used-height)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-node-style-construct : (-> String Geo-Anchor-Name (Option (Dia-Node-Style-Make* S)) (-> S) (Values Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (define key : Symbol (string->symbol text))

    (values key
            (let ([maybe-style (and mk-style (mk-style key anchor))])
              (if (or (not maybe-style) (void? maybe-style))
                  (mk-fallback-style)
                  maybe-style)))))
