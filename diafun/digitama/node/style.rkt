#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/geometry/anchor)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Node-Style-Make* S) (-> Symbol Geo-Anchor-Name (Option S)))
(define-type Dia-Node-Style-Make (Dia-Node-Style-Make* Dia-Node-Style))

(struct dia-node-style
  ([width : (Option Nonnegative-Flonum)]
   [height : (Option Nonnegative-Flonum)]
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
(define dia-node-extent : (-> Symbol String Dia-Node-Style (Values Geo:Text Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [node-key text this-style]
    (define fallback-style : Dia-Node-Base-Style ((default-dia-node-base-style)))
    
    (define label : Geo:Text
      (geo-text #:id (string->symbol (string-append "~" (geo-anchor->string node-key)))
                #:color (or (dia-node-style-font-paint this-style)
                            (dia-node-base-style-font-paint fallback-style))
                text (or (dia-node-style-font this-style)
                         (dia-node-base-style-font fallback-style))))
    
    (define-values (width height)
      (values (or (dia-node-style-width this-style) (dia-node-base-style-width fallback-style))
              (or (dia-node-style-height this-style) (dia-node-base-style-height fallback-style))))
    
    (define-values (used-width used-height)
      (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
            [else (let-values ([(lwidth lheight) (geo-flsize label)])
                    (values (if (> width 0.0) width lwidth)
                            (if (> height 0.0) height lheight)))]))

    (values label used-width used-height)))

(define dia-node-select-stroke-paint : (-> Dia-Node-Style Maybe-Stroke-Paint)
  (lambda [this-style]
    (define paint : Maybe-Stroke-Paint (dia-node-style-stroke-paint this-style))
    (define fallback-paint : Maybe-Stroke-Paint (dia-node-base-style-stroke-paint ((default-dia-node-base-style))))
    (cond [(void? paint) fallback-paint]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define dia-node-select-fill-paint : (-> Dia-Node-Style Maybe-Fill-Paint)
  (lambda [this-style]
    (define paint : Maybe-Fill-Paint (dia-node-style-fill-paint this-style))
    (define fallback-paint : Maybe-Fill-Paint (dia-node-base-style-fill-paint ((default-dia-node-base-style))))
    
    (if (void? paint) fallback-paint paint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-node-style-construct : (-> String Geo-Anchor-Name (Option (Dia-Node-Style-Make* S)) (-> S) (Values Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (define key : Symbol (string->symbol text))

    (values key
            (or (and mk-style (mk-style key anchor))
                (mk-fallback-style)))))
