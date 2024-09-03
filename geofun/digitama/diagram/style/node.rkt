#lang typed/racket/base

(provide (all-defined-out))

(require "../../geometry/trail.rkt")

(require "../../convert.rkt")
(require "../../dc/text.rkt")

(require "../../../font.rkt")
(require "../../../paint.rkt")
(require "../../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Geo-Node-Style-Refine* G) (-> Symbol G Geo-Anchor-Name G))
(define-type Geo-Node-Style-Refine (Geo-Node-Style-Refine* Geo-Node-Style))

(struct geo-node-style
  ([width : (Option Nonnegative-Flonum)]
   [height : (Option Nonnegative-Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Geo-Node-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-node-base-style
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Geo-Node-Base-Style
  #:transparent)

(define make-null-node-style : (-> Geo-Node-Base-Style)
  (lambda []
    (geo-node-base-style 0.0 0.0 #false #false (void) (void))))

(define default-geo-node-base-style : (Parameterof (-> Geo-Node-Base-Style))
  (make-parameter make-null-node-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-node-extent : (-> String Geo-Node-Style (Values Geo:Text Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text this-style]
    (define fallback-style : Geo-Node-Base-Style ((default-geo-node-base-style)))
    
    (define label : Geo:Text
      (geo-text #:color (or (geo-node-style-font-paint this-style)
                            (geo-node-base-style-font-paint fallback-style))
                text (or (geo-node-style-font this-style)
                         (geo-node-base-style-font fallback-style))))
    
    (define-values (width height)
      (values (or (geo-node-style-width this-style) (geo-node-base-style-width fallback-style))
              (or (geo-node-style-height this-style) (geo-node-base-style-height fallback-style))))
    
    (define-values (used-width used-height)
      (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
            [else (let-values ([(lwidth lheight) (geo-flsize label)])
                    (values (if (> width 0.0) width lwidth)
                            (if (> height 0.0) height lheight)))]))

    (values label used-width used-height)))

(define geo-node-select-stroke-paint : (-> Geo-Node-Style Maybe-Stroke-Paint)
  (lambda [this-style]
    (define paint : Maybe-Stroke-Paint (geo-node-style-stroke-paint this-style))
    (define fallback-paint : Maybe-Stroke-Paint (geo-node-base-style-stroke-paint ((default-geo-node-base-style))))
    (cond [(void? paint) fallback-paint]
          [(stroke? paint) paint]
          [(stroke? fallback-paint) (desc-stroke fallback-paint #:color paint)]
          [else paint])))

(define geo-node-select-fill-paint : (-> Geo-Node-Style Maybe-Fill-Paint)
  (lambda [this-style]
    (define paint : Maybe-Fill-Paint (geo-node-style-fill-paint this-style))
    (define fallback-paint : Maybe-Fill-Paint (geo-node-base-style-fill-paint ((default-geo-node-base-style))))
    
    (if (void? paint) fallback-paint paint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (C) geo-node-style-refine : (-> String Geo-Anchor-Name (-> Symbol String)
                                                      (-> C) (Option (Geo-Node-Style-Refine* C))
                                                      (Values String C))
  (lambda [text anchor mk-label mk-style refine]
    (define key : Symbol (string->symbol text))
    (if (not refine)
        (values (mk-label key) (mk-style))
        (values (mk-label key) (refine key (mk-style) anchor)))))