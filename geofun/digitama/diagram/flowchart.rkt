#lang typed/racket/base

(provide (all-defined-out))

(require "configure/flowchart.rkt")

(require "../convert.rkt")
(require "../unsafe/path.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/constants.rkt")
(require "../layer/sticker.rkt")

(require "../dc/path.rkt")
(require "../dc/paint.rkt")
(require "../../paint.rkt")

(require "../../font.rkt")
(require "../../stroke.rkt")
(require "../../resize.rkt")
(require "../../composite.rkt")
(require "../../constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-chart-block-constructor : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (define-values (label config) (geo-flow-block-detect anchor))

    (and config
         (geo-flow-block label config))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block : (-> String Geo-Flow-Block-Config (U Geo Void))
  (lambda [text type]
    (cond [(geo-flow-process-config? type) (geo-flow-block-process text type)]
          [(geo-flow-decision-config? type) (geo-flow-block-decision text type)]
          [(geo-flow-input-config? type) (geo-flow-block-input text type)]
          [(geo-flow-output-config? type) (geo-flow-block-output text type)]
          [(geo-flow-start-config? type) (geo-flow-block-start text type)]
          [(geo-flow-stop-config? type) (geo-flow-block-stop text type)])))

(define geo-flow-block-process : (-> String Geo-Flow-Process-Config Geo)
  (lambda [text self]
    (define-values (label width height)
      (geo-flow-block-extent text
                             (geo-flow-process-config-font self) (geo-flow-process-config-font-paint self)
                             (geo-flow-process-config-block-width self) (geo-flow-process-config-block-height self)))
    
    (geo-cc-superimpose
     (geo-rectangle #:stroke (geo-flow-select-stroke-paint (geo-flow-process-config-stroke-paint self))
                    #:fill (geo-flow-select-fill-paint (geo-flow-process-config-fill-paint self))
                    width height)
     label)))

(define geo-flow-block-decision : (-> String Geo-Flow-Decision-Config Geo)
  (lambda [text self]
    (define-values (label width height)
      (geo-flow-block-extent text
                             (geo-flow-decision-config-font self) (geo-flow-decision-config-font-paint self)
                             (geo-flow-decision-config-block-width self) (geo-flow-decision-config-block-height self)))
    
    (geo-cc-superimpose
     (geo-rhombus #:stroke (geo-flow-select-stroke-paint (geo-flow-decision-config-stroke-paint self))
                  #:fill (geo-flow-select-fill-paint (geo-flow-decision-config-fill-paint self))
                  width height)
     label)))

(define geo-flow-block-start : (-> String Geo-Flow-Start-Config Geo)
  (lambda [text self]
    (geo-flow-block-terminal text
                             (geo-flow-start-config-font self) (geo-flow-start-config-font-paint self)
                             (geo-flow-start-config-stroke-paint self) (geo-flow-start-config-fill-paint self)
                             (geo-flow-start-config-block-width self) (geo-flow-start-config-block-height self))))

(define geo-flow-block-stop : (-> String Geo-Flow-Stop-Config Geo)
  (lambda [text self]
    (geo-flow-block-terminal text
                             (geo-flow-stop-config-font self) (geo-flow-stop-config-font-paint self)
                             (geo-flow-stop-config-stroke-paint self) (geo-flow-stop-config-fill-paint self)
                             (geo-flow-stop-config-block-width self) (geo-flow-stop-config-block-height self))))

(define geo-flow-block-input : (-> String Geo-Flow-Input-Config Geo)
  (lambda [text self]
    (geo-flow-block-dataIO text
                           (geo-flow-input-config-font self) (geo-flow-input-config-font-paint self)
                           (geo-flow-input-config-stroke-paint self) (geo-flow-input-config-fill-paint self)
                           (geo-flow-input-config-block-width self) (geo-flow-input-config-block-height self))))

(define geo-flow-block-output : (-> String Geo-Flow-Output-Config Geo)
  (lambda [text self]
    (geo-flow-block-dataIO text
                           (geo-flow-output-config-font self) (geo-flow-output-config-font-paint self)
                           (geo-flow-output-config-stroke-paint self) (geo-flow-output-config-fill-paint self)
                           (geo-flow-output-config-block-width self) (geo-flow-output-config-block-height self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-terminal : (-> String (Option Font) Option-Fill-Paint Maybe-Stroke-Paint Maybe-Fill-Paint
                                      (Option Nonnegative-Flonum) (Option Nonnegative-Flonum)
                                      Geo)
  (lambda [text block-font font-paint stroke-paint fill-paint block-width block-height]
    (define-values (label width height) (geo-flow-block-extent text block-font font-paint block-width block-height))
    (define r : Flonum (* height 0.5))
    (geo-cc-superimpose
     (geo-stadium #:stroke (geo-flow-select-stroke-paint stroke-paint)
                  #:fill (geo-flow-select-fill-paint fill-paint)
                  (- width (* r 2.0)) r)
     label)))

(define geo-flow-block-dataIO : (-> String (Option Font) Option-Fill-Paint Maybe-Stroke-Paint Maybe-Fill-Paint
                                    (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) Geo)
  (lambda [text block-font font-paint stroke-paint fill-paint block-width block-height]
    (define-values (label width height) (geo-flow-block-extent text block-font font-paint block-width block-height))
    (geo-cc-superimpose
     (geo-parallelogram #:stroke (geo-flow-select-stroke-paint stroke-paint)
                        #:fill (geo-flow-select-fill-paint fill-paint)
                        width height (/ pi 3.0))
     label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path->flow-chart : (-> Geo:Path Geo:Path)
  (lambda [self]
    self))

(define geo-flow-chart-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:path?])
      (define-values (xoff yoff) (geo-bbox-offset-values (geo:path-bbox self)))
      (path_stamp null xoff yoff
                  (current-stroke-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-extent : (-> String (Option Font) Option-Fill-Paint
                                    (Option Nonnegative-Flonum) (Option Nonnegative-Flonum)
                                    (Values Geo Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text block-font font-paint block-width block-height]
    (define label (geo-text text (or block-font (default-flow-font)) #:color (or font-paint (default-flow-font-paint))))
    (define width (or block-width (default-flow-block-width)))
    (define height (or block-height (default-flow-block-height)))
    
    (define-values (used-width used-height)
      (cond [(and (> width 0.0) (> height 0.0)) (values width height)]
            [else (let-values ([(lwidth lheight) (geo-flsize label)])
                    (values (if (> width 0.0) width lwidth)
                            (if (> height 0.0) height lheight)))]))

    (values label used-width used-height)))

(define geo-flow-select-stroke-paint : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint)
  (lambda [paint]
    (define flow-fallback (default-flow-stroke-paint))
    (cond [(void? paint) flow-fallback]
          [(stroke? paint) paint]
          [(stroke? flow-fallback) (desc-stroke flow-fallback #:color paint)]
          [else paint])))

(define geo-flow-select-fill-paint : (-> Maybe-Fill-Paint Maybe-Fill-Paint)
  (lambda [paint]
    (if (void? paint) (default-flow-fill-paint) paint)))
