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

(define geo-flow-block : (-> String Geo-Flow-Block-Config (U Geo Void))
  (lambda [text type]
    (define-values (label width height)
      (geo-flow-block-extent text
                             (geo-flow-block-config-font type) (geo-flow-block-config-font-paint type)
                             (geo-flow-block-config-block-width type) (geo-flow-block-config-block-height type)))
    
    (cond [(geo-flow-process-config? type) (geo-flow-block-process label type width height)]
          [(geo-flow-decision-config? type) (geo-flow-block-decision label type width height)]
          [(geo-flow-input-config? type) (geo-flow-block-dataIO label type width height)]
          [(geo-flow-output-config? type) (geo-flow-block-dataIO label type width height)]
          [(geo-flow-preparation-config? type) (geo-flow-block-preparation label type width height)]
          [(geo-flow-start-config? type) (geo-flow-block-terminal label type width height)]
          [(geo-flow-stop-config? type) (geo-flow-block-terminal label type width height)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-process : (-> Geo:Text Geo-Flow-Block-Config Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label self width height]
    (geo-cc-superimpose
     (geo-rectangle #:stroke (geo-flow-select-stroke-paint (geo-flow-block-config-stroke-paint self))
                    #:fill (geo-flow-select-fill-paint (geo-flow-block-config-fill-paint self))
                    width height)
     label)))

(define geo-flow-block-decision : (-> Geo:Text Geo-Flow-Block-Config Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label self width height]
    (geo-cc-superimpose
     (geo-rhombus #:stroke (geo-flow-select-stroke-paint (geo-flow-block-config-stroke-paint self))
                  #:fill (geo-flow-select-fill-paint (geo-flow-block-config-fill-paint self))
                  width height)
     label)))

(define geo-flow-block-preparation : (-> Geo:Text Geo-Flow-Block-Config Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label self width height]
    (geo-cc-superimpose
     (geo-hexagon-tile #:stroke (geo-flow-select-stroke-paint (geo-flow-block-config-stroke-paint self))
                       #:fill (geo-flow-select-fill-paint (geo-flow-block-config-fill-paint self))
                       width height)
     label)))

(define geo-flow-block-terminal : (-> Geo:Text Geo-Flow-Block-Config Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label self width height]
    (define r : Flonum (* height 0.5))
    (geo-cc-superimpose
     (geo-stadium #:stroke (geo-flow-select-stroke-paint (geo-flow-block-config-stroke-paint self))
                  #:fill (geo-flow-select-fill-paint (geo-flow-block-config-fill-paint self))
                  (- width (* r 2.0)) r)
     label)))

(define geo-flow-block-dataIO : (-> Geo:Text Geo-Flow-Block-Config Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label self width height]
    (geo-cc-superimpose
     (geo-parallelogram #:stroke (geo-flow-select-stroke-paint (geo-flow-block-config-stroke-paint self))
                        #:fill (geo-flow-select-fill-paint (geo-flow-block-config-fill-paint self))
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
                                    (Values Geo:Text Nonnegative-Flonum Nonnegative-Flonum))
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
