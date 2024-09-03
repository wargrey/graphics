#lang typed/racket/base

(provide (all-defined-out))

(require "style/node.rkt")
(require "style/flow.rkt")
(require "shape/flow.rkt")

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
    (define-values (text style) (geo-flow-block-detect anchor))

    (and style
         (parameterize ([default-geo-node-base-style make-geo-flow-node-style])
           (let-values ([(label width height) (geo-node-extent text style)])
             (cond [(geo-flow-process-style? style) (geo-flow-block-process label style width height)]
                   [(geo-flow-decision-style? style) (geo-flow-block-decision label style width height)]
                   [(geo-flow-input-style? style) (geo-flow-block-dataIO label style width height)]
                   [(geo-flow-output-style? style) (geo-flow-block-dataIO label style width height)]
                   [(geo-flow-preparation-style? style) (geo-flow-block-preparation label style width height)]
                   [(geo-flow-start-style? style) (geo-flow-block-terminal label style width height)]
                   [(geo-flow-stop-style? style) (geo-flow-block-terminal label style width height)]))))))

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
