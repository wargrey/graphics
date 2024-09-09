#lang typed/racket/base

(provide (all-defined-out) Geo-Flow-Arrow->Edge)
(provide Geo-Node-Style-Make* Geo-Node-Style-Make)
(provide Geo-Edge-Style-Make* Geo-Edge-Style-Make)
(provide (all-from-out "digitama/diagram/style/flow.rkt"))
(provide default-flow-chart-node-constructor)
(provide default-flow-chart-arrow-constructor)
(provide geo-edge geo:edge? Geo:Edge)

(require "digitama/composite.rkt")
(require "digitama/convert.rkt")

(require "digitama/diagram/style/flow.rkt")
(require "digitama/diagram/style/node.rkt")
(require "digitama/diagram/style/edge.rkt")

(require "digitama/diagram/flowchart.rkt")
(require "digitama/geometry/anchor.rkt")
(require "digitama/layer/sticker.rkt")
(require "digitama/layer/type.rkt")

(require "digitama/dc/composite.rkt")
(require "digitama/dc/path.rkt")
(require "digitama/dc/edge.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option Geo-Trusted-Anchors)
                              #:λnode Geo-Anchor->Sticker #:λarrow Geo-Flow-Arrow->Edge)
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:operator [op #false] #:trusted-anchors [trusted-anchors #false]
           #:λnode [make-node default-flow-chart-node-constructor] #:λarrow [make-arrow default-flow-chart-arrow-constructor]
           self]
    (parameterize ([default-geo-edge-base-style make-geo-flow-edge-style])
      (define stickers : (Listof (GLayerof Geo)) (geo-flow-stick self make-node make-arrow trusted-anchors))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'flowchart:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (vector-immutable Width Height stickers))])))
          self))))
