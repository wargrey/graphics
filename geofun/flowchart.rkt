#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Node-Style-Refine* Geo-Node-Style-Refine)
(provide (all-from-out "digitama/diagram/style/flow.rkt"))
(provide default-flow-chart-block-constructor)

(require "digitama/composite.rkt")

(require "digitama/diagram/style/flow.rkt")
(require "digitama/diagram/style/node.rkt")

(require "digitama/diagram/flowchart.rkt")
(require "digitama/geometry/trail.rkt")
(require "digitama/layer/sticker.rkt")

(require "digitama/dc/composite.rkt")
(require "digitama/dc/path.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-flow : (->* (Geo:Path)
                             (Geo-Anchor->Sticker #:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option Geo-Trusted-Anchors))
                             (U Geo:Group Geo:Path))
  (lambda [#:trusted-anchors [trusted-anchors #false] #:id [id #false] #:operator [op #false]
           self [make-block default-flow-chart-block-constructor]]
    (geo:path-stick self make-block trusted-anchors #false
                    (or id (gensym 'flowchart:)) op
                    (geo-path-sticker-offset self))))
