#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/diagram/configure/flowchart.rkt"))
(provide default-flow-chart-block-constructor)

(require "digitama/composite.rkt")

(require "digitama/diagram/configure/flowchart.rkt")

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
    (geo:path-stick self make-block geo-path->flow-chart trusted-anchors #false
                    (or id (gensym 'flowchart:)) op)))
