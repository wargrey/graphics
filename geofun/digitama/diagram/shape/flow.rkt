#lang typed/racket/base

(provide (all-defined-out))

(require "../style/node.rkt")

(require "../../convert.rkt")
(require "../../geometry/anchor.rkt")
(require "../../geometry/constants.rkt")

(require "../../../resize.rkt")
(require "../../../composite.rkt")
(require "../../../constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-process : (-> Symbol Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-rectangle #:id (geo-flow-node-id node-key)
                                       #:stroke (geo-node-select-stroke-paint style)
                                       #:fill (geo-node-select-fill-paint style)
                                       width height)
                        label)))

(define geo-flow-block-decision : (-> Symbol Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-rhombus #:id (geo-flow-node-id node-key)
                                     #:stroke (geo-node-select-stroke-paint style)
                                     #:fill (geo-node-select-fill-paint style)
                                     width height)
                        label)))

(define geo-flow-block-preparation : (-> Symbol Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-hexagon-tile #:id (geo-flow-node-id node-key)
                                          #:stroke (geo-node-select-stroke-paint style)
                                          #:fill (geo-node-select-fill-paint style)
                                          width height)
                        label)))

(define geo-flow-block-terminal : (-> Symbol Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (define r : Flonum (* height 0.5))
    (geo-cc-superimpose #:id node-key
                        (geo-stadium #:id (geo-flow-node-id node-key)
                                     #:stroke (geo-node-select-stroke-paint style)
                                     #:fill (geo-node-select-fill-paint style)
                                     (- width (* r 2.0)) r)
                        label)))

(define geo-flow-block-dataIO : (-> Symbol Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [node-key label style width height]
    (geo-cc-superimpose #:id node-key
                        (geo-parallelogram #:id (geo-flow-node-id node-key)
                                           #:stroke (geo-node-select-stroke-paint style)
                                           #:fill (geo-node-select-fill-paint style)
                                           width height (/ pi 3.0))
                        label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-node-id : (-> Symbol Symbol)
  (lambda [node-key]
    (string->symbol (string-append "&" (geo-anchor->string node-key)))))
