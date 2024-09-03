#lang typed/racket/base

(provide (all-defined-out))

(require "../style/node.rkt")

(require "../../convert.rkt")
(require "../../geometry/constants.rkt")

(require "../../../resize.rkt")
(require "../../../composite.rkt")
(require "../../../constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-process : (-> Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label style width height]
    (geo-cc-superimpose
     (geo-rectangle #:stroke (geo-node-select-stroke-paint style)
                    #:fill (geo-node-select-fill-paint style)
                    width height)
     label)))

(define geo-flow-block-decision : (-> Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label style width height]
    (geo-cc-superimpose
     (geo-rhombus #:stroke (geo-node-select-stroke-paint style)
                  #:fill (geo-node-select-fill-paint style)
                  width height)
     label)))

(define geo-flow-block-preparation : (-> Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label style width height]
    (geo-cc-superimpose
     (geo-hexagon-tile #:stroke (geo-node-select-stroke-paint style)
                       #:fill (geo-node-select-fill-paint style)
                       width height)
     label)))

(define geo-flow-block-terminal : (-> Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label style width height]
    (define r : Flonum (* height 0.5))
    (geo-cc-superimpose
     (geo-stadium #:stroke (geo-node-select-stroke-paint style)
                  #:fill (geo-node-select-fill-paint style)
                  (- width (* r 2.0)) r)
     label)))

(define geo-flow-block-dataIO : (-> Geo:Text Geo-Node-Style Nonnegative-Flonum Nonnegative-Flonum Geo)
  (lambda [label style width height]
    (geo-cc-superimpose
     (geo-parallelogram #:stroke (geo-node-select-stroke-paint style)
                        #:fill (geo-node-select-fill-paint style)
                        width height (/ pi 3.0))
     label)))
