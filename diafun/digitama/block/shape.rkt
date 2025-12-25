#lang typed/racket/base

(provide (all-defined-out))

(require "style.rkt")

(require geofun/constructor)
(require geofun/digitama/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-polygon-shape : (-> (Option Symbol) Dia-Block-Style-Layers (Listof Float-Complex) Geo)
  (lambda [node-key style vertices]
    (geo-polygon #:id (and node-key (dia-block-shape-id node-key))
                 #:stroke (dia-block-resolve-stroke-paint style)
                 #:fill (dia-block-resolve-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))
