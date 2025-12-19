#lang typed/racket/base

(provide (all-defined-out))

(require "style.rkt")

(require geofun/constructor)
(require geofun/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-polygon-shape : (-> (Option Symbol) Dia-Block-Style (Listof Float-Complex) Geo)
  (lambda [node-key style vertices]
    (geo-polygon #:id (and node-key (dia-block-shape-id node-key))
                 #:stroke (dia-block-select-stroke-paint style)
                 #:fill (dia-block-select-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))
