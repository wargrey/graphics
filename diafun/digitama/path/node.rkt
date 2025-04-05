#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "interface.rkt")

(require geofun/constructor)
(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) diauc-node-style-construct : (->* (Geo-Anchor-Name String
                                                                        (Option (Dia-Node-Style-Make* (∩ S Dia-Node-Style) (Option Symbol)))
                                                                        (-> (∩ S Dia-Node-Style)))
                                                       ((Option Symbol))
                                                       Dia-Path-Block-Datum)
  (lambda [anchor text mk-style mk-fallback-style [hint #false]]
    (list text (dia-node-style-construct anchor mk-style mk-fallback-style hint) hint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-polygon-shape : (-> (Option Symbol) Dia-Node-Style (Listof Float-Complex) Geo)
  (lambda [node-key style vertices]
    (geo-polygon #:id (and node-key (dia-node-shape-id node-key))
                 #:stroke (dia-node-select-stroke-paint style)
                 #:fill (dia-node-select-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))
