#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/convert)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)
(require geofun/digitama/layer/sticker)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Cartesian-Layer (U 'grid 'axes 'visualizer 'data 'annotation 'sticker 'projection 'tick))

(define default-plot-cartesian-layer-order : (Parameterof (Listof Plot-Cartesian-Layer))
  (make-parameter '(grid axes visualizer projection annotation tick)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-cartesian-layers : (-> (Immutable-HashTable Plot-Cartesian-Layer (Listof (GLayerof Geo))) (Listof Geo-Visualizer)
                                    (Option (Listof Plot-Cartesian-Layer))
                                    (Listof (GLayerof Geo)))
  (lambda [layers plots order]
    (apply append
           (for/list : (Listof (Listof (GLayerof Geo))) ([layer (in-list order)])
             (case/eq layer
               [(visualizer)
                (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)])
                  (if (geo:visualizer? self)
                      (geo-own-pin-layer 'lt (geo:visualizer-position self) self 0.0+0.0i)
                      (geo-sticker->layer (car self) (geo:visualizer-position self) #:default-anchor 'lt)))]
               [(data)
                (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)] #:when (geo:visualizer? self))
                  (geo-own-pin-layer 'lt (geo:visualizer-position self) self 0.0+0.0i))]
               [(sticker)
                (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)] #:unless (geo:visualizer? self))
                  (geo-sticker->layer (car self) (geo:visualizer-position self) #:default-anchor 'lt))]
               [else (hash-ref layers layer (inst list (GLayerof Geo)))])))))
