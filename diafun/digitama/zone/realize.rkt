#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require geofun/digitama/self)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require geofun/digitama/path/dc)
(require geofun/digitama/track/self)
(require geofun/digitama/track/anchor)

(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flex-zone-realize : (-> Geo:Track:Zone:Flex (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (Listof (GLayerof Geo-Path))
                                    (Option Nonnegative-Flonum) 
                                    (Option (GLayerof Geo:Group)))
  (lambda [self blockdb tracks opacity]
    #false))

(define dia-fixed-zone-realize : (-> Geo:Track:Zone:Flex (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (Listof (GLayerof Geo-Path))
                                     (Option Nonnegative-Flonum) 
                                     (Option (GLayerof Geo:Group)))
  (lambda [self blockdb tracks opacity]
    #false))
