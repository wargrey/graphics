#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/dc/path)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaFlow-Block-Identifier (-> Geo-Anchor-Name (Option (Pairof Symbol Dia-Node-Style))))

(define-type DiaFlow-Anchor->Node (-> Geo:Path Geo-Anchor-Name Float-Complex Symbol Dia-Node-Style (U Geo-Sticker-Datum Void False)))

(define-type DiaFlow-Arrow-Endpoint (Pairof Geo-Anchor-Name Geo))
(define-type DiaFlow-Arrow->Edge (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
                                     (List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                     (U Dia:Edge Void False)))

(define-type DiaFlow-Arrow-Info-Stickers (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
                                             (Pairof Float-Complex Flonum) (Pairof Float-Complex Flonum)
                                             (U Geo-Sticker-Datum Void False)))
