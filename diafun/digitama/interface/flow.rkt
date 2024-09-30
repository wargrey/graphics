#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/dc/path)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/label.rkt")
(require "../edge/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaFlow-Block-Datum (List String Dia-Node-Style (Option Symbol)))
(define-type DiaFlow-Block-Identifier (-> Geo-Anchor-Name (Option DiaFlow-Block-Datum)))
(define-type DiaFlow-Arrow-Endpoint (Pairof Geo-Anchor-Name Geo))

(define-type DiaFlow-Anchor->Node-Label
  (-> Geo:Path Geo-Anchor-Name String Dia-Node-Style Float-Complex (Option Symbol)
      (Option Geo)))

(define-type DiaFlow-Anchor->Node-Shape
  (-> Geo:Path Geo-Anchor-Name (Option Geo) Dia-Node-Style Float-Complex (Option Symbol)
      (U Geo-Sticker-Datum Void False)))

(define-type DiaFlow-Arrow->Edge
  (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
      Geo-Path-Clean-Prints (Listof Dia-Edge-Label)
      (U Dia:Edge Dia:Labeled-Edge Void False)))

(define-type DiaFlow-Arrow->Edge-Label
  (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
      Float-Complex Float-Complex Dia-Edge-Label-Datum
      (U Dia-Edge-Label (Listof Dia-Edge-Label) Void False)))

(define-type DiaFlow-Free-Track->Edge
  (-> Geo:Path Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Geo-Path-Clean-Prints (Listof Dia-Edge-Label)
      (U Dia:Edge Dia:Labeled-Edge Void False)))

(define-type DiaFlow-Free-Track->Edge-Label
  (-> Geo:Path Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Float-Complex Float-Complex Dia-Edge-Label-Datum
      (U Dia-Edge-Label (Listof Dia-Edge-Label) Void False)))

