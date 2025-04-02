#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require "../node/style.rkt")
(require "../node/dc.rkt")
(require "../edge/style.rkt")
(require "../edge/label.rkt")
(require "../edge/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Path-Block-Datum (List String Dia-Node-Style (Option Symbol)))

(define-type Dia-Path-Block-Identifier (-> Geo-Anchor-Name (Option Dia-Path-Block-Datum)))
(define-type Dia-Path-Arrow-Identifier (-> Dia:Node (Option Dia:Node) (Listof Dia-Edge-Label-Datum) (Option Dia-Edge-Style)))
(define-type Dia-Path-Block-Create (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol) Dia:Node))

(define-type Dia-Path-Id->Node-Label
  (-> Symbol String Dia-Node-Style (Option Symbol)
      (Option Geo)))

(define-type Dia-Path-Id->Node-Shape
  (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol)
      (U Void  ; use default
         False ; invisible node
         Dia:Node)))

(define-type Dia-Path-Arrow->Edge
  (-> Dia:Node (Option Dia:Node) Dia-Edge-Style
      Geo-Path-Clean-Prints+ (Listof Dia-Edge-Label)
      (U Dia:Edge Dia:Labeled-Edge Void False)))

(define-type Dia-Path-Arrow->Edge-Label
  (-> Dia:Node (Option Dia:Node) Dia-Edge-Style
      Float-Complex Float-Complex Dia-Edge-Label-Datum
      (U Dia-Edge-Label (Listof Dia-Edge-Label) Void False)))

(define-type Dia-Path-Free-Track->Edge
  (-> Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Geo-Path-Clean-Prints+ (Listof Dia-Edge-Label)
      (U Dia:Edge Dia:Labeled-Edge Void False)))

(define-type Dia-Path-Free-Track->Edge-Label
  (-> Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Float-Complex Float-Complex Dia-Edge-Label-Datum
      (U Dia-Edge-Label (Listof Dia-Edge-Label) Void False)))

