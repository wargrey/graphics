#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/digitama/geometry/footprint)
(require geofun/digitama/track/self)

(require geofun/digitama/path/label)
(require geofun/digitama/dc/path)

(require "style.rkt")
(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-Level API
; For three-value results,
; Void means use engine's fallback
; False means use denies the object
(define-type (Dia-Track-Identifier S)
  (-> Dia:Block Dia:Block (Listof Geo-Path-Label-Datum) (Listof Geo-Track-Info-Datum) (Option (Dia-Track-Style S))))

(define-type (Dia-Dangling-Track-Identifier S)
  (case-> [Dia:Block (Listof Geo-Path-Label-Datum) (Listof Geo-Track-Info-Datum) -> (Option (Dia-Track-Style S))]
          [False (Listof Geo-Path-Label-Datum) (Listof Geo-Track-Info-Datum) Dia:Block -> (Option (Dia-Track-Style S))]))

(define-type (Dia-Track-Builder S)
  (-> (Option Dia:Block) (Option Dia:Block) Geo-Path-Clean-Prints* (Dia-Track-Style-Spec S)
      (U Geo-Path Void False)))

(define-type (Dia-Track-Annotator S)
  (-> Index Geo-Path-Label-Datum Nonnegative-Flonum (Dia-Track-Style-Spec S)
      (U Geo:Path:Label (Listof Geo:Path:Label) Void False)))

(define-type (Dia-Track-Link-Root-Style S)
  (-> (Dia-Track-Style S)
      (Option (Dia-Track-Style S))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All diagram modules should provide their own factories, via `define-struct #:specialized`
(define-struct #:forall (S) dia-track-factory : Dia-Track-Factory
  ([identifier : (Dia-Track-Identifier S)]
   [dangling-identifier : (Option (Dia-Dangling-Track-Identifier S))]
   [annotator : (Option (Dia-Track-Annotator S))]
   [builder : (Option (Dia-Track-Builder S))]
   [λroot-style : (Option (Dia-Track-Link-Root-Style S))]
   [λbackstop-style : (-> Dia-Track-Backstop-Style)])
  #:transparent)
