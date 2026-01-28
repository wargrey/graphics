#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Dia-Track-Annotator Dia-Free-Track-Annotator]))

(require geofun/digitama/self)
(require geofun/digitama/track/self)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/dc/path)
(require geofun/digitama/path/label)

(require "../track/style.rkt")
(require "../track/interface.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-Level API
; For three-value results,
; Void means use engine's fallback
; False means use denies the object
(define-type (Dia-Free-Track-Adjuster S)
  (-> (Dia-Track-Style S)
      Dia-Track-Endpoint Dia-Track-Endpoint Geo-Path-Clean-Prints*
      (Listof Geo-Path-Label-Datum) (Listof Geo-Track-Info-Datum)
      (U (Dia-Track-Style S) False Void)))

(define-type (Dia-Free-Track-Builder S)
  (-> Dia-Track-Endpoint Dia-Track-Endpoint Geo-Path-Clean-Prints* (Dia-Track-Style-Spec S)
      (U Geo-Path Void False)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Note-Builder Style Metadata)
  (-> Symbol (Option Geo) (Dia-Block-Style-Spec Style)
      Nonnegative-Flonum Nonnegative-Flonum
      Metadata

      ; Yes, the engine doesn't provide a fallback
      (Option Dia:Block:Note)))
