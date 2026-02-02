#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Dia-Track-Annotator Dia-Free-Track-Annotator]))

(require geofun/digitama/self)

(require "../track/style.rkt")
(require "../track/interface.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-Level API
; For three-value results,
; Void means use engine's fallback
; False means use denies the object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Note-Builder Style Metadata)
  (-> Symbol (Option Geo) (Dia-Block-Style-Spec Style)
      Nonnegative-Flonum Nonnegative-Flonum
      Metadata

      ; Yes, the engine doesn't provide a fallback
      (Option Dia:Block:Note)))
