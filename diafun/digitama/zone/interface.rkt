#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/richtext/self)

(require "dc.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Zone-Metadata (Pairof (Option Keyword) (Listof Any))) ; the head is interpreted as the `stereotype`
(define-type (Dia-Zone-Typesetter S) (-> Symbol (Option Symbol) Geo-Rich-Text (Dia-Zone-Style-Spec S) (Option Geo)))

(define-type Dia-Zone-Describer
  (U (Immutable-HashTable Symbol Geo-Maybe-Rich-Text)
     (-> Symbol (Option Symbol) String (Dia-Zone-Style-Spec Dia-Zone-Style) Dia-Zone-Metadata
         Geo-Maybe-Rich-Text)))

(define-type (Dia-Zone-Identifier S)
  (-> Symbol (Option Symbol) Dia-Zone-Metadata
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         (#%Dia-Zone-Style S))))

(define-type (Dia-Zone-Builder S)
  (-> Symbol (Option Symbol) (Option Geo) (Dia-Zone-Style-Spec S)
      Nonnegative-Flonum Nonnegative-Flonum Dia-Zone-Metadata Geo-Insets-Mask
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         (Pairof Dia:Zone Float-Complex))))
