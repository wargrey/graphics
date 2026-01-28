#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/digitama/self)
(require geofun/digitama/track/anchor)
(require geofun/digitama/richtext/self)

(require "style.rkt")
(require "dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-Level API
(define-type (Dia-Block-Describer Style Metadata)
  (U (Immutable-HashTable Geo-Anchor-Name Geo-Maybe-Rich-Text)
     (-> Geo-Anchor-Name String (Dia-Block-Style-Spec Style) Metadata Geo-Maybe-Rich-Text)))

(define-type (Dia-Block-Identifier Style Metadata) (-> Geo-Anchor-Name String Positive-Index (Option (Dia-Block-Info Style Metadata))))
(define-type (Dia-Block-Typesetter Style) (-> Symbol Geo-Rich-Text (Dia-Block-Style-Spec Style) (Option Geo)))

(define-type (Dia-Block-Builder Style Metadata)
  (-> Symbol (Option Geo) (Dia-Block-Style-Spec Style)
      Nonnegative-Flonum Nonnegative-Flonum
      (Option Flonum) Metadata
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         Dia:Block)))

(define-type (Dia-Block-Link-Root-Style Style)
  (-> (Dia-Block-Style Style)
      (Option (Dia-Block-Style Style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Style Metadata) dia-block-reference-size : (-> (Dia-Block-Factory Style Metadata) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (define s ((dia-block-factory-λbackstop-style self)))

    (values (dia-block-backstop-style-width s)
            (dia-block-backstop-style-height s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lower-Level API
(define-type (Dia-Block-Create Style Metadata)
  (-> Symbol (Option Geo) (Dia-Block-Style-Spec Style)
      Nonnegative-Flonum Nonnegative-Flonum
      (Option Flonum) Metadata
      Dia:Block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All diagram modules should provide their own factories, via `define-struct #:specialized`
(define-struct #:forall (Style Metadata) dia-block-factory : Dia-Block-Factory
  ([identifier : (Dia-Block-Identifier Style Metadata)]
   [typesetter : (Option (Dia-Block-Typesetter Style))]
   [builder : (Option (Dia-Block-Builder Style Metadata))]
   [fallback-builder : (Dia-Block-Builder Style Metadata)]
   [λroot-style : (Option (Dia-Block-Link-Root-Style Style))]
   [λbackstop-style : (-> Dia-Block-Backstop-Style)])
  #:transparent)
