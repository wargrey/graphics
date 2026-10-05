#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/richtext/self)

(require "style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type ($Arrayof M) (U (Listof M) (Vectorof M)))

(define-type ($Matrixof M)
  (U (Listof (U (Listof M) (Vectorof M)))
     (Vectorof (Vectorof M))))

(struct mtx-idx
  ([row : Index]
   [col : Index]
   [ord : Index])
  #:type-name Mtx-Indices
  #:constructor-name unsafe-mtx-idx
  #:transparent)

(struct mtx-hdr
  ; treat 0 as #false
  ([row : Index]
   [col : Index]
   [anchor : Symbol])
  #:type-name Mtx-Hdr-Index
  #:constructor-name mtx-hdr
  #:transparent)

(define mtx-indices : (-> #:row Index #:col Index #:ordinal Index Mtx-Indices)
  (lambda [#:row row #:col col #:ordinal idx]
    (unsafe-mtx-idx row col idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Entry-Theme-Adjuster (Expr-Slot-Theme-Adjuster Any Mtx-Entry-Style Mtx-Indices))
(define-type Mtx-Mask-Theme-Adjuster (Expr-Slot-Theme-Adjuster Any Mtx-Mask-Style Mtx-Indices))
(define-type Mtx-Hole-Theme-Adjuster (Expr-Slot-Theme-Adjuster Any Mtx-Hole-Style Mtx-Indices))

(define-type Mtx-Row-Header-Theme-Adjuster (Expr-Slot-Theme-Adjuster Void Mtx-Row-Header-Style Mtx-Hdr-Index))
(define-type Mtx-Col-Header-Theme-Adjuster (Expr-Slot-Theme-Adjuster Void Mtx-Col-Header-Style Mtx-Hdr-Index))
(define-type Mtx-Corner-Theme-Adjuster (Expr-Slot-Theme-Adjuster Void Mtx-Corner-Style Mtx-Hdr-Index))

(define default-mtx-entry-theme-adjuster : (Parameterof (Option Mtx-Entry-Theme-Adjuster)) (make-parameter #false))
(define default-mtx-hole-theme-adjuster : (Parameterof (Option Mtx-Hole-Theme-Adjuster)) (make-parameter #false))
(define default-mtx-mask-theme-adjuster : (Parameterof (Option Mtx-Mask-Theme-Adjuster)) (make-parameter #false))

(define default-mtx-row-header-theme-adjuster : (Parameterof (Option Mtx-Row-Header-Theme-Adjuster)) (make-parameter #false))
(define default-mtx-col-header-theme-adjuster : (Parameterof (Option Mtx-Col-Header-Theme-Adjuster)) (make-parameter #false))
(define default-mtx-corner-theme-adjuster : (Parameterof (Option Mtx-Corner-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Slot-Create (Expr-Slot-Create Mtx-Slot-Style Mtx-Indices))
(define-type (Mtx-Entry->Slot M) (Expr-Datum->Slot M Mtx-Slot-Style Mtx-Indices))

(define-type Mtx-Header-Slot-Create (Expr-Slot-Create Mtx-Slot-Style Mtx-Hdr-Index))
(define-type Mtx-Header->Slot (Expr-Datum->Slot Void Mtx-Slot-Style Mtx-Hdr-Index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Mtx-Entry M) (-> M (Expr-Slot-Style-Spec Mtx-Slot-Style) Mtx-Indices Geo-Maybe-Rich-Text))
(define-type Mtx-Mask (-> Index Index Boolean))

(define-type Mtx-Static-Headers
  (U String
     (Listof Geo-Maybe-Rich-Text)
     (Immutable-Vectorof Geo-Maybe-Rich-Text)))

(define-type Mtx-Headers (U (-> Index Index Geo-Maybe-Rich-Text) Mtx-Static-Headers))
(define-type Mtx-Spec-Headers (U (-> Index Geo-Maybe-Rich-Text) Mtx-Static-Headers))
