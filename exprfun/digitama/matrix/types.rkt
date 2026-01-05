#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/richtext/self)

(require "../slot/style.rkt")
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
(define-type Mtx-Slot-Create (Expr-Slot-Create Mtx-Indices))
(define-type (Mtx-Entry->Slot M) (Expr-Datum->Slot M Mtx-Indices))
(define-type (Mtx-Style-Make S) (Expr-Slot-Style-Make Any S Mtx-Indices))

(define-type Mtx-Header-Slot-Create (Expr-Slot-Create Mtx-Hdr-Index))
(define-type Mtx-Header->Slot (Expr-Datum->Slot Void Mtx-Hdr-Index))
(define-type (Mtx-Header-Style-Make S) (Expr-Slot-Style-Make Void S Mtx-Hdr-Index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Mtx-Entry M) (-> M Expr-Slot-Style-Layers Mtx-Indices Geo-Maybe-Rich-Text))
(define-type Mtx-Mask (-> Index Index Boolean))

(define-type Mtx-Static-Headers
  (U String
     (Listof Geo-Maybe-Rich-Text)
     (Immutable-Vectorof Geo-Maybe-Rich-Text)))

(define-type Mtx-Headers (U (-> Index Index Geo-Maybe-Rich-Text) Mtx-Static-Headers))
(define-type Mtx-Spec-Headers (U (-> Index Geo-Maybe-Rich-Text) Mtx-Static-Headers))
