#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)

(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require "../block/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Arrayof M) (U (Listof M) (Vectorof M)))

(define-type (Dia-Matrixof M)
  (U (Listof (U (Listof M) (Vectorof M)))
     (Vectorof (Vectorof M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Indices (List Index Index Index))
(define-type Mtx-Block-Create (Dia-Block-Create* Mtx-Indices))
(define-type Mtx-Header->Block (Dia-Anchor->Block* Symbol Mtx-Indices))
(define-type (Mtx-Entry->Block M) (Dia-Anchor->Block* (Pairof Symbol M) Mtx-Indices))
(define-type (Mtx-Style-Make S) (Dia-Block-Style-Make* Any S Mtx-Indices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Maybe-Desc (U DC-Markup-Text Geo Void False))
(define-type (Mtx-Entry M) (-> M Dia-Block-Style Mtx-Indices Mtx-Maybe-Desc))
(define-type Mtx-Mask (-> Index Index Boolean))

(define-type Mtx-Static-Headers
  (U String
     (Listof Mtx-Maybe-Desc)
     (Immutable-Vectorof Mtx-Maybe-Desc)))

(define-type Mtx-Headers (U (-> Index Index Mtx-Maybe-Desc) Mtx-Static-Headers))
(define-type Mtx-Spec-Headers (U (-> Index Mtx-Maybe-Desc) Mtx-Static-Headers))
