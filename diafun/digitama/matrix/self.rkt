#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)

(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require "../block/style.rkt")
(require "../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Matrix-Urgent-Datum (Pairof Index Index))
(define-type (Dia-Matrix-Block-Style-Make S) (Dia-Block-Style-Make* S Dia-Matrix-Urgent-Datum))
(define-type Dia-Matrix-Id->Block (Dia-Anchor->Block* Dia-Matrix-Urgent-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Matrixof M)
  (U (Listof (U (Listof (U M Void)) (Vectorof (U M Void))))
     (Vectorof (U (Listof (U M Void)) (Vectorof (U M Void))))))

(define-type Dia-Matrix-Optional-Entry (U DC-Markup-Text Geo False))
(define-type (Dia-Matrix-Entry M) (-> Index Index M Dia-Matrix-Optional-Entry))
(define-type Dia-Matrix-Mask (-> Index Index Boolean))

(define-type Dia-Matrix-Headers
  (U (-> Index Index Dia-Matrix-Optional-Entry)
     (Listof Dia-Matrix-Optional-Entry)
     (Immutable-Vectorof Dia-Matrix-Optional-Entry)))

(define-type Dia-Matrix-Sub-Headers (U String Dia-Matrix-Headers))
