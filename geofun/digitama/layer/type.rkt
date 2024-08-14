#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Pin-Port (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))
(define-type Geo-Append-Align (U 'vl 'vc 'vr 'ht 'hc 'hb))

(define-type (GLayerof G) (Immutable-Vector G Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
(define-type (GLayer-Listof G) (Pairof (GLayerof G) (Listof (GLayerof G))))
(define-type (GLayer-Groupof G) (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum (GLayer-Listof G)))
