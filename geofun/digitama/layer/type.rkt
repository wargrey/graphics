#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Pin-Port (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))
(define-type Geo-Append-Align (U 'vl 'vc 'vr 'ht 'hc 'hb))

(define-type (GLayerof G) (Immutable-Vector G Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
(define-type (GLayer-Listof G) (Pairof (GLayerof G) (Listof (GLayerof G))))
(define-type (GLayer-Groupof G) (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum (GLayer-Listof G)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Geo-Config-Argof T) (U (Listof T) (∩ T (U Symbol Number))))

(define #:forall (T D) geo-config-expand : (case-> [(Geo-Config-Argof T) Natural T -> (Vectorof T)]
                                                   [(Geo-Config-Argof T) Natural D (-> T D) -> (Vectorof D)])
  (case-lambda
    [(config n defval)
     (if (list? config)
         (if (pair? config)
             (list->n:vector config n)
             (make-vector n defval))
         (make-vector n config))]
    [(config n defval ->v)
     (if (list? config)
         (if (pair? config)
             (list->n:vector* config n ->v)
             (make-vector n defval))
         (make-vector n (->v config)))]))

(define geo-port-merge : (-> Geo-Pin-Port Geo-Pin-Port Geo-Pin-Port)
  (lambda [prow pcol]
    (cond [(eq? prow pcol) prow]
          [(memq prow '(lt lc lb)) (case pcol [(ct rt) 'lt] [(cc rc) 'lc] [(cb rb) 'lb] [else pcol])]
          [(memq prow '(ct cc cb)) (case pcol [(lt rt) 'ct] [(lc rc) 'cc] [(lb rb) 'cb] [else pcol])]
          [(memq prow '(rt rc rb)) (case pcol [(lt ct) 'rt] [(lc cc) 'rc] [(lb cb) 'rb] [else pcol])]
          [else 'cc])))
