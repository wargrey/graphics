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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-group-boundary : (-> (Listof (GLayerof G)) Nonnegative-Flonum Nonnegative-Flonum
                                              (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [siblings Width Height]
    (let check-boundary ([lx : Flonum 0.0]
                         [ty : Flonum 0.0]
                         [rx : Nonnegative-Flonum Width]
                         [by : Nonnegative-Flonum Height]
                         [siblings : (Listof (GLayerof G)) siblings])
      (if (pair? siblings)
          (let ([self (car siblings)])
            (check-boundary (min lx (vector-ref self 1)) (min ty (vector-ref self 2))
                            (max rx (+ (vector-ref self 1) (vector-ref self 3))) (max by (+ (vector-ref self 2) (vector-ref self 4)))
                            (cdr siblings)))
          (values lx ty rx by)))))

(define #:forall (G) geo-layer-translate : (-> (GLayerof G) Flonum Flonum (GLayerof G))
  (lambda [self xoff yoff]
    (vector-immutable (vector-ref self 0)
                      (+ xoff (vector-ref self 1))
                      (+ yoff (vector-ref self 2))
                      (vector-ref self 3) (vector-ref self 4))))
