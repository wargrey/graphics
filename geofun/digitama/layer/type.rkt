#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)
(require racket/case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Pin-Anchor (U 'lt 'lc 'lb 'l? 'ct 'cc 'cb 'c? 'rt 'rc 'rb 'r? '?t '?c '?b '??))
(define-type Geo-Append-Align (U 'vl 'vc 'vr 'v? 'ht 'hc 'hb 'h?))

(define-type (GLayer-Listof G) (Pairof (GLayerof G) (Listof (GLayerof G))))

(struct (G) glayer
  ([master : G]
   [x : Flonum]
   [y : Flonum]
   [width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum])
  #:type-name GLayerof
  #:transparent)

(struct (G) glayer-group
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [layers : (GLayer-Listof G)])
  #:type-name GLayer-Groupof
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-layer-position-values : (-> (GLayerof G) (Values Flonum Flonum))
  (lambda [self]
    (values (glayer-x self) (glayer-y self))))

(define #:forall (G) geo-layer-position : (-> (GLayerof G) Float-Complex)
  (lambda [self]
    (define-values (x y) (geo-layer-position-values self))
    (make-rectangular x y)))

(define #:forall (G) geo-layer-center-position-values : (-> (GLayerof G) (Values Flonum Flonum))
  (lambda [self]
    (values (+ (glayer-x self) (* (glayer-width self) 0.5))
            (+ (glayer-y self) (* (glayer-height self) 0.5)))))

(define #:forall (G) geo-layer-center-position : (-> (GLayerof G) Float-Complex)
  (lambda [self]
    (define-values (x y) (geo-layer-center-position-values self))
    (make-rectangular x y)))

(define #:forall (G) geo-layer-size : (case-> [(GLayerof G) -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                              [(GLayerof G) Nonnegative-Flonum -> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                              [(GLayerof G) Nonnegative-Flonum Nonnegative-Flonum -> (Values Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(self) (values (glayer-width self) (glayer-height self))]
    [(self s) (values (* (glayer-width self) s) (* (glayer-height self) s))]
    [(self sw sh) (values (* (glayer-width self) sw) (* (glayer-height self) sh))]))

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

(define geo-anchor-merge : (-> Geo-Pin-Anchor Geo-Pin-Anchor Geo-Pin-Anchor)
  (lambda [prow pcol]
    (cond [(eq? prow pcol) prow]
          [(memq pcol '(lt lc lb l?)) (case/eq prow [(lt ct rt ?t) 'lt] [(lc cc rc ?c) 'lc] [(lb cb rb ?b) 'lb] [else 'l?])]
          [(memq pcol '(ct cc cb c?)) (case/eq prow [(lt ct rt ?t) 'ct] [(lc cc rc ?c) 'cc] [(lb cb rb ?b) 'cb] [else 'c?])]
          [(memq pcol '(rt rc rb r?)) (case/eq prow [(lt ct rt ?t) 'rt] [(lc cc rc ?c) 'rc] [(lb cb rb ?b) 'rb] [else 'r?])]
          [(memq pcol '(?t ?c ?b ??)) (case/eq prow [(lt ct rt ?t) '?t] [(lc cc rc ?c) '?c] [(lb cb rb ?b) '?b] [else '??])]
          [else 'cc])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-group-boundary : (->* ((Listof (GLayerof G)) Nonnegative-Flonum Nonnegative-Flonum)
                                               (Flonum Flonum)
                                               (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [siblings Width Height [x0 0.0] [y0 0.0]]
    (let check-boundary ([lx : Flonum x0]
                         [ty : Flonum y0]
                         [rx : Nonnegative-Flonum Width]
                         [by : Nonnegative-Flonum Height]
                         [siblings : (Listof (GLayerof G)) siblings])
      (if (pair? siblings)
          (let* ([self (car siblings)]
                 [x (glayer-x self)]
                 [y (glayer-y self)])
            (check-boundary (min lx x) (min ty y)
                            (max rx (+ x (glayer-width self)))
                            (max by (+ y (glayer-height self)))
                            (cdr siblings)))
          (values lx ty rx by)))))

(define #:forall (G) geo-layer-translate : (case-> [(GLayerof G) Flonum Flonum -> (GLayerof G)]
                                                   [(GLayerof G) Float-Complex -> (GLayerof G)])
  (case-lambda
    [(self xoff yoff)
     (glayer (glayer-master self)
             (+ xoff (glayer-x self)) (+ yoff (glayer-y self))
             (glayer-width self) (glayer-height self))]
    [(self offset)
     (geo-layer-translate self (real-part offset) (imag-part offset))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-pin-anchor? : (-> Any Boolean : Geo-Pin-Anchor)
  (lambda [v]
    (case/eq v
             [(lt lc lb) #true]
             [(ct cc cb) #true]
             [(rt rc rb) #true]
             [else (case/eq v
                            [(l? c? r?) #true]
                            [(?t ?c ?b) #true]
                            [(??) #true]
                            [else #false])])))
    
(define geo-append-align? : (-> Any Boolean : Geo-Append-Align)
  (lambda [v]
    (case/eq v
             [(vl vc vr v?) #true]
             [(ht hc hb h?) #true]
             [else #false])))
