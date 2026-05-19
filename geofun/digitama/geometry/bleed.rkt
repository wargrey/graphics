#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; used in kernel only
(struct geo-bleed
  ([top : Nonnegative-Flonum]
   [right : Nonnegative-Flonum]
   [bottom : Nonnegative-Flonum]
   [left : Nonnegative-Flonum])
  #:type-name Geo-Bleed
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bleed-scale : (-> Geo-Bleed Flonum Flonum Geo-Bleed)
  (lambda [self sx0 sy0]
    (cond [(eq? self geo-zero-bleeds) geo-zero-bleeds]
          [else (let-values ([(sx sy) (values (abs sx0) (abs sy0))])
                  (geo-bleed (* (geo-bleed-top self)    sy)
                             (* (geo-bleed-right self)  sx)
                             (* (geo-bleed-bottom self) sy)
                             (* (geo-bleed-left self)   sx)))])))

(define geo-bleed-expand : (-> Geo-Bleed Nonnegative-Flonum Nonnegative-Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self width height]
    (define toff (geo-bleed-top self))
    (define loff (geo-bleed-left self))
    
    (values loff toff
            (+ width loff (geo-bleed-right self))
            (+ height toff (geo-bleed-bottom self)))))

(define geo-bleed-union : (-> Geo-Bleed Geo-Bleed Geo-Bleed)
  (lambda [lpd rpd]
    (cond [(eq? rpd lpd) rpd]
          [(equal? rpd lpd) rpd]
          [else (let-values ([(lt lr lb ll) (values (geo-bleed-top lpd) (geo-bleed-right lpd) (geo-bleed-bottom lpd) (geo-bleed-left lpd))]
                             [(rt rr rb rl) (values (geo-bleed-top rpd) (geo-bleed-right rpd) (geo-bleed-bottom rpd) (geo-bleed-left rpd))])
                  (cond [(and (>= lt rt) (>= lr rr) (>= lb rb) (>= ll rl)) lpd]
                        [(and (<= lt rt) (<= lr rr) (<= lb rb) (<= ll rl)) rpd]
                        [else (geo-bleed (max lt rt) (max lr rr) (max lb rb) (max ll rl))]))])))

(define geo-bleed-union* : (case-> [(Listof (Option Geo-Bleed)) -> (Option Geo-Bleed)]
                                   [Geo-Bleed (Listof Geo-Bleed) -> Geo-Bleed])
  (case-lambda
    [(selves)
     (for/fold ([pad : (Option Geo-Bleed) #false])
               ([self (in-list selves)])
       (cond [(not pad) self]
             [(not self) pad]
             [else (geo-bleed-union pad self)]))]
    [(self extras)
     (for/fold ([pad : Geo-Bleed self])
               ([extra (in-list extras)])
       (geo-bleed-union pad extra))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-zero-bleeds : Geo-Bleed (geo-bleed 0.0 0.0 0.0 0.0))
