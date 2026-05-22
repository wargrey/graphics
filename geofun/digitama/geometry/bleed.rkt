#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; used in kernel only, for stroke width
(struct geo-bleed
  ([top : Nonnegative-Flonum]    [top-scalable? : Boolean]
   [right : Nonnegative-Flonum]  [right-scalable? : Boolean]
   [bottom : Nonnegative-Flonum] [bottom-scalable? : Boolean]
   [left : Nonnegative-Flonum]   [left-scalable? : Boolean])
  #:type-name Geo-Bleed
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bleed-scale : (-> Geo-Bleed Flonum Flonum Geo-Bleed)
  (lambda [self sx0 sy0]
    (cond [(eq? self geo-zero-bleeds) geo-zero-bleeds]
          [else (let*-values ([(sx sy) (values (abs sx0) (abs sy0))]
                              [(t t?) (values (geo-bleed-top self) (geo-bleed-top-scalable? self))]
                              [(r r?) (values (geo-bleed-right self) (geo-bleed-right-scalable? self))]
                              [(b b?) (values (geo-bleed-bottom self) (geo-bleed-bottom-scalable? self))]
                              [(l l?) (values (geo-bleed-left self) (geo-bleed-left-scalable? self))]
                              [(t b) (values (if t? (* t sy) t) (if b? (* b sy) b))]
                              [(l r) (values (if l? (* l sx) l) (if r? (* r sx) r))]
                              [(l l? r r?) (if (>= sx0 0) (values l l? r r?) (values r r? l l?))]
                              [(t t? b b?) (if (>= sy0 0) (values t t? b b?) (values b b? t t?))])
                  (geo-bleed t t? r r? b b? l l?))])))

(define geo-bleed-expand : (-> Geo-Bleed Nonnegative-Flonum Nonnegative-Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self width height]
    (define toff (geo-bleed-top self))
    (define loff (geo-bleed-left self))
    
    (values loff toff
            (+ width loff (geo-bleed-right self))
            (+ height toff (geo-bleed-bottom self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-zero-bleeds : Geo-Bleed (geo-bleed 0.0 #false 0.0 #false 0.0 #false 0.0 #false))
