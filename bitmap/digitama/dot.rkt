#lang typed/racket/base

(provide (all-defined-out))

(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~point2d : (-> Point2D Float-Complex)
  (lambda [dt]
    (define-values (x y) (point2d-values dt))
    (make-rectangular x y)))

(define ~point2ds : (->* ((Listof Point2D)) (Real Real) (Values (Listof Flonum) (Listof Flonum) Flonum Flonum Flonum Flonum))
  (lambda [raws [dx 0.0] [dy 0.0]]
    (define xoff : Flonum (real->double-flonum dx))
    (define yoff : Flonum (real->double-flonum dy))
    
    (let normalize ([dots : (Listof Point2D) raws]
                    [sx : (Listof Flonum) null]
                    [sy : (Listof Flonum) null]
                    [lx : Flonum +inf.0]
                    [ty : Flonum +inf.0]
                    [rx : Flonum -inf.0]
                    [by : Flonum -inf.0])
      (cond [(null? dots) (values (reverse sx) (reverse sy) lx ty rx by)]
            [else (let*-values ([(self rest) (values (car dots) (cdr dots))]
                                [(x0 y0) (point2d-values self)]
                                [(x y) (values (+ x0 xoff) (+ y0 yoff))])
                    (normalize rest
                               (cons x sx) (cons y sy)
                               (min lx x) (min ty y)
                               (max rx x) (max by y)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define point2d-values : (-> Point2D (Values Flonum Flonum))
  (lambda [dt]
    (cond [(real? dt) (values (real->double-flonum dt) 0.0)]
          [(list? dt) (values (real->double-flonum (car dt)) (real->double-flonum (cadr dt)))]
          [(pair? dt) (values (real->double-flonum (car dt)) (real->double-flonum (cdr dt)))]
          [else (values (real->double-flonum (real-part dt)) (real->double-flonum (imag-part dt)))])))
