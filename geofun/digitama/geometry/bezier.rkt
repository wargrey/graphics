#lang typed/racket/base

;;; https://pomax.github.io/bezierinfo/

(provide (all-defined-out))

(require racket/flonum)
(require math/number-theory)

(require plotfun/digitama/sample)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bezier-extremities : (->* (Float-Complex (Listof Float-Complex)) (Positive-Index) (Listof Float-Complex))
  (lambda [head tail [samples 200]]
    (define fbezier   (bezier-function #:derivative 0 head tail))
    (define fbezier~  (bezier-function #:derivative 1 head tail))
    (define fbezier~~ (bezier-function #:derivative 2 head tail))
    
    (if (and fbezier fbezier~ fbezier~~)
        (reverse (for/fold ([extremities : (Listof Float-Complex) null])
                           ([t (in-list (geo-linear-samples 0.0 1.0 samples))])
                   (let newton-raphson ([tn : Flonum (real->double-flonum t)])
                     (if (<= 0.0 tn 1.0)
                         (let ([f~tn (imag-part (fbezier~ tn))])
                           (cond [(< (abs f~tn) 1/10000000) (cons (fbezier tn) extremities)]
                                 [else (newton-raphson (- tn (/ f~tn (imag-part (fbezier~~ tn)))))]))
                         extremities))))
         null)))

(define bezier-derivative-weights : (-> Float-Complex (Listof Float-Complex) [#:order Byte] (Listof Float-Complex))
  (lambda [head tail #:order [order 1]]
    (define fln : Flonum (exact->inexact (length tail)))
    (define weights : (Listof Float-Complex)
      (for/list ([p1 (in-list (cons head tail))]
                 [p2 (in-list tail)])
        (* fln (- p2 p1))))
    
    (cond [(<= order 1) weights]
          [(null? weights) null]
          [else (bezier-derivative-weights #:order (sub1 order) (car weights) (cdr weights))])))

(define bezier-function : (-> Float-Complex (Listof Float-Complex) [#:derivative Byte] (Option (-> Flonum Float-Complex)))
  (lambda [head tail #:derivative [order 0]]
    (define points : (Listof Float-Complex)
      (cond [(= order 0) (cons head tail)]
            [else (bezier-derivative-weights #:order order head tail)]))
    (define n+1 : Index (length points))
    
    (and (> n+1 0)
         (let ([n : Index (- n+1 1)])
           (λ [[t : Flonum]]
             (let accum : Float-Complex ([i : Nonnegative-Fixnum 0]
                                         [pts : (Listof Float-Complex) points]
                                         [intermediate-point : Float-Complex 0.0+0.0i])
               (if (and (pair? pts) (<= i n))
                   (accum (+ i 1) (cdr pts)
                          (+ intermediate-point
                             (* (exact->inexact (binomial n i))
                                (flexpt (- 1.0 t) (exact->inexact (- n i)))
                                (flexpt t (exact->inexact i))
                                (car pts))))
                   intermediate-point)))))))

(define bezier-tangent-function : (-> Float-Complex (Listof Float-Complex) (Option (-> Flonum Float-Complex)))
  (lambda [head tail]
    (define make-derivative-point (bezier-function #:derivative 1 head tail))

    (and make-derivative-point
         (λ [[t : Flonum]]
           (let ([dp (make-derivative-point t)])
             (/ dp (magnitude dp)))))))

(define bezier-normal-function : (-> (-> Flonum Float-Complex) (Option (-> Flonum Float-Complex)))
  (lambda [make-tangent-point]
    (λ [[t : Flonum]]
      (let* ([tan-p (make-tangent-point t)])
        (make-rectangular (- (imag-part tan-p))
                          (real-part tan-p))))))

#;(bezier-extremities (list 60.0+105.0i  75.0+30.0i   215.0+115.0i 140.0+160.0i))
#;(bezier-extremities (list 120.0+160.0i 35.0+200.0i  220.0+260.0i 220.0+40.0i))
