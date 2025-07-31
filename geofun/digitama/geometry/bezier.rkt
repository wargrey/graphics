#lang typed/racket/base

;;; https://pomax.github.io/bezierinfo/

(provide (all-defined-out))

(require (only-in racket/list last remove-duplicates))
(require racket/flonum)

(require digimon/complex)
(require math/number-theory)
(require plotfun/digitama/sample)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bezier-quadratic-extremities : (-> Float-Complex Float-Complex Float-Complex (Listof Float-Complex))
  ; B'(t) = (b - a)t + a => t = a/(a - b)
  (lambda [head ctrl end]
    (define fbezier  (bezier-function #:derivative 0 head (list ctrl end)))
    (define a (- ctrl head))
    (define b (- end ctrl))

    (cond [(or (= a b) (not fbezier)) null]
          [else (let* ([v (- a b)]
                       [tx (/ (real-part a) (real-part v))]
                       [ty (/ (imag-part a) (imag-part v))])
                  (cond [(< 0.0 tx 1.0) (list (fbezier tx))]
                        [(< 0.0 ty 1.0) (list (fbezier ty))]
                        [else null]))])))

(define bezier-cubic-extremities : (-> Float-Complex Float-Complex Float-Complex Float-Complex (Listof Float-Complex))
  ; B'(t) = at^2 + bt + c
  ; a = 3(-p1 + 3p2 - 3p3 + p4)
  ; b = 6(p1 - 2p2 + p3)
  ; c = 3(p2 - p1)
  (lambda [head ctrl1 ctrl2 end]
    (define fbezier  (bezier-function #:derivative 0 head (list ctrl1 ctrl2 end)))
    (define a (* 3.0 (+ (* 3.0 (- ctrl1 ctrl2)) (- end head))))
    (define b (* 6.0 (+ head (* -2.0 ctrl1) ctrl2)))
    (define c (* 3.0 (- ctrl1 head)))
    (define txs (quadratic-solutions (real-part a) (real-part b) (real-part c)))
    (define tys (quadratic-solutions (imag-part a) (imag-part b) (imag-part c)))

    (cond [(not fbezier) null]
          [else (for/list ([t (in-list (append txs tys))]
                           #:when (< 0.0 t 1.0))
                  (fbezier (real->double-flonum t)))])))

(define bezier-nth-extremities : (-> Float-Complex (Listof Float-Complex) Index (Listof Float-Complex))
  (lambda [head tail samples]
    (define fbezier   (bezier-function #:derivative 0 head tail))
    (define f~bezier  (bezier-function #:derivative 1 head tail))
    (define f~~bezier (bezier-function #:derivative 2 head tail))

    (if (and fbezier f~bezier f~~bezier)
        (let ([ts (map real->double-flonum (geo-linear-samples 0.0 1.0 (max 3 samples)))])
          (define (solve [part : (-> Float-Complex Flonum)]) : (Listof Flonum)
            (for/fold ([tseqs : (Listof Flonum) null])
                      ([tseq (in-list ts)])
              (let newton-raphson ([t : Flonum tseq]
                                   [trials : Index 0])
                (if (and (<= 0.0 t 1.0) (< trials 16))
                    (let* ([f~t (part (f~bezier t))])
                      (if (< (abs f~t) 1e-11)
                          (if (or (null? tseqs) (not (= t (car tseqs))))
                              (cons t tseqs)
                              tseqs)
                          (let* ([f~~t (part (f~~bezier t))]
                                 [dt (/ f~t f~~t)])
                            (cond [(< (abs dt) 1e-9) tseqs]
                                  [else (newton-raphson (- t dt)
                                                        (+ trials 1))]))))
                    tseqs))))
          (reverse (for/list : (Listof Float-Complex) ([t (in-list (remove-duplicates (append (solve real-part) (solve imag-part))))]
                                                       #:when (< 0.0 t 1.0))
                     (fbezier t))))
        null)))

(define bezier-extremities : (-> Float-Complex (Listof Float-Complex) Index (Listof Float-Complex))
  (lambda [head tail samples]
    (cond [(null? tail) null]
          [(null? (cdr tail)) null]
          [(null? (cddr tail))  (bezier-quadratic-extremities head (car tail) (cadr tail))]
          [(null? (cdddr tail)) (bezier-cubic-extremities head (car tail) (cadr tail) (caddr tail))]
          [else (bezier-nth-extremities head tail samples)])))

(define bezier-bounding-box : (-> Float-Complex (Listof Float-Complex) Index (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [head tail samples]
    (define extremities (bezier-extremities head tail samples))
    (define-values (lx ty rx by) (flc-interval (list* head (last tail) (bezier-extremities head tail samples))))

    (values lx ty
            (max 0.0 (- rx lx))
            (max 0.0 (- by ty)))))

(define bezier-length : (->* (Float-Complex (Listof Float-Complex) Index)
                             (#:t0 Nonnegative-Exact-Rational #:tn Nonnegative-Exact-Rational)
                             Nonnegative-Flonum)
  (lambda [head tail samples #:t0 [tmin 0] #:tn [tmax 1]]
    (define fbezier (bezier-function head tail #:derivative 0))
    (define t0 : Nonnegative-Exact-Rational (min tmin 1))
    (define tn : Nonnegative-Exact-Rational (min tmax 1))
    (define op : (-> Exact-Rational Exact-Rational Boolean) (if (<= tmin tmax) <= >=))
    (define delta : Exact-Rational (/ (- tn t0) (if (> samples 0) samples 500)))

    (if (and fbezier (not (zero? delta)))
        (let approximate ([t : Exact-Rational (+ t0 delta)]
                          [prev : Float-Complex (fbezier (exact->inexact t0))]
                          [acc-len : Nonnegative-Flonum 0.0])
          (if (op t tn)
              (let ([here (fbezier (exact->inexact t))])
                (approximate (+ t delta) here
                             (+ acc-len (magnitude (- here prev)))))
              acc-len))
        #;'#:deadcode 0.0)))

(define bezier-reparameterize-by-length : (->* (Float-Complex (Listof Float-Complex) Nonnegative-Flonum Index)
                                               (#:t0 Nonnegative-Exact-Rational #:tn Nonnegative-Exact-Rational)
                                               Nonnegative-Exact-Rational)
  (lambda [head tail size samples #:t0 [tmin 0] #:tn [tmax 1]]
    (define fbezier (bezier-function head tail #:derivative 0))
    (define t0 : Nonnegative-Exact-Rational (min tmin 1))
    (define tn : Nonnegative-Exact-Rational (min tmax 1))
    (define op : (-> Exact-Rational Exact-Rational Boolean) (if (<= tmin tmax) <= >=))
    (define delta : Exact-Rational (/ (- tn t0) (if (> samples 0) samples 500)))
    
    (if (and fbezier (not (zero? delta)))
        (let peek ([t : Exact-Rational (+ t0 delta)]
                   [prev : Float-Complex (fbezier (exact->inexact t0))]
                   [acc-len : Nonnegative-Flonum 0.0])
          (if (op t tn)
              (let* ([here (fbezier (exact->inexact t))]
                     [dlen (magnitude (- here prev))]
                     [len++ (+ acc-len dlen)])
                (cond [(< len++ size) (peek (+ t delta) here len++)]
                      [(= len++ size) (abs t)]
                      [else (let ([diff (/ (- len++ size) dlen)])
                              (abs (- t (* delta (/ (inexact->exact (numerator diff))
                                                    (inexact->exact (denominator diff)))))))]))
              tn))
        t0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
