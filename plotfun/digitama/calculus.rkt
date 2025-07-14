#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [df/dx- df/dx_]
                     [d²f/dx² ddf/dxx]
                     [d²f/dx² d2f/dx2]))

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DF/DX (->* ((-> Real Complex) Real) (Positive-Real) (Option Real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE:
; In order to balance the roundoff and truncation errors,
; we choose these two ϵs for the 1st-order and 2nd-order derivatives, respectively.

; a smaller ϵ might cause unstable results for `y = kx + b`,
; yet still doesn't increase the precision for other functions.
(define the-ϵ/1st-order : Positive-Exact-Rational 1/10000000) #;|10√(machine ϵ)|

; a smaller ϵ for d²f/dx² would result in 0
(define the-ϵ/2nd-order : Positive-Exact-Rational 1/10000) #;|√√(machine ϵ)|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define df/dx : DF/DX
  (lambda [f x [ϵ the-ϵ/1st-order]]
    (define fx : Complex (f x))

    (let refine ([δx : Nonnegative-Real (* ϵ (+ 1 (abs x)))]
                 [slopes : (Listof Real) null])
      (if (and (or (null? slopes)
                   (null? (cdr slopes))
                   (>= (abs (- (car slopes) (cadr slopes))) ϵ))
               (< (length slopes) 10))

          (let ([δx/2 (* δx 1/2)]
                [fx+δ (f (+ x δx))]
                [fx-δ (f (- x δx))])
            (cond [(and (rational? fx+δ) (rational? fx-δ))
                   (refine δx/2 (cons (/ (- fx+δ fx-δ) (* δx 2)) slopes))]
                  [(and (rational? fx+δ) (rational? fx))
                   (refine δx/2 (cons (/ (- fx+δ fx) δx) slopes))]
                  [(and (rational? fx-δ) (rational? fx))
                   (refine δx/2 (cons (/ (- fx fx-δ) δx) slopes))]
                  [else #false]))

          (slope-guard slopes ϵ)))))

(define df/dx+ : DF/DX
  (lambda [f x [ϵ the-ϵ/1st-order]]
    (define fx : Complex (f x))

    (and (rational? fx)
         (let refine ([δx : Nonnegative-Real (* ϵ (+ 1 (abs x)))]
                      [slopes : (Listof Real) null])
           (if (and (or (null? slopes)
                        (null? (cdr slopes))
                        (>= (abs (- (car slopes) (cadr slopes))) ϵ))
                    (< (length slopes) 10))
               
               (let ([δx/2 (* δx 1/2)]
                     [fx+δ (f (+ x δx))])
                 (and (rational? fx+δ)
                      (refine δx/2 (cons (/ (- fx+δ fx) δx) slopes))))

               (slope-guard slopes ϵ))))))

(define df/dx- : DF/DX
  (lambda [f x [ϵ the-ϵ/1st-order]]
    (define fx : Complex (f x))

    (and (rational? fx)
         (let refine ([δx : Nonnegative-Real (* ϵ (+ 1 (abs x)))]
                      [slopes : (Listof Real) null])
           (if (and (or (null? slopes)
                        (null? (cdr slopes))
                        (>= (abs (- (car slopes) (cadr slopes))) ϵ))
                    (< (length slopes) 10))
               
               (let ([δx/2 (* δx 1/2)]
                     [fx-δ (f (- x δx))])
                 (and (rational? fx-δ)
                      (refine δx/2 (cons (/ (- fx fx-δ) δx) slopes))))

               (slope-guard slopes ϵ))))))

(define d²f/dx² : DF/DX
  (lambda [f x0 [ϵ the-ϵ/2nd-order]]
    (define fx : Complex (f x0))

    (and (rational? fx)
         (let ([-2fx : Real (* -2.0 fx)]
               [δx0 : Nonnegative-Real (* ϵ (+ 1 (abs x0)))])
           (let refine ([δx : Nonnegative-Real δx0]
                        [fx+2δ : Complex (f (+ x0 δx0 δx0))]
                        [fx-2δ : Complex (f (- x0 δx0 δx0))]
                        [vs : (Listof Real) null])
             (if (and (or (null? vs)
                          (null? (cdr vs))
                          (>= (abs (- (car vs) (cadr vs))) ϵ))
                      (< (length vs) 10))
                 
                 (let ([δx/2 (* δx 1/2)]
                       [fx+δ (f (+ x0 δx))]
                       [fx-δ (f (- x0 δx))]
                       [δx² (* δx δx)])
                   (cond [(and (rational? fx+δ) (rational? fx-δ))
                          (refine δx/2 fx+δ fx-δ
                                  (cons (/ (+ fx+δ fx-δ -2fx) δx²) vs))]
                         [(and (rational? fx+δ) (rational? fx+2δ))
                          (refine δx/2 fx+δ fx-δ
                                  (cons (/ (+ fx+2δ fx (* -2.0 fx+δ)) δx²) vs))]
                         [(and (rational? fx-δ) (rational? fx-2δ))
                          (refine δx/2 fx+δ fx-δ
                                  (cons (/ (+ fx fx-2δ (* -2.0 fx-δ)) δx²) vs))]
                         [else #false]))
                 
                 (car vs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tangent-line : (->* ((-> Real Complex) Real) (#:%.lf Integer #:df/dx DF/DX Positive-Real) (Option (-> Real Real)))
  (lambda [f x [ϵ the-ϵ/1st-order] #:%.lf [%.lf 2] #:df/dx [df/dx df/dx]]
    (define fx : Complex (f x))
    (define k (df/dx  f x ϵ))

    (and (rational? k)
         (let* ([y (cond [(rational? fx) fx]
                         [else (let ([lim- (f (- x ϵ))]
                                     [lim+ (f (+ x ϵ))])
                                 (cond [(and (rational? lim-) (rational? lim+)) (* (+ lim- lim+) 1/2)]
                                       [(rational? lim-) lim-]
                                       [(rational? lim+) lim+]
                                      [else #false]))])]
                [b (and y (- y (* k x)))])
           (and b
                (procedure-rename
                 (λ [[x : Real]]
                   (+ (* k x) b))
                 (let ([K (~r k #:precision %.lf)]
                       [B (~r (abs b) #:precision %.lf)])
                   (string->symbol
                    (cond [(string=? K "0") (format "y = ~a" B)]
                          [(string=? B "0") (format "y = ~ax" (case K [("1") '||] [("-1") '-] [else K]))]
                          [else (format "y = ~ax ~a ~a"
                                  (case K [("1") '||] [("-1") '-] [else K])
                                  (if (< b 0.0) '- '+) B)])))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) safe-real-function : (-> (-> R (Option Complex)) (-> R Real))
  (lambda [f]
    ((inst procedure-rename (-> R Real))
     (λ [[x : R]]
       (with-handlers ([exn:fail? (λ _ +nan.0)])
         (let ([y (f x)])
           (cond [(real? y) y]
                 [else +nan.0]))))
     (assert (object-name f) symbol?))))

(define slope-guard : (-> (Listof Real) Positive-Real Real)
  (lambda [slopes ϵ]
    (define k (car slopes))

    (cond [(> k (/ +1 ϵ)) +inf.0]
          [(< k (/ -1 ϵ)) -inf.0]
          [else k])))

(define differentiable? : (-> (-> Real (Option Real)) Real Boolean)
  (lambda [f x]
    #false))
