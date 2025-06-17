#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define df/dx : (->* ((-> Real (Option Complex)) Real) (Positive-Real) (Option Real))
  (lambda [f x [ϵ #;|sqrt machine ϵ| 1/100000000]]
    (define fx : (Option Complex)
      (with-handlers ([exn:fail? (λ _ +nan.0)])
        (f x)))

    (let iterate ([δx : Nonnegative-Real (* ϵ (max 1 (abs x)))]
                  [prev : Real +inf.0]
                  [d : Real 0.0])
      (if (and (>= (abs (- d prev)) ϵ)
               (>= δx 1/1000000000000))
          (let ([δx/2 (* δx 1/2)]
                [f+ (f (+ x δx))]
                [f_ (f (- x δx))])
            (cond [(and (rational? f+) (rational? f_)) (iterate δx/2 d (/ (- f+ f_) (* δx 2)))]
                  [(and (rational? f+) (rational? fx)) (iterate δx/2 d (/ (- f+ fx) δx))]
                  [(and (rational? f_) (rational? fx)) (iterate δx/2 d (/ (- fx f_) δx))]
                  [else #false]))
          d))))

(define df/dx+ : (->* ((-> Real (Option Complex)) Real) (Positive-Real) (Option Real))
  (lambda [f x [ϵ #;|sqrt machine ϵ| 1/100000000]]
    (define fx : (Option Complex)
      (with-handlers ([exn:fail? (λ _ +nan.0)])
        (f x)))

    (let iterate ([δx : Nonnegative-Real (* ϵ (max 1 (abs x)))]
                  [prev : Real +inf.0]
                  [d : Real 0.0])
      (if (and (>= (abs (- d prev)) ϵ)
               (>= δx 1/1000000000000))
          (let ([δx/2 (* δx 1/2)]
                [f+ (f (+ x δx))]
                [f_ (f (- x δx))])
            (cond [(and (rational? f+) (rational? f_)) (iterate δx/2 d (/ (- f+ f_) (* δx 2)))]
                  [(and (rational? f+) (rational? fx)) (iterate δx/2 d (/ (- f+ fx) δx))]
                  [(and (rational? f_) (rational? fx)) (iterate δx/2 d (/ (- fx f_) δx))]
                  [else #false]))
          d))))

(define df/dx_ : (->* ((-> Real (Option Complex)) Real) (Positive-Real) (Option Real))
  (lambda [f x [ϵ #;|sqrt machine ϵ| 1/100000000]]
    (define fx : (Option Complex)
      (with-handlers ([exn:fail? (λ _ +nan.0)])
        (f x)))

    (let iterate ([δx : Nonnegative-Real (* ϵ (max 1 (abs x)))]
                  [prev : Real +inf.0]
                  [d : Real 0.0])
      (if (and (>= (abs (- d prev)) ϵ)
               (>= δx 1/1000000000000))
          (let ([δx/2 (* δx 1/2)]
                [f+ (f (+ x δx))]
                [f_ (f (- x δx))])
            (cond [(and (rational? f+) (rational? f_)) (iterate δx/2 d (/ (- f+ f_) (* δx 2)))]
                  [(and (rational? f+) (rational? fx)) (iterate δx/2 d (/ (- f+ fx) δx))]
                  [(and (rational? f_) (rational? fx)) (iterate δx/2 d (/ (- fx f_) δx))]
                  [else #false]))
          d))))

(define (f [x : Real]) : (Option Real)
  (/ (+ (* x x) x -2)
     (- (* x x) x)))

(cons (df/dx sin 1) (cos 1))
(cons (df/dx exp 1) (exp 1))
(df/dx f 1)
(df/dx abs 0)
(df/dx sqrt 1/2)
(df/dx sqrt 1)
(df/dx sqrt 0.0)
(df/dx (λ [x] (* x (sin (/ 1 x)))) 0)
(df/dx (λ [x] (expt x 3/2)) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define differentiable? : (-> (-> Real (Option Real)) Real Boolean)
  (lambda [f x]
    #false))
