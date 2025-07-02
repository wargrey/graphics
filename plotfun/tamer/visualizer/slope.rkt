#lang typed/racket/base

(require plotfun/digitama/calculus)
(require plotfun/cartesian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f [x : Real]) : (Option Number)
  (/ (+ (* x x) x -2)
     (- (* x x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vfuncs : (-> (-> Real (Option Number)) (U Real (Listof Real)) (Listof Plot-Visualizer))
  (lambda [f xs]
    (define delta 1)
    
    (list* (function f)
           
           (filter plot-visualizer?
                   (for/list : (Listof (Option Plot-Visualizer)) ([x (if (list? xs) (in-list xs) (in-value xs))]
                                                                  [ref (in-naturals 1)])
                     (define k (df/dx f x))

                     (printf "~a@~a: ~a~n" (object-name f) x k)
                     (and k (function #:color (- ref)
                                      (λ [[v : Real]] : (Option Complex)
                                        (* k v)) (- x delta) (+ x delta))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(plot-cartesian
 #:x-range (cons -5 5)
 #:y-range (cons -5 5)
 (list (vfuncs sin 1)
       (vfuncs exp 1)
       (vfuncs f 1)
       (vfuncs abs 1)
       (vfuncs sqrt '(0.0 1/2 1))
       (vfuncs (λ [x] (* x (sin (/ 1 x)))) 0)
       (vfuncs (λ [x] (expt x 3/2)) 0)))
