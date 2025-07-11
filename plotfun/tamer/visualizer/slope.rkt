#lang typed/racket/base

(require geofun/vector)
(require plotfun/cartesian)
(require plotfun/digitama/calculus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (|(x²+x-2)/(x²-x)| [x : Real]) : (Option Real)
  (and (or (not (plot-sampling?))
           (and (> (abs x) 0.01)
                (> (abs (- x 1)) 0.01)))
       (/ (+ (* x x) x -2)
          (- (* x x) x))))

(define (|xsin(1/x)| [x : Real]) : (Option Real)
  (* x (sin (/ 1 x))))

(define (+sqrt [x : Real]) : (Option Complex)
  (* +1.0 (sqrt x)))

(define (-sqrt [x : Real]) : (Option Complex)
  (* -1.0 (sqrt x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vfuncs : (->* ((-> Real (Option Complex)) (U Real (Listof Real))) () (Listof Plot-Visualizer))
   (lambda [f xs]
    (define safe-f (safe-real-function f))
    (define delta 0.618)
    
    (list* (function safe-f)
           
           (filter plot-visualizer?
                   (for/list : (Listof (Option Plot-Visualizer)) ([x (if (list? xs) (in-list xs) (in-value xs))])
                     (define tanline (tangent-line safe-f x))

                     (and tanline
                          (function #:label 'name #:safe-slope safe-f
                                    tanline (- x delta) (+ x delta))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geo-hb-append
 (plot-cartesian
  #:x-range (cons -5 5)
  #:y-range (cons -5 5)
  (list (vfuncs |(x²+x-2)/(x²-x)| '(-1 0 1 2))))

 (plot-cartesian
  #:x-range (cons -5 5)
  #:y-range (cons -5 5)
  (list (vfuncs sin '(-2 0 2))
        (vfuncs exp 1)))

 (plot-cartesian
  #:x-range (cons -1 9)
  #:y-range (cons -5 5)
  (list (vfuncs +sqrt '(0 2))
        (vfuncs -sqrt '(0 2)))))

(geo-hb-append
 (plot-cartesian
  #:width 1000.0
  #:x-range (cons -2 2)
  #:y-range (cons -1 2)
  (list (vfuncs abs '(0 1))
        (vfuncs |xsin(1/x)| '(-1 0 1)))))
