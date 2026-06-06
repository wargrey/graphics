#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Brightness-Threshold (U Nonnegative-Flonum (Pairof Nonnegative-Flonum Nonnegative-Flonum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define brightness-threshold : (-> Brightness-Threshold (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [T]
    (cond [(pair? T) (values (car T) (cdr T))]
          [else (values (max 0.0 (- 0.5 T)) (min 1.0 (+ 0.5 T)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define wcag-contrast-ratio : (-> Flonum Flonum Flonum)
  (lambda [Lf bgL]
    (/ (+ (max Lf bgL) 0.05)
       (+ (min Lf bgL) 0.05))))

(define wcag-minimum-contrast : (-> Flonum Nonnegative-Flonum Flonum)
  (lambda [bgL cr]
    (if (> bgL 0.5)
        (- (/ (+ bgL 0.05) cr) 0.05)
        (- (* (+ bgL 0.05) cr) 0.05))))
