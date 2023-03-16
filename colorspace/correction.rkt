#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Color-Correct (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define color-gamma-encode : Color-Correct
  (lambda [r g b]
    (values (color-component-gamma-encode r)
            (color-component-gamma-encode g)
            (color-component-gamma-encode b))))

(define color-gamma-decode : Color-Correct
  (lambda [r g b]
    (values (color-component-gamma-decode r)
            (color-component-gamma-decode g)
            (color-component-gamma-decode b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define color-component-gamma-encode : (-> Flonum Flonum)
  (lambda [c]
    (if (<= c 0.0031308)
        (* c 12.92)
        (- (* (flexpt c (/ 1.0 2.4)) 1.055) 0.055))))

(define color-component-gamma-decode : (-> Flonum Flonum)
  (lambda [c]
    (if (<= c 0.04045) ; 0.03928 is outdated
        (/ c 12.92)
        (flexpt (/ (+ 0.055 c) 1.055) 2.4))))
