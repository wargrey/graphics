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
;;; https://drafts.csswg.org/css-color/#color-conversion-code
(define color-component-gamma-encode : (-> Flonum Flonum)
  (lambda [c]
    (define sgn (if (negative? c) -1.0 1.0))
    (define abs (flabs c))
    
    (if (fl<= abs 0.0031308)
        (fl* c 12.92)
        (fl* sgn (fl- (fl* (flexpt abs (fl/ 1.0 2.4)) 1.055) 0.055)))))

(define color-component-gamma-decode : (-> Flonum Flonum)
  (lambda [c]
    (define sgn (if (negative? c) -1.0 1.0))
    (define abs (flabs c))

    (if (fl<= abs 0.04045)
        (fl/ c 12.92)
        (fl* sgn (flexpt (fl/ (fl+ 0.055 abs) 1.055) 2.4)))))
