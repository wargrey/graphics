#lang typed/racket/base

(provide (all-defined-out))

(define-type Color (U Symbol Integer Keyword FlColor))
(define-type Point2D (U Complex (Pairof Real Real) (List Real Real)))

(struct paint () #:transparent #:type-name Paint)
(struct flcolor () #:transparent #:type-name FlColor)
(struct rgba flcolor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum])
  #:type-name FlRGBA
  #:transparent)

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

(define color? : (-> Any Boolean : Color)
  (lambda [v]
    (or (flcolor? v)
        (symbol? v)
        (exact-integer? v)
        (keyword? v))))
