#lang typed/racket/base

(provide (all-defined-out))

(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Placement (U 'inside 'outside 'center))
(define-type Option-Geo-Tip (Option Geo-Tip))
(define-type Maybe-Geo-Tip (U Option-Geo-Tip Void))

(struct Geo-Tip ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Datum (Immutable-Vector Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))

(define geo-tip-values : (-> Geo-Tip-Datum (Values Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex Boolean))
  (lambda [self]
    (values (vector-ref self 0)
            (vector-ref self 1) (vector-ref self 2)
            (vector-ref self 3) (vector-ref self 4)
            (vector-ref self 5) (vector-ref self 6))))

(define tip-db : (Weak-HashTable Any Geo-Tip-Datum) (make-weak-hash))
