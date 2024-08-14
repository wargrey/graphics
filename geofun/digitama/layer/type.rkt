#lang typed/racket/base

(provide (all-defined-out))

(require "../convert.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Layer (Immutable-Vector Geo<%> Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
(define-type Geo-Layer-List (Pairof Geo-Layer (Listof Geo-Layer)))
(define-type Geo-Layer-Group (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Geo-Layer-List))
