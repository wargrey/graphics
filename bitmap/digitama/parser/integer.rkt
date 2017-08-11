#lang typed/racket/base

(provide (all-defined-out))

(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (> v 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))
