#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require "../unsafe/require.rkt")

(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (> v 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)

  (define (real-bytes->octet src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 1)))
  (define (real-bytes->short src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 2)))
  (define (real-bytes->int src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 4)))
  (define (real-bytes->long src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 8)))
  (define (real-bytes->size src idx size) (integer-bytes->integer src #false #true idx (unsafe-fx+ idx size)))
  (define (real-bytes->double src idx) (floating-point-bytes->real src #true idx (unsafe-fx+ idx 8))))

(unsafe/require/provide
 (submod "." unsafe)
 [real-bytes->octet (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Byte])]
 [real-bytes->short (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Index])]
 [real-bytes->int (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Nonnegative-Fixnum])]
 [real-bytes->long (case-> [Bytes Integer True -> Integer] [Bytes Integer False -> Natural])]
 [real-bytes->size (-> Bytes Integer Integer Index)]
 [real-bytes->double (-> Bytes Integer Flonum)])
