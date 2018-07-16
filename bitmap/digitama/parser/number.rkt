#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(define nonnegative-fixnum? : (-> Any Boolean : Nonnegative-Fixnum) (λ [n] (and (fixnum? n) (>= n 0))))

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (> v 0))))
(define positive-fixnum? : (-> Any Boolean : Positive-Fixnum) (λ [n] (and (fixnum? n) (> n 0))))

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)

  (define (msb-bytes->octet src idx signed?) (unsafe-bytes-ref src idx))
  (define (msb-bytes->short src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 2)))
  (define (msb-bytes->int src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 4)))
  (define (msb-bytes->long src idx signed?) (integer-bytes->integer src signed? #true idx (unsafe-fx+ idx 8)))
  (define (msb-bytes->size src idx size) (integer-bytes->integer src #false #true idx (unsafe-fx+ idx size)))
  (define (msb-bytes->double src idx) (floating-point-bytes->real src #true idx (unsafe-fx+ idx 8)))

  (define (lsb-bytes->octet src idx signed?) (unsafe-bytes-ref src idx))
  (define (lsb-bytes->short src idx signed?) (integer-bytes->integer src signed? #false idx (unsafe-fx+ idx 2)))
  (define (lsb-bytes->int src idx signed?) (integer-bytes->integer src signed? #false idx (unsafe-fx+ idx 4)))
  (define (lsb-bytes->long src idx signed?) (integer-bytes->integer src signed? #false idx (unsafe-fx+ idx 8)))
  (define (lsb-bytes->size src idx size) (integer-bytes->integer src #false #false idx (unsafe-fx+ idx size)))
  (define (lsb-bytes->double src idx) (floating-point-bytes->real src #false idx (unsafe-fx+ idx 8))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [msb-bytes->octet (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Byte])]
 [msb-bytes->short (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Index])]
 [msb-bytes->int (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Nonnegative-Fixnum])]
 [msb-bytes->long (case-> [Bytes Integer True -> Integer] [Bytes Integer False -> Natural])]
 [msb-bytes->size (-> Bytes Integer Integer Index)]
 [msb-bytes->double (-> Bytes Integer Flonum)]

 [lsb-bytes->octet (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Byte])]
 [lsb-bytes->short (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Index])]
 [lsb-bytes->int (case-> [Bytes Integer True -> Fixnum] [Bytes Integer False -> Nonnegative-Fixnum])]
 [lsb-bytes->long (case-> [Bytes Integer True -> Integer] [Bytes Integer False -> Natural])]
 [lsb-bytes->size (-> Bytes Integer Integer Index)]
 [lsb-bytes->double (-> Bytes Integer Flonum)])
