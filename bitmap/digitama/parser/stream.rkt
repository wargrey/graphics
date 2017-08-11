#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "integer.rkt"))
(provide (rename-out [bytes-ref parse-uint8]))

(require "integer.rkt")

(define parse-boolean : (-> Bytes Fixnum Boolean)
  (lambda [src start]
    (> (bytes-ref src start) 0)))

(define parse-keyword : (All (a) (case-> [Bytes Fixnum -> Symbol]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) -> a]))
  (case-lambda
    [(src start key?) (assert (parse-keyword src start) key?)]
    [(src start)
     (let ([3chkey? : Boolean (= (bytes-ref src (+ start 3)) 32)])
       (define key : String (bytes->string/utf-8 src #false start (+ start (if 3chkey? 3 4))))
       (string->symbol key))]))

(define parse-int16 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-int16 src start fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #true #true start (+ start 2)) subinteger?)]))

(define parse-uint16 : (All (a) (case-> [Bytes Fixnum -> Index]
                                        [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-size src start 2)]
    [(src start subinteger?) (parse-size src start 2 subinteger?)]))

(define parse-int32 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-int32 src start fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #true #true start (+ start 4)) subinteger?)]))

(define parse-uint32 : (All (a) (case-> [Bytes Fixnum -> Nonnegative-Fixnum]
                                        [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-uint32 src start nonnegative-fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #false #true start (+ start 4)) subinteger?)]))

(define parse-size : (All (a) (case-> [Bytes Fixnum Fixnum -> Index]
                                      [Bytes Fixnum Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start size) (parse-size src start size index?)]
    [(src start size size?) (assert (integer-bytes->integer src #false #true start (+ start size)) size?)]))

(define parse-nsizes-list : (-> Bytes Fixnum Fixnum Fixnum (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Fixnum (assert (+ (* (- count 1) size) start) fixnum?)]
                [dest : (Listof Index) null])
      (cond [(< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (assert (- idx size) fixnum?) (cons n dest)))]))))

(define parse-double : (-> Bytes Fixnum Flonum)
  (lambda [src start]
    (with-asserts ([start index?])
      (floating-point-bytes->real src #true start (+ start 8)))))

(define parse-nbytes : (-> Bytes Fixnum Fixnum Bytes)
  (lambda [src start bsize]
    (subbytes src start (+ start bsize))))

(define parse-nbytes-list : (-> Bytes Fixnum (Listof Index) (Listof Bytes))
  (lambda [src start bsizes]
    (for/list : (Listof Bytes) ([interval (in-list (nbytes-pairs start bsizes))])
      (subbytes src (car interval) (cdr interval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nbytes-pairs : (-> Fixnum (Listof Index) (Listof (Pairof Integer Integer)))
  (lambda [start bsizes]
    (let parse ([last-end : Fixnum start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (assert (+ last-end (car sizes)) fixnum?)])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
