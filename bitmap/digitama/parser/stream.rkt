#lang typed/racket/base

(provide (except-out (all-defined-out) assert*))
(provide (all-from-out "integer.rkt"))
(provide (rename-out [bytes-ref parse-uint8]))

(require typed/racket/unsafe)
(unsafe-require/typed racket/base [object-name (-> Procedure Symbol)])

(require "integer.rkt")

(require (for-syntax racket/base))

(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw)
     #`(let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw 'assert (symbol->string (object-name ?)) v)))
            'feature-profile:TR-dynamic-check #t))]))

(define parse-boolean : (-> Bytes Fixnum Boolean)
  (lambda [src start]
    (> (bytes-ref src start) 0)))

(define parse-keyword : (All (a) (case-> [Bytes Fixnum -> Symbol]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start key? throw) (assert* (parse-keyword src start) key? throw)]
    [(src start)
     (let ([3chkey? : Boolean (= (bytes-ref src (+ start 3)) 32)])
       (define key : String (bytes->string/utf-8 src #false start (+ start (if 3chkey? 3 4))))
       (string->symbol key))]))

(define parse-int16 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : #:+ a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start) (parse-int16 src start fixnum? raise-result-error)]
    [(src start subinteger? throw) (assert* (integer-bytes->integer src #true #true start (+ start 2)) subinteger? throw)]))

(define parse-uint16 : (All (a) (case-> [Bytes Fixnum -> Index]
                                        [Bytes Fixnum (-> Any Boolean : a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start) (parse-size src start 2 index? raise-result-error)]
    [(src start subinteger? throw) (parse-size src start 2 subinteger? throw)]))

(define parse-int32 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : #:+ a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start) (parse-int32 src start fixnum? raise-result-error)]
    [(src start subinteger? throw) (assert* (integer-bytes->integer src #true #true start (+ start 4)) subinteger? throw)]))

(define parse-uint32 : (All (a) (case-> [Bytes Fixnum -> Nonnegative-Fixnum]
                                        [Bytes Fixnum (-> Any Boolean : a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start) (parse-size src start 4 nonnegative-fixnum? raise-result-error)]
    [(src start subinteger? throw) (parse-size src start 4 subinteger? throw)]))

(define parse-size : (All (a) (case-> [Bytes Fixnum Fixnum -> Index]
                                      [Bytes Fixnum Fixnum (-> Any Boolean : a) (-> Symbol String Any Any * Nothing) -> a]))
  (case-lambda
    [(src start size) (parse-size src start size index? raise-result-error)]
    [(src start size size? throw) (assert* (integer-bytes->integer src #false #true start (+ start size)) size? throw)]))

(define parse-nsizes-list : (-> Bytes Fixnum Fixnum Fixnum (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Index (assert (+ (* (- count 1) size) start) index?)]
                [dest : (Listof Index) null])
      (cond [(< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (assert (- idx size) index?) (cons n dest)))]))))

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
    (let parse ([last-end : Index (assert start index?)]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (assert (+ last-end (car sizes)) index?)])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
