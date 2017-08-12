#lang typed/racket/base

(provide (except-out (all-defined-out) assert* define-integer-parser define-integer-parser*))
(provide (all-from-out "number.rkt"))

(require typed/racket/unsafe)
(unsafe-require/typed racket/base [object-name (-> Procedure Symbol)])

(require "number.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-type Throw-Range-Error (-> Symbol String Any Any * Nothing))

(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw)
     #`(let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw 'assert (symbol->string (object-name ?)) v)))
            'feature-profile:TR-dynamic-check #t))]))

(define-syntax (define-integer-parser stx)
  (syntax-case stx [:]
    [(_ parse-integer real-bytes->integer [argl : Argl] ... #:-> Integer_t)
     (with-syntax ([parse-integer* (format-id #'parse-integer "~a*" (syntax-e #'parse-integer))])
       #'(begin (define parse-integer : (-> Bytes Integer Argl ... Integer_t)
                  (lambda [src start argl ...]
                    (real-bytes->integer src start argl ...)))
                
                (define parse-integer* : (All (a) (-> Bytes Integer Argl ... (-> Any Boolean : #:+ a) Throw-Range-Error a))
                  (lambda [src start argl ... subinteger? throw]
                    (assert* (real-bytes->integer src start argl ...) subinteger? throw)))))]
    [(_ parse-integer real-bytes->integer signed? #:-> Integer_t)
     (with-syntax ([parse-integer* (format-id #'parse-integer "~a*" (syntax-e #'parse-integer))])
       #'(begin (define (parse-integer [src : Bytes] [start : Integer]) : Integer_t (real-bytes->integer src start signed?))

                (define parse-integer* : (All (a) (-> Bytes Integer (-> Any Boolean : #:+ a) Throw-Range-Error a))
                  (lambda [src start subinteger? throw]
                    (assert* (real-bytes->integer src start signed?) subinteger? throw)))))]))

(define-syntax (define-integer-parser* stx)
  (syntax-case stx [:]
    [(_ [real-bytes->integer [parse-integer #:-> Integer] [parse-uinteger #:-> Natural]] ...)
     #'(begin (define-integer-parser parse-integer  real-bytes->integer #true  #:-> Integer) ...
              (define-integer-parser parse-uinteger real-bytes->integer #false #:-> Natural) ...)]))

(define parse-boolean : (-> Bytes Fixnum Boolean)
  (lambda [src start]
    (> (bytes-ref src start) 0)))

(define parse-keyword : (All (a) (case-> [Bytes Fixnum -> Symbol]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) Throw-Range-Error -> a]))
  (case-lambda
    [(src start key? throw) (assert* (parse-keyword src start) key? throw)]
    [(src start)
     (let ([3chkey? : Boolean (= (bytes-ref src (+ start 3)) 32)])
       (define key : String (bytes->string/utf-8 src #false start (+ start (if 3chkey? 3 4))))
       (string->symbol key))]))

(define-integer-parser*
  [real-bytes->octet [parse-int8  #:-> Fixnum]  [parse-uint8  #:-> Byte]]
  [real-bytes->short [parse-int16 #:-> Fixnum]  [parse-uint16 #:-> Index]]
  [real-bytes->int   [parse-int32 #:-> Fixnum]  [parse-uint32 #:-> Nonnegative-Fixnum]]
  [real-bytes->long  [parse-int64 #:-> Integer] [parse-uint64 #:-> Natural]])

(define-integer-parser parse-size real-bytes->size [size : Integer] #:-> Index)

(define parse-nsizes-list : (-> Bytes Integer Integer Integer (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Index (assert (+ (* (- count 1) size) start) index?)]
                [dest : (Listof Index) null])
      (cond [(< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (assert (- idx size) index?) (cons n dest)))]))))

(define parse-double : (-> Bytes Integer Flonum)
  (lambda [src start]
    (real-bytes->double src start)))

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
