#lang typed/racket/base

(provide (except-out (all-defined-out) define-integer-parser define-integer-parser*))
(provide (all-from-out "number.rkt"))

(provide (rename-out [parse-size parse-msize]
                     [parse-int8 parse-mint8] [parse-uint8 parse-uint8]
                     [parse-int16 parse-mint16] [parse-uint16 parse-muint16]
                     [parse-int32 parse-mint32] [parse-uint32 parse-muint32]
                     [parse-int64 parse-mint64] [parse-uint64 parse-muint64]))

(provide (rename-out [msb-bytes->double parse-double]
                     [msb-bytes->double parse-mdouble]
                     [lsb-bytes->double parse-ldouble]))

(require typed/racket/unsafe)
(unsafe-require/typed racket/base [object-name (-> Procedure Symbol)])

(require "number.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-type Throw-Range-Error (-> Symbol String Any Nothing))

(define-syntax (assert* stx)
  (syntax-case stx []
    [(_ sexp pred throw) #'(assert* sexp pred throw 'assert)]
    [(_ sexp pred throw src)
     #`(let ([v sexp]
             [? pred])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (? v) v (throw src (symbol->string (object-name ?)) v)))
            'feature-profile:TR-dynamic-check #t))]))

(define-syntax (define-integer-parser stx)
  (syntax-case stx [:]
    [(_ parse-integer do-bytes->integer [argl : Argl] ... #:-> Integer_t)
     (with-syntax ([parse-integer* (format-id #'parse-integer "~a*" (syntax-e #'parse-integer))])
       #'(begin (define parse-integer : (-> Bytes Integer Argl ... Integer_t)
                  (lambda [src start argl ...]
                    (do-bytes->integer src start argl ...)))
                
                (define parse-integer* : (All (a) (-> Bytes Integer Argl ... (-> Any Boolean : #:+ a) Throw-Range-Error a))
                  (lambda [src start argl ... subinteger? throw]
                    (assert* (do-bytes->integer src start argl ...) subinteger? throw)))))]
    [(_ parse-integer do-bytes->integer signed? #:-> Integer_t)
     (with-syntax ([parse-integer* (format-id #'parse-integer "~a*" (syntax-e #'parse-integer))])
       #'(begin (define (parse-integer [src : Bytes] [start : Integer]) : Integer_t (do-bytes->integer src start signed?))

                (define parse-integer* : (All (a) (-> Bytes Integer (-> Any Boolean : #:+ a) Throw-Range-Error a))
                  (lambda [src start subinteger? throw]
                    (assert* (do-bytes->integer src start signed?) subinteger? throw)))))]))

(define-syntax (define-integer-parser* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer [parse-integer #:-> Integer] [parse-uinteger #:-> Natural]] ...)
     #'(begin (define-integer-parser parse-integer  do-bytes->integer #true  #:-> Integer) ...
              (define-integer-parser parse-uinteger do-bytes->integer #false #:-> Natural) ...)]))

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
  [msb-bytes->octet [parse-int8  #:-> Fixnum]  [parse-uint8  #:-> Byte]]
  [msb-bytes->short [parse-int16 #:-> Fixnum]  [parse-uint16 #:-> Index]]
  [msb-bytes->int   [parse-int32 #:-> Fixnum]  [parse-uint32 #:-> Nonnegative-Fixnum]]
  [msb-bytes->long  [parse-int64 #:-> Integer] [parse-uint64 #:-> Natural]])

(define-integer-parser*
  [lsb-bytes->octet [parse-lint8  #:-> Fixnum]  [parse-luint8  #:-> Byte]]
  [lsb-bytes->short [parse-lint16 #:-> Fixnum]  [parse-luint16 #:-> Index]]
  [lsb-bytes->int   [parse-lint32 #:-> Fixnum]  [parse-luint32 #:-> Nonnegative-Fixnum]]
  [lsb-bytes->long  [parse-lint64 #:-> Integer] [parse-luint64 #:-> Natural]])

(define-integer-parser parse-size msb-bytes->size [size : Integer] #:-> Index)
(define-integer-parser parse-lsize lsb-bytes->size [size : Integer] #:-> Index)

(define parse-nsizes-list : (-> Bytes Integer Integer Integer (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Index (assert (+ (* (- count 1) size) start) index?)]
                [dest : (Listof Index) null])
      (cond [(< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (assert (- idx size) index?) (cons n dest)))]))))

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
