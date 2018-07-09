#lang typed/racket/base

;;; https://www.w3.org/TR/PNG/#9Filters

(provide filter-scanline-reconstruct)

(require bitmap/stdio)

(require "enum.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define filter-scanline-reconstruct : (-> Input-Port (Listof Bytes) Positive-Index Positive-Byte PNG-Filter-Method (Listof Bytes))
  (lambda [/dev/stdin blocks fxwidth fxcount filter-method]
    (let reconstruct ([rest : (Listof Bytes) blocks]
                      [senilnacs : (Listof Bytes) null])
      (cond [(null? rest) senilnacs]
            [else (let*-values ([(block blocks) (values (car rest) (cdr rest))]
                                [(senil maybe-rest-in-line) (block-reconstruct /dev/stdin block fxwidth fxcount senilnacs)])
                    (cond [(not maybe-rest-in-line) (reconstruct blocks senil)]
                          [(pair? blocks) (reconstruct (cons (bytes-append maybe-rest-in-line (car blocks)) (cdr blocks)) senil)]
                          [else senil #| this should not happen |#]))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define block-reconstruct : (-> Input-Port Bytes Positive-Index Positive-Byte (Listof Bytes) (Values (Listof Bytes) (Option Bytes)))
  (lambda [/dev/stdin block fxwidth fxcount senilnacs]
    (define size : Index (bytes-length block))
    (define stride : Index (assert (* fxwidth fxcount) index?))
    (let deserialize ([idx : Index 0]
                      [senilnacs : (Listof Bytes) senilnacs])
      (cond [(= idx size) (values senilnacs #false)]
            [else (let* ([start : Natural (+ idx 1)]
                         [end : Natural (+ start stride)])
                    (cond [(> end size) (values senilnacs (subbytes block idx size))]
                          [else (let ([filter-type (bytes-ref block idx)]
                                      [cline (subbytes block start end)])
                                  (when (and (> filter-type 0) (pair? senilnacs))
                                    (define pline : Bytes (car senilnacs))
                                    (case filter-type
                                      [(1) (filtering-reconstruct-sub     cline pline stride fxcount)]
                                      [(2) (filtering-reconstruct-up      cline pline stride fxcount)]
                                      [(3) (filtering-reconstruct-average cline pline stride fxcount)]
                                      [(4) (filtering-reconstruct-paeth   cline pline stride fxcount)]
                                      [else (throw-range-error 'deserialize-block (cons 0 4) filter-type "filter-type")]))
                                  (deserialize end (cons cline senilnacs)))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-filter-function stx)
  (syntax-case stx []
    [(_ filter-type #:lambda [cline pline fxcount] #:with i [var ref-sexp] ... #:=> sample)
     (with-syntax ([filter-function (format-id #'filter-type "filtering-filter-~a" (syntax-e #'filter-type))]
                   [reconstruct-function (format-id #'filter-type "filtering-reconstruct-~a" (syntax-e #'filter-type))])
       #'(begin (define filter-function : (-> Bytes Bytes Index Positive-Byte Void)
                  (lambda [cline pline stride fxcount]
                    (let filtering ([i : Positive-Fixnum fxcount])
                      (when (< i stride)
                        (define x (bytes-ref cline i))
                        (define var ref-sexp) ...
                        (bytes-set! cline i (- x sample))
                        (filtering (+ i 1))))))
                
                (define reconstruct-function : (-> Bytes Bytes Index Positive-Byte Void)
                  (lambda [cline pline stride fxcount]
                    (let filtering ([i : Positive-Fixnum fxcount])
                      (when (< i stride)
                        (define x (bytes-ref cline i))
                        (define var ref-sexp) ...
                        (bytes-set! cline i (+ x sample))
                        (filtering (+ i 1))))))))]))

(define-filter-function sub
  #:lambda [cline pline fxcount]
  #:with i [a (bytes-ref cline (- i fxcount))]
  #:=> a)

(define-filter-function up
  #:lambda [cline pline fxcount]
  #:with i [b (bytes-ref pline i)]
  #:=> b)

(define-filter-function average
  #:lambda [cline pline fxcount]
  #:with i [a (bytes-ref cline (- i fxcount))] [b (bytes-ref pline i)]
  #:=> (quotient (+ a b) 2))

(define-filter-function paeth
  #:lambda [cline pline fxcount]
  #:with i [a (bytes-ref cline (- i fxcount))] [b (bytes-ref pline i)] [c (bytes-ref pline (- i fxcount))]
  #:=> (PaethPredicator a b c))

(define PaethPredicator : (-> Byte Byte Byte Byte)
  (lambda [a b c]
    (define p (+ a b (- c)))
    (define pa (abs (- p a)))
    (define pb (abs (- p b)))
    (define pc (abs (- p c)))
    (cond [(and (<= pa pb) (<= pa pc)) a]
          [(<= pb pc) b]
          [else c])))
