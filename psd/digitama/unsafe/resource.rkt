#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../resource/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-resource-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 6))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [psd-resource-parser? (-> Any Boolean : PSD-Resource-Parser)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-res-parsers : (HashTable Fixnum PSD-Resource-Parser) (make-hasheq))

(define make-fallback-parser : (-> (-> PSD-Resource) PSD-Resource-Parser)
  (lambda [fallback]
    (λ [[id : Integer] [name : String] [bs : Bytes] [idx : Fixnum] [size : Index] [args : (Listof Any)]] : PSD-Resource
      (fallback))))
