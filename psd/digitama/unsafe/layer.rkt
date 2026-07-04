#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../layer/block.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-layer-tagged-block-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [psd-layer-tagged-block-parser? (-> Any Boolean : PSD-Layer-Tagged-Block-Parser)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-info-parsers : (HashTable Symbol PSD-Layer-Tagged-Block-Parser) (make-hasheq))

(define make-fallback-parser : (-> (-> PSD-Layer-Tagged-Block) PSD-Layer-Tagged-Block-Parser)
  (lambda [fallback]
    (λ [[bs : Bytes] [idx : Fixnum] [size : Index] [args : (Listof Any)]] : PSD-Layer-Tagged-Block
      (fallback))))
