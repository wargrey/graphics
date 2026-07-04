#lang typed/racket/base

(require "../../parser.rkt")
(require "../block.rkt")

(unsafe-provide lyid)

(define lyid : (-> Bytes Index Index Null PSD-Layer-Tagged-Block)
  (lambda [layer-info start size argl]
    (psd:ltb:id (parse-uint32 layer-info start index?))))
