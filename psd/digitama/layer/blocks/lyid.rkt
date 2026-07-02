#lang typed/racket/base

(require "../../parser.rkt")
(require "../format.rkt")

(unsafe-provide lyid)

(define lyid : (-> Bytes Fixnum Index Null PSD-Layer-Id)
  (lambda [layer-info start size argl]
    (PSD-Layer-Id (parse-uint32 layer-info start index?))))
