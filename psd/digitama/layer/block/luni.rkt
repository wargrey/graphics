#lang typed/racket/base

(require "../../parser.rkt")
(require "../block.rkt")

(unsafe-provide luni)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define luni : (-> Bytes Index Index Null PSD-Layer-Tagged-Block)
  (lambda [layer-info start size argl]
    (define-values (name idx) (parse-utf16-string layer-info start))
    (psd:ltb:unicode:name name)))
