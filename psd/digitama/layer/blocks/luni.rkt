#lang typed/racket/base

(require "../../parser.rkt")
(require "../format.rkt")

(unsafe-provide luni)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define luni : (-> Bytes Fixnum Index Null PSD-Layer-Unicode-Name)
  (lambda [layer-info start size argl]
    (define-values (name idx) (parse-utf16-string layer-info start))
    (PSD-Layer-Unicode-Name name)))
