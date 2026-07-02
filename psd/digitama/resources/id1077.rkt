#lang typed/racket/base

(require "format.rkt")

(unsafe-provide 0x435)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Don't forget to update 1007
(define 0x435 : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size argl]
    (PSD-Resource id name)))
