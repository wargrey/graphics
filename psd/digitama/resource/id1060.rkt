#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")

(unsafe-provide 0x424)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x424 : (-> Integer String Bytes Index Index Null PSD:Res:File:Info)
  (lambda [id name xmp idx size argl]
    (psd:res:file:info id name (make-special-comment (parse-nbytes xmp idx size)))))
