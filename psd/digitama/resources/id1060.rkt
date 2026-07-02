#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")

(unsafe-provide 0x424)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x424 : (-> Integer String Bytes Fixnum Index Null PSD-File-Info)
  (lambda [id name xmp idx size argl]
    (PSD-File-Info id name (make-special-comment (parse-nbytes xmp idx size)))))
