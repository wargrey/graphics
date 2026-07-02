#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")

(unsafe-provide 0x3f3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x3f3 : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size args]
    (PSD-Print-Flags id name
                     (parse-boolean block idx)
                     (parse-boolean block (unsafe-fx+ idx 1))
                     (parse-boolean block (unsafe-fx+ idx 2))
                     (parse-boolean block (unsafe-fx+ idx 3))
                     (parse-boolean block (unsafe-fx+ idx 4))
                     (parse-boolean block (unsafe-fx+ idx 5))
                     (parse-boolean block (unsafe-fx+ idx 6))
                     (parse-boolean block (unsafe-fx+ idx 7))
                     (parse-boolean block (unsafe-fx+ idx 8)))))
