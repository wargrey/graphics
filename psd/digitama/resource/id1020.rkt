#lang typed/racket/base

(require "format.rkt")
(require "../exn.rkt")

(unsafe-provide 0x3fc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x3fc : (-> Integer String Bytes Index Index Null PSD-Resource)
  (lambda [id name block idx size args]
    (throw-obsolete-error id)))
