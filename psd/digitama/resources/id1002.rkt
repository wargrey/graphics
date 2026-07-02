#lang typed/racket/base

(require "format.rkt")
(require "../exn.rkt")

(unsafe-provide 0x3ea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x3ea : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size args]
    (throw-obsolete-error id "No longer read by Photoshop")))
