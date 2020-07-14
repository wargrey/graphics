#lang typed/racket/base

(provide (all-defined-out))

(require "../../digitama/font.rkt")

(define css-font-weight->integer : (-> Symbol (Option Integer))
  (lambda [weight]
    (font-weight->integer weight)))
