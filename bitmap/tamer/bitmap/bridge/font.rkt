#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-font-weight->integer : (-> Symbol (Option Integer))
  (lambda [weight] ; enumerated integers mass the type system
    (font-weight->integer weight)))
