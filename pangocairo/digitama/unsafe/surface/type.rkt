#lang typed/racket/base

(provide (all-defined-out))

(require "../visual/ctype.rkt")
(require "../source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Cairo-Surface-Create S) (-> Flonum Flonum Flonum Boolean (Values S Cairo-DC)))
(define-type (Cairo-Surface-Create+BG S) (-> Flonum Flonum Flonum (Option Fill-Source) Boolean (Values S Cairo-DC)))

(define-type (Cairo-Surface-Create* S)
  (case-> [Flonum Flonum Flonum Boolean -> (Values S Cairo-DC)]
          [Flonum Flonum Flonum (Option Fill-Source) Boolean -> (Values S Cairo-DC)]))
