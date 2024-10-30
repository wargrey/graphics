#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/typed/more.rkt")
(require "digitama/unsafe/typed/pango.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For being used in untyped code

(define cairo-version : (-> Index) (λ [] (cairo_version)))
(define pango-version : (-> Index) (λ [] (pango_version)))

(define cairo-version-string : (-> String) (λ [] (cairo_version_string)))
(define pango-version-string : (-> String) (λ [] (cairo_version_string)))
