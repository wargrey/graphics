#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/version.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-version : (-> Integer) (lambda [] (cairo_version)))
(define pango-version : (-> Integer) (lambda [] (pango_version)))

(define cairo-version-string : (-> String) (lambda [] (cairo_version_string)))
(define pango-version-string : (-> String) (lambda [] (pango_version_string)))
