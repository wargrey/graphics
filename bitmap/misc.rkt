#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/base.rkt")
(require "digitama/unsafe/resize.rkt")
(require "digitama/unsafe/convert.rkt")
(require "digitama/unsafe/version.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-alter-density : (->* (Bitmap) (Positive-Flonum) Bitmap)
  (lambda [src [dest-density (default-bitmap-density)]]
    (define density : Positive-Flonum (bitmap-density src))
    (cond [(= density dest-density) src]
          [else (let ([s (/ density dest-density)])
                  (bitmap_scale (bitmap-surface src)
                                s s dest-density))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-version : (-> Integer) (lambda [] (cairo_version)))
(define pango-version : (-> Integer) (lambda [] (pango_version)))

(define cairo-version-string : (-> String) (lambda [] (cairo_version_string)))
(define pango-version-string : (-> String) (lambda [] (pango_version_string)))
