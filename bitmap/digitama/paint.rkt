#lang typed/racket/base

(provide (all-defined-out))

(require "misc.rkt")

;;; https://svgwg.org/svg2-draft/painting.html
;;; racket/draw/private/dc

(define solid-dash : (Vectorof Nonnegative-Flonum) '#())

(define-enumeration* stroke-dash-style #:as Stroke-Dash-Style 
  line-dash->array #:-> (Values (Vectorof Nonnegative-Flonum) Flonum)
  [(dot)           (values '#(1.0 2.0)         2.0)]
  [(dash)          (values '#(4.0 2.0)         2.0)]
  [(short-dash)    (values '#(2.0 2.0)         2.0)]
  [(dot-dash)      (values '#(1.0 2.0 4.0 2.0) 4.0)]
  [#:else #|none|# (values solid-dash          0.0)])

(define-enumeration* stroke-line-cap-option #:+> Stroke-Cap-Style ; order matters
  line-cap->integer integer->line-cap
  [0 butt round square])

(define-enumeration* stroke-line-join-option #:+> Stroke-Join-Style ; order matters
  line-join->integer integer->line-join
  [0 miter round bevel])

(define-enumeration* fill-rule-option #:+> Fill-Rule-Style ; order matters
  fill-rule->integer integer->fill-rule
  [0 nonzero evenodd])
