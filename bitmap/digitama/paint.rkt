#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)
(require typed/racket/unsafe)

(require "misc.rkt")

(unsafe-provide (rename-out [line-cap->integer linecap->integer]))
(unsafe-provide (rename-out [line-join->integer linejoin->integer]))

;;; https://svgwg.org/svg2-draft/painting.html
;;; racket/draw/private/dc

(define solid-dash : (Vectorof Nonnegative-Flonum) '#())

(define-enumeration* stroke-dash-style #:as Stroke-Dash-Style 
  line-dash->array #:-> [linewidth Nonnegative-Flonum] (Values Flonum (Vectorof Nonnegative-Flonum))
  [(dot)           (values 2.0 (dasharray-normalize #(1.0 2.0) linewidth))]
  [(dot-dash)      (values 4.0 (dasharray-normalize #(1.0 2.0 4.0 2.0) linewidth))]
  [(short-dash)    (values 2.0 (dasharray-normalize #(2.0 2.0) linewidth))]
  [(long-dash)     (values 2.0 (dasharray-normalize #(4.0 2.0) linewidth))]
  [#:else #|none|# (values 0.0 solid-dash)])

(define-enumeration* stroke-line-cap-option #:+> Stroke-Cap-Style ; order matters
  line-cap->integer integer->line-cap
  [0 butt round square])

(define-enumeration* stroke-line-join-option #:+> Stroke-Join-Style ; order matters
  line-join->integer integer->line-join
  [0 miter round bevel])

(define-enumeration* fill-rule-option #:+> Fill-Rule-Style ; order matters
  fill-rule->integer integer->fill-rule
  [0 nonzero evenodd])

(define dasharray-normalize : (-> (Vectorof Nonnegative-Flonum) Nonnegative-Flonum (Vectorof Nonnegative-Flonum))
  (lambda [dasharray linewidth]
    (cond [(fl= linewidth 1.0) dasharray]
          [else (for/vector : (Vectorof Nonnegative-Flonum) ([dash (in-vector dasharray)])
                  (fl* dash linewidth))])))
