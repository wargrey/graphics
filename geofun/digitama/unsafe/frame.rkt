#lang typed/racket/base

(provide (all-defined-out))

(require "../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_frame_size : (-> Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Option Stroke)
                            (Values Nonnegative-Flonum Nonnegative-Flonum
                                    Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                    Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [dest-width dest-height mtop mright mbottom mleft ptop pright pbottom pleft border]
    (define line-width (stroke-maybe-width border))
    (define line-inset (* line-width 0.5))
    (define-values (width  border-x border-width  dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    
    (values width height border-x border-y border-width border-height dest-x dest-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame-metrics : (-> Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [line-width line-inset flmopen flmclose flpopen flpclose size]
    (define border-position (+ flmopen line-inset))
    (define body-position (+ flmopen flpopen line-width))
    (define border-size (+ flpopen flpclose size line-width))
    (define frame-size (+ border-position border-size flmclose line-inset))
    
    (values frame-size border-position border-size body-position)))
