#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")

(require "../convert.rkt")
(require "../composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-append-position
  : (case-> [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum Geo-Layer Flonum Flonum -> Geo-Layer]
            [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum Geo<%> Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Layer])
  (case-lambda
    [(alignment width height layer xoff yoff)
     (geo-append-position alignment width height (vector-ref layer 0)
                          (+ (vector-ref layer 1) xoff) (+ (vector-ref layer 2) yoff)
                          (vector-ref layer 3) (vector-ref layer 4))]
    [(alignment width height geo maybe-x maybe-y swidth sheight)
     (define-values (dest-x dest-y)
       (case alignment
         [(vl) (values 0.0                      maybe-y)]
         [(vc) (values (* (- width swidth) 0.5) maybe-y)]
         [(vr) (values (- width swidth)         maybe-y)]
         [(ht) (values maybe-x                  0.0)]
         [(hc) (values maybe-x                  (* (- height sheight) 0.5))]
         [(hb) (values maybe-x                  (- height sheight))]
         [else #| deadcode |# (values maybe-x    maybe-y)]))
     (vector-immutable geo dest-x dest-y swidth sheight)]))

(define geo-superimpose-position
  : (case-> [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum -> (Values Flonum Flonum)]
            [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum Geo<%> Nonnegative-Flonum Nonnegative-Flonum -> Geo-Layer]
            [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum Geo-Layer -> Geo-Layer])
  (case-lambda
    [(port Width Height layer)
     (geo-superimpose-position port Width Height (vector-ref layer 0)
                               (vector-ref layer 3) (vector-ref layer 4))]
    [(port Width Height self width height)
     (let-values ([(x y) (geo-superimpose-position port Width Height width height)])
       (vector-immutable self x y width height))]
    [(port Width Height width height)
     (let*-values ([(rx by) (values (- Width width) (- Height height))]
                   [(cx cy) (values (* rx 0.5)  (* by 0.5))])
       (case port
         [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
         [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
         [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
         [else #| deadcode |# (values 0.0 0.0)]))]))
