#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(unsafe-provide (rename-out [geo-append-layer unsafe-append-layer]
                            [geo-superimpose-layer unsafe-superimpose-layer]))

(require "type.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-append-layer
  : (case-> [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) Flonum Flonum -> (GLayerof G)]
            [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum G Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> (GLayerof G)])
  (case-lambda
    [(alignment width height layer xoff yoff)
     (geo-append-layer alignment width height (vector-ref layer 0)
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

(define #:forall (G) geo-superimpose-layer
  : (case-> [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum -> (Values Flonum Flonum)]
            [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum G Nonnegative-Flonum Nonnegative-Flonum -> (GLayerof G)]
            [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) -> (GLayerof G)]
            [Geo-Pin-Port Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) Flonum Flonum Any -> (GLayerof G)])
  (case-lambda
    [(port Width Height layer)
     (geo-superimpose-layer port Width Height (vector-ref layer 0) (vector-ref layer 3) (vector-ref layer 4))]
    [(port Width Height self xoff yoff _)
     (let*-values ([(width height) (values (vector-ref self 3) (vector-ref self 4))]
                   [(x y) (geo-superimpose-layer port Width Height width height)])
       (vector-immutable (vector-ref self 0) (+ x xoff) (+ y yoff) width height))]
    [(port Width Height self width height)
     (let-values ([(x y) (geo-superimpose-layer port Width Height width height)])
       (vector-immutable self x y width height))]
    [(port Width Height width height)
     (let*-values ([(rx by) (values (- Width width) (- Height height))]
                   [(cx cy) (values (* rx 0.5)  (* by 0.5))])
       (case port
         [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
         [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
         [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
         [else #| deadcode |# (values 0.0 0.0)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-own-layer : (-> Geo-Pin-Port Float-Complex (∩ G Geo<%>) Float-Complex (GLayerof G))
  (lambda [port target self offset]
    (define-values (width height) (geo-flsize self))
    (define-values (dx dy) (geo-superimpose-layer port 0.0 0.0 width height))
    (define pos (+ target offset))

    (vector-immutable self
                      (+ (real-part pos) dx)
                      (+ (imag-part pos) dy)
                      width height)))
