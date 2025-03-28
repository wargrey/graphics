#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(unsafe-provide (rename-out [geo-append-layer unsafe-append-layer]
                            [geo-superimpose-layer unsafe-superimpose-layer]
                            [geo-superimpose-layer-make unsafe-superimpose-layer-make]))

(require "type.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-superimpose-layer-make : (-> Geo-Pin-Anchor G Nonnegative-Flonum Nonnegative-Flonum
                                                      (-> Nonnegative-Flonum Nonnegative-Flonum (GLayerof G)))
  (lambda [anchor sfc w h]
    (λ [[W : Nonnegative-Flonum] [H : Nonnegative-Flonum]] : (GLayerof G)
      (geo-superimpose-layer anchor W H sfc w h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-append-layer
  : (case-> [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) Flonum Flonum -> (GLayerof G)]
            [Geo-Append-Align Nonnegative-Flonum Nonnegative-Flonum G Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> (GLayerof G)])
  (case-lambda
    [(alignment width height layer xoff yoff)
     (geo-append-layer alignment width height (glayer-master layer)
                       (+ (glayer-x layer) xoff) (+ (glayer-y layer) yoff)
                       (glayer-width layer) (glayer-height layer))]
    [(alignment width height geo maybe-x maybe-y swidth sheight)
     (define-values (dest-x dest-y)
       (case alignment
         [(vl) (values 0.0                           maybe-y)]
         [(vc) (values (* (- width swidth) 0.5)      maybe-y)]
         [(vr) (values (- width swidth)              maybe-y)]
         [(v?) (values (* (- width swidth) (random)) maybe-y)]
         [(ht) (values maybe-x                       0.0)]
         [(hc) (values maybe-x                      (* (- height sheight) 0.5))]
         [(hb) (values maybe-x                      (- height sheight))]
         [(h?) (values maybe-x                      (* (- height sheight) (random)))]
         [else #;'deadcode (values maybe-x maybe-y)]))
     (glayer geo dest-x dest-y swidth sheight)]))

(define #:forall (G) geo-superimpose-layer
  : (case-> [Geo-Pin-Anchor Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum -> (Values Flonum Flonum)]
            [Geo-Pin-Anchor Nonnegative-Flonum Nonnegative-Flonum G Nonnegative-Flonum Nonnegative-Flonum -> (GLayerof G)]
            [Geo-Pin-Anchor Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) -> (GLayerof G)]
            [Geo-Pin-Anchor Nonnegative-Flonum Nonnegative-Flonum (GLayerof G) Flonum Flonum Any -> (GLayerof G)])
  (case-lambda
    [(anchor Width Height layer)
     (geo-superimpose-layer anchor Width Height (glayer-master layer) (glayer-width layer) (glayer-height layer))]
    [(anchor Width Height self xoff yoff _)
     (let*-values ([(width height) (values (glayer-width self) (glayer-height self))]
                   [(x y) (geo-superimpose-layer anchor Width Height width height)])
       (glayer (glayer-master self) (+ x xoff) (+ y yoff) width height))]
    [(anchor Width Height self width height)
     (let-values ([(x y) (geo-superimpose-layer anchor Width Height width height)])
       (glayer self x y width height))]
    [(anchor Width Height width height)
     (let*-values ([(rx by) (values (- Width width) (- Height height))]
                   [(cx cy) (values (* rx 0.5)  (* by 0.5))])
       (case anchor
         [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)] [(l?) (values 0.0 (* (random) by))]
         [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)] [(c?) (values  cx (* (random) by))]
         [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)] [(r?) (values  rx (* (random) by))]
         [(?t) (values (* (random) rx) 0.0)]
         [(?c) (values (* (random) rx) cy)]
         [(?b) (values (* (random) rx) by)]
         [else (values (* (random) rx) (* (random) by))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-own-pin-layer : (-> Geo-Pin-Anchor Float-Complex (∩ G Geo<%>) Float-Complex (GLayerof G))
  (lambda [anchor target self offset]
    (define-values (width height) (geo-flsize self))
    (define-values (dx dy) (geo-superimpose-layer anchor 0.0 0.0 width height))
    (define pos (+ target offset))

    (glayer self
            (+ (real-part pos) dx)
            (+ (imag-part pos) dy)
            width height)))
