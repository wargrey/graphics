#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../geometry/ink.rkt")
(require "../unsafe/dc/plain.rkt")
(require "../convert.rkt")
(require "../../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:blank geo
  ([body : (Option Geo)])
  #:type-name Geo:Blank
  #:transparent)

(struct geo:solid geo
  ([pattern : FlRGBA])
  #:type-name Geo:Solid
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-blank : (->* () (Real (Option Real+%) #:id (Option Symbol)) Geo:Blank)
  (let ([blank-db : (Weak-HashTable Any Geo:Blank) (make-weak-hash)])
    (lambda [[width 0.0] [height #false] #:id [id #false]]
      (define-values (flwidth flheight) (~extent* width height))
      (hash-ref! blank-db (cons flwidth flheight)
                 (λ [] (create-geometry-object geo:blank
                                               #:with [id void (geo-blank-extent flwidth flheight) geo-zero-pads]
                                               #false))))))

(define geo-ghost : (-> Geo [#:id (Option Symbol)] Geo:Blank)
  (lambda [geo #:id [id #false]]
    (define-values (flwidth flheight) (geo-flsize geo))
    (create-geometry-object geo:blank
                            #:with [id void geo-ghost-extent geo-zero-pads]
                            geo)))

(define geo-solid : (->* () (Color Real #:id (Option Symbol)) Geo:Solid)
  (lambda [[color transparent] [size 1] #:id [id #false]]
    (define edge-size : Nonnegative-Flonum (~length size))
    (create-geometry-object geo:solid
                            #:with [id geo-draw-solid (geo-shape-extent edge-size) geo-zero-pads]
                            (rgb* color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the size of target geometry might be affected by stroke and border
(define geo-ghost-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:blank?])
      (define-values (w h ?ink) (geo-extent (assert (geo:blank-body self))))
      (values w h geo-null-ink))))

(define geo-blank-extent : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Calculate-Extent)
  (lambda [flwidth flheight]
    (λ [self]
      (values flwidth flheight geo-null-ink))))

(define geo-draw-solid : Geo-Surface-Draw!
  (λ [self cr x0 y0 width height]
    (when (geo:solid? self)
      (dc_pattern cr x0 y0 width height (geo:solid-pattern self)))))
