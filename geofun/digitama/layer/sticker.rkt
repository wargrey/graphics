#lang typed/racket/base

(provide (all-defined-out))

(require "../convert.rkt")

(require "type.rkt")
(require "position.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-sticker
  ([self : Geo]
   [anchor : Geo-Pin-Anchor]
   [offset : Float-Complex])
  #:type-name Geo-Sticker
  #:transparent)

(define make-sticker : (case-> [Geo -> Geo-Sticker]
                               [Geo Geo-Pin-Anchor -> Geo-Sticker]
                               [Geo Geo-Pin-Anchor Complex -> Geo-Sticker]
                               [Geo Geo-Pin-Anchor Real Real -> Geo-Sticker])
  (case-lambda
    [(self) (geo-sticker self 'cc 0.0+0.0i)]
    [(self anchor) (geo-sticker self anchor 0.0+0.0i)]
    [(self anchor offset)
     (if (real? offset)
         (geo-sticker self anchor (make-rectangular (real->double-flonum offset) 0.0))
         (geo-sticker self anchor (make-rectangular (real->double-flonum (real-part offset))
                                                    (real->double-flonum (imag-part offset)))))]
    [(self anchor dx dy)
     (geo-sticker self anchor
                  (make-rectangular (real->double-flonum dx)
                                    (real->double-flonum dy)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Sticker-Datum (U Geo-Sticker Geo))

(define geo-sticker-datum? : (-> Any Boolean : Geo-Sticker-Datum)
  (lambda [maybe]
    (or (geo-sticker? maybe)
        (geo? maybe))))

(define geo-sticker->layer : (->* (Geo-Sticker-Datum Float-Complex)
                                  (Float-Complex #:default-anchor (Option Geo-Pin-Anchor))
                                  (GLayerof Geo))
  (lambda [self pos [offset 0.0+0.0i] #:default-anchor [default-anchor #false]]
    (if (geo? self)
        (geo-own-pin-layer (or default-anchor 'cc) pos self offset)
        (geo-own-pin-layer (geo-sticker-anchor self) pos (geo-sticker-self self)
                           (+ (geo-sticker-offset self) offset)))))
