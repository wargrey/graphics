#lang typed/racket/base

(provide (all-defined-out))

(require "affine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-ink
  ([pos : Float-Complex]
   [width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum])
  #:constructor-name unsafe-geo-ink
  #:type-name Geo-Ink
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo-ink : (case-> [Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum -> Geo-Ink]
                               [Float-Complex Nonnegative-Flonum Nonnegative-Flonum -> Geo-Ink])
  (case-lambda
    [(x y width height) (make-geo-ink (make-rectangular x y) width height)]
    [(pos width height) (unsafe-geo-ink pos width height)]))

(define geo-ink-values : (-> Geo-Ink (Values Float-Complex Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (values (geo-ink-pos self)
            (geo-ink-width self)
            (geo-ink-height self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-ink-embolden : (->* (Geo-Ink Nonnegative-Flonum) (Boolean Boolean) Geo-Ink)
  (lambda [self thickness [xstroke? #true] [ystroke? #true]]
    (if (or xstroke? ystroke?)
        (let ([w (geo-ink-width self)]
              [h (geo-ink-height self)])
          (unsafe-geo-ink (geo-ink-pos self)
                          (if (or xstroke?) (+ w thickness) w)
                          (if (or ystroke?) (+ h thickness) h)))
        self)))
  
(define geo-ink-scale : (-> Geo-Ink Flonum Flonum Geo-Ink)
  (lambda [self sx sy]
    (define-values (ow oh) (values (geo-ink-width self) (geo-ink-height self)))
    (define-values (nw nh) (values (* (abs sx) (geo-ink-width self)) (* (abs sy) (geo-ink-height self))))
    (define-values (lx ty) (values (real-part (geo-ink-pos self)) (imag-part (geo-ink-pos self))))
    (define-values (rx by) (values (+ lx ow) (+ ty oh)))

    (unsafe-geo-ink (make-rectangular (if (>= sx 0.0) (* sx lx) (+ (* sx rx) nw))
                                      (if (>= sy 0.0) (* sy ty) (+ (* sy by) nh)))
                    nw nh)))

;;; TODO: this might not be correct.
(define geo-ink-rotate : (-> Geo-Ink Flonum Geo-Ink)
  (lambda [self theta.rad]
    (define-values (ox oy) (values (real-part (geo-ink-pos self)) (imag-part (geo-ink-pos self))))
    (define-values (ow oh) (values (geo-ink-width self) (geo-ink-height self)))
    (define-values (lx ty rx by) (geo-rectangle-rotate ow oh theta.rad))

    (unsafe-geo-ink (make-rectangular (max (+ lx ox) 0.0)
                                      (max (+ ty oy) 0.0))
                    (max (- rx lx) 0.0)
                    (max (- by ty) 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-null-ink : Geo-Ink (make-geo-ink 0.0 0.0 0.0 0.0))
