#lang typed/racket/base

(provide (all-defined-out))
(provide geo:transform? geo:region? Geo:Transform Geo:Region)
(provide geo-scale geo:scaling? Geo:Scaling)
(provide geo-rotate geo:rotation? Geo:Rotation)

(require "digitama/convert.rkt")
(require "digitama/resize.rkt")

(require "digitama/dc/resize.rkt")
(require "digitama/geometry/ink.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-section : (case-> [Geo Complex Complex -> Geo]
                              [Geo Complex Real Real -> Geo]
                              [Geo Real Real Real Real -> Geo])
  (case-lambda
    [(self pt0 ptn) (let ([delta (- ptn pt0)]) (geo-section self (real-part pt0) (imag-part pt0) (real-part delta) (imag-part delta)))]
    [(self pt width height) (geo-section self (real-part pt) (imag-part pt) width height)]
    [(self x y width height)
     (let-values ([(src-width src-height) (geo-flsize self)])
       (cond [(and (zero? x) (zero? y) (= width src-width) (= height src-height)) self]
             [else (make-geo:region self x y (min (max width 0.0) src-width) (min (max height 0.0) src-height))]))]))

(define geo-copy : (case-> [Geo -> Geo:Region]
                           [Geo Complex Real Real -> Geo:Region]
                           [Geo Real Real Real Real -> Geo:Region])
  (case-lambda
    [(self) (let-values ([(src-width src-height) (geo-flsize self)]) (geo-copy self 0.0 0.0 src-width src-height))]
    [(self pt width height) (geo-copy self (real-part pt) (imag-part pt) width height)]
    [(self x y width height)
     (let-values ([(src-width src-height) (geo-flsize self)])
       (make-geo:region self x y (min (max width 0.0) src-width) (min (max height 0.0) src-height)))]))

(define geo-inset : (case-> [Geo -> Geo]
                            [Geo Real -> Geo:Region]
                            [Geo Real Real -> Geo:Region]
                            [Geo Real Real Real Real -> Geo:Region])
  (case-lambda
    [(self inset) (geo-inset self inset inset inset inset)]
    [(self vertical horizontal) (geo-inset self vertical horizontal vertical horizontal)]
    [(self top right bottom left)
     (let-values ([(flwidth flheight) (geo-flsize self)])
       (make-geo:region self (- left) (- top) (max (+ left flwidth right) 0.0) (max (+ top flheight bottom) 0.0)))]
    [(self)
     (let-values ([(flw flh) (geo-flsize self)])
       (cond [(= flw flh) self]
             [(< flw flh) (geo-inset self 0.0 (* (- flh flw) 0.5))]
             [else (geo-inset self (* (- flw flh) 0.5) 0.0)]))]))

(define geo-bounding-box : (-> Geo (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self]
    (define-values (_w _h ink) (geo-extent* self))
    (define-values (pos width height) (geo-ink-values ink))
    
    ;;; All geo vector graphics are accommodated in bounded surfaces,
    ;;; the positions of the bounding boxes are non-negative.

    (values (max (real-part pos) 0.0)
            (max (imag-part pos) 0.0)
            width height)))

(define geo-trim : (-> Geo Geo)
  (lambda [self]
    (define-values (_w _h ink) (geo-extent* self))
    (define-values (pos width height) (geo-ink-values ink))
    
    (geo-section self (real-part pos) (imag-part pos) width height)))

(define-cropper geo-crop : (-> Geo Nonnegative-Real Nonnegative-Real Geo)
  (#:lambda [self width height left% top%]
   (define-values (W H) (geo-flsize self))
   (define w (min W (real->double-flonum width)))
   (define h (min H (real->double-flonum height)))
   (geo-section self (* (- W w) left%) (* (- H h) top%) w h))
  #:with ("geo-~a-crop" [lt 0.0 0.0] [lc 0.0 0.5] [lb 0.0 1.0]
                        [ct 0.5 0.0] [cc 0.5 0.5] [cb 0.5 1.0]
                        [rt 1.0 0.0] [rc 1.0 0.5] [rb 1.0 1.0]))

(define geo-resize : (case-> [Geo (U Geo Nonnegative-Real) -> Geo]
                             [Geo Nonnegative-Real Nonnegative-Real -> Geo])
  (case-lambda
    [(self refer)
     (if (not (real? refer))
         (let-values ([(rw rh) (geo-flsize refer)]) (geo-resize self rw rh))
         (let-values ([(w h) (geo-flsize self)]) (geo-scale self (/ refer (min w h)))))]
    [(self w h)
     (let-values ([(flwidth flheight) (geo-flsize self)])
       (geo-scale self
                  (if (zero? w) 1.0 (/ w flwidth))
                  (if (zero? h) 1.0 (/ h flheight))))]))

(define geo-fit : (case-> [Geo Geo -> Geo]
                          [Geo Nonnegative-Real Nonnegative-Real -> Geo]
                          [Geo Geo Nonnegative-Real Nonnegative-Real -> Geo]
                          [Geo Geo Nonnegative-Real Nonnegative-Real Nonnegative-Real -> Geo]
                          [Geo Geo Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real -> Geo])
  (case-lambda
    [(self refer) (geo-fit self refer 1.0 1.0)]
    [(self refer wratio hratio) (geo-fit self refer wratio hratio 0.0 0.0)]
    [(self refer wratio hratio pad) (geo-fit self refer wratio hratio pad pad)]
    [(self refer wratio hratio wpad hpad)
     (let-values ([(flwidth flheight) (geo-flsize refer)])
       (geo-fit self
                (max (- (* flwidth  (real->double-flonum wratio)) (real->double-flonum wpad)) 0.0)
                (max (- (* flheight (real->double-flonum hratio)) (real->double-flonum hpad)) 0.0)))]
    [(self width height)
     (let-values ([(flwidth flheight) (geo-flsize self)])
       (cond [(and (positive? width) (positive? height))
              (geo-scale self
                         (min (/ (min flwidth  (real->double-flonum width))  flwidth)
                              (/ (min flheight (real->double-flonum height)) flheight)))]
             [(positive? width)  (geo-scale self (/ (min flwidth  (real->double-flonum width))  flwidth))]
             [(positive? height) (geo-scale self (/ (min flheight (real->double-flonum height)) flheight))]
             [else self]))]))
    