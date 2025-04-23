#lang typed/racket/base

(provide (all-defined-out))

(require "../../paint.rkt")
(require "../paint.rkt")
(require "../convert.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/ink.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")
(require "../geometry/footprint.rkt")

(require "../path/self.rkt")
(require "../unsafe/dc/path.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Print-Datum Point2D)
(define-type Geo-Path-Infobase (HashTable (Pairof Float-Complex Float-Complex) Geo:Path:Info))

(struct geo:path geo
  ([trail : Geo-Trail]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex]
   [footprints : Geo-Path-Prints]
   [foot-infos : Geo-Path-Infobase])
  #:type-name Geo:Path
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:path?])
      (define-values (width height pos) (geo-bbox-values (geo:path-bbox self)))
      (values width height (make-geo-ink pos width height)))))

(define geo-draw-path! : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Fill-Rule Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill frule]
    (λ [self cr x0 y0 width height]
      (when (geo:path? self)
        (define-values (xoff yoff) (geo-bbox-offset-values (geo:path-bbox self)))
        (dc_path cr (+ x0 xoff) (+ y0 yoff) width height (reverse (geo:path-footprints self))
                    (geo-select-stroke-paint* alt-stroke) (geo-select-fill-source alt-fill)
                    frule)))))
