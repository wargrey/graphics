#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../paint.rkt")
(require "../layer/type.rkt")
(require "../richtext/self.rkt")

(require "../track/self.rkt")
(require "../track/trail.rkt")
(require "../track/anchor.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/ink.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/footprint.rkt")

(require "../unsafe/dc/path.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Print-Datum Point2D)
(define-type Geo-Track-Infobase (HashTable (Pairof Float-Complex Float-Complex) Geo:Track:Info))

(struct geo:track:zone
  ([grid-pos : Float-Complex]
   [grid-span : Float-Complex]
   [desc : Geo-Rich-Text]
   [type : Symbol])
  #:type-name Geo:Track:Zone
  #:transparent)

(struct geo:track:zone:fixed geo:track:zone
  ()
  #:type-name Geo:Track:Zone:Fixed
  #:transparent)

(struct geo:track:zone:flex geo:track:zone
  ([anchor : Geo-Pin-Anchor])
  #:type-name Geo:Track:Zone:Flex
  #:transparent)

(struct geo:track geo
  ([trail : Geo-Trail]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex]
   [footprints : Geo-Path-Prints]
   [foot-infos : Geo-Track-Infobase]
   [zones : (Listof Geo:Track:Zone)])
  #:type-name Geo:Track
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:track?])
      (define-values (width height pos) (geo-bbox-values (geo:track-bbox self)))
      (values width height (make-geo-ink pos width height)))))

(define geo-draw-track! : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:track? self)
        (define-values (xoff yoff) (geo-bbox-offset-values (geo:track-bbox self)))
        (dc_path cr (+ x0 xoff) (+ y0 yoff) width height (reverse (geo:track-footprints self))
                    (geo-select-stroke-paint* alt-stroke)
                    (geo-select-fill-source alt-fill))))))
