#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require "metadata.rkt")
(require "trail.rkt")
(require "anchor.rkt")

(require "../self.rkt")
(require "../paint.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../richtext/self.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/ink.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/footprint.rkt")

(require "../unsafe/dc/path.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Print-Datum Point2D)
(define-type Geo-Track-Infobase (HashTable (Pairof Float-Complex Float-Complex) Geo:Track:Info))

(define current-flex-zone : (Parameterof (Option Geo:Track:Zone:Flex)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:track:zone
  ([id : Symbol]
   [type : Symbol]
   [stereotype : (Option Keyword)]
   [desc : Geo-Rich-Text])
  #:type-name Geo:Track:Zone
  #:transparent)

(struct geo:track:zone:fixed geo:track:zone
  ([grid-span : Float-Complex])
  #:type-name Geo:Track:Zone:Fixed
  #:transparent)

(struct geo:track:zone:flex geo:track:zone
  ([caption-anchor : Geo-Pin-Anchor]
   [anchors : (Listof Geo-Anchor-Name)]
   [children : (Listof Geo:Track:Zone:Flex)])
  #:type-name Geo:Track:Zone:Flex
  #:transparent
  #:mutable)

(struct geo:track geo
  ([trail : Geo-Trail]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex]
   [footprints : Geo-Path-Prints]
   [foot-infos : Geo-Track-Infobase]
   [zones : (Listof Geo:Track:Zone:Flex)]
   [stickers : (Listof (Pairof Geo-Sticker-Datum Float-Complex))])
  #:type-name Geo:Track
  #:transparent
  #:mutable)

(define geo-track-try-fit! : (case-> [Geo:Track Float-Complex -> Void]
                                     [Geo:Track (Option Geo-Anchor-Name) Float-Complex -> Void]
                                     [Geo:Track (Option Geo-Anchor-Name) Float-Complex Float-Complex -> Void]
                                     [Geo:Track (Option Geo-Anchor-Name) Float-Complex Float-Complex Float-Complex -> Void])
  (case-lambda
    [(self pt) (geo-bbox-fit! (geo:track-bbox self) pt)]
    [(self anchor pt)
     (cond [(not anchor) (geo-track-try-fit! self pt)]
           [else (geo-trail-set! (geo:track-trail self) anchor pt)
                 (geo-current-zone-try-push-anchor! anchor)])]
    [(self anchor pt ctrl)
     (geo-track-try-fit! self anchor pt)
     (geo-bbox-fit! (geo:track-bbox self) ctrl)]
    [(self anchor pt ctrl1 ctrl2)
     (geo-track-try-fit! self anchor pt)
     (geo-bbox-fit! (geo:track-bbox self) ctrl1)
     (geo-bbox-fit! (geo:track-bbox self) ctrl2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-create-flex-zone! : (-> Geo:Track Symbol Symbol Geo-Option-Rich-Text Geo-Pin-Anchor (Option Keyword) Geo:Track:Zone:Flex)
  (lambda [master id type desc anchor keyword]
    (define maybe-zone (geo-find-flex-zone master id))

    (when (and maybe-zone)
      (raise-user-error 'geo-create-flex-zone! "duplicate zone name: ~a" id))

    (define parent-zone (current-flex-zone))
    (define self-zone
      (geo:track:zone:flex id type keyword
                           (or desc (symbol->immutable-string id))
                           anchor null null))

    (if (not parent-zone)
        (set-geo:track-zones! master (cons self-zone (geo:track-zones master)))
        (set-geo:track:zone:flex-children! parent-zone (cons self-zone (geo:track:zone:flex-children parent-zone))))
    
    self-zone))

(define geo-flex-zone-ref : (-> Geo:Track Symbol Geo:Track:Zone:Flex)
  (lambda [master id]
    (define the-zone (geo-find-flex-zone master id))

    (when (not the-zone)
      (raise-user-error 'geo-extend-flex-zone! "no such a zone: ~a" id))
    
    the-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-find-flex-zone : (-> Geo:Track Symbol (Option Geo:Track:Zone:Flex))
  (lambda [master id]
    (let find ([zones : (Listof Geo:Track:Zone:Flex) (geo:track-zones master)])
      (and (pair? zones)
           (let subfind ([self : Geo:Track:Zone:Flex (car zones)])
             (cond [(eq? (geo:track:zone-id self) id) self]
                   [else (or (ormap subfind (geo:track:zone:flex-children self))
                             (find (cdr zones)))]))))))

(define geo-current-zone-try-push-anchor! : (-> Geo-Anchor-Name Void)
  (lambda [anchor]
    (define the-zone (current-flex-zone))

    (when (and the-zone)
      (define anchors (geo:track:zone:flex-anchors the-zone))
      (unless (memq anchor anchors)
        (set-geo:track:zone:flex-anchors! the-zone (cons anchor anchors))))))

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
