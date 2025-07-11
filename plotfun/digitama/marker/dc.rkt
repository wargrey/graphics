#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/color)
(require geofun/digitama/markup)
(require geofun/digitama/edge/label)

(require geofun/digitama/dc/edge)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)

(require geofun/digitama/geometry/footprint)

(require "self.rkt")
(require "guard.rkt")
(require "anchor.rkt")
(require "../axis/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:marker geo:labeled-edge
  ([self : Plot:Mark])
  #:type-name Plot:Marker
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-marker
  (lambda [#:id [id : (Option Symbol) #false]
           #:font [font : (Option Font) #false] #:color [color : (Option Color) #false] #:pin-stroke [pin-stroke : Maybe-Stroke-Paint (void)]
           #:fallback-pin [fallback-pin : (Option Plot-Mark-Fallback-Vector) #false] #:fallback-gap [fallback-gap : (Option Plot-Mark-Fallback-Vector) #false]
           #:fallback-anchor [fallback-anchor : Plot-Mark-Auto-Anchor plot-mark-auto-anchor] #:length-base [length-base : Nonnegative-Flonum 100.0]
           [self : Plot:Mark] [transform : Plot-Position-Transform]] : Plot:Marker
    (define label : (Option Geo-Sticker-Datum)
      (let ([desc (plot:mark-desc self)])
        (cond [(geo? desc) desc]
              [(geo-sticker? desc) desc]
              [(dc-markup-text? desc) (geo-edge-label-text desc #false font color)]
              [else (let ([desc.geo (and desc (desc (plot:mark-point self) (plot:mark-datum self)
                                                    (or font (default-font)) (or color hilite)
                                                    transform))])
                      (cond [(geo? desc.geo) desc.geo]
                            [(geo-sticker? desc.geo) desc.geo]
                            [(dc-markup-text? desc.geo) (geo-edge-label-text desc.geo #false font color)]
                            [else #false]))])))

    (define-values (prints location angle) (plot-mark->footprints self transform length-base fallback-pin fallback-gap))
    
    (define edge : Geo:Edge
      (geo-edge* #:id id #:stroke pin-stroke
                 #:source-tip (plot:mark-shape self) #:target-tip #false
                 #:tip-placement 'center #:tip-color color
                 prints))

    (define label-layer
      (and label
           (geo-sticker->layer #:default-anchor (or (plot:mark-anchor self)
                                                    (fallback-anchor location angle))
                               label (- location (geo:edge-origin edge)))))
    
    (create-geometry-group plot:marker id #false #false
                           (cond [(not label-layer) (geo-own-layers edge)]
                                 [else (geo-path-layers-merge (geo-own-layers edge)
                                                              (list label-layer))])
                           self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WARNING: It's better for callers to ensure valid inputs as specifications vary across visualizers. 
(define plot-mark->footprints : (-> Plot:Mark Plot-Position-Transform Nonnegative-Flonum
                                    (Option Plot-Mark-Fallback-Vector) (Option Plot-Mark-Fallback-Vector)
                                    (Values Geo-Path-Clean-Prints+ Float-Complex (Option Flonum)))
  (lambda [self transform length-base fallback-pin fallback-gap]
    (define pt : Complex (plot:mark-point self))
    (define-values (x y)
      (values (real->double-flonum (real-part pt))
              (real->double-flonum (imag-part pt))))

    (define start : Float-Complex (transform x y))
    (define-values (pin pin.rad) (plot-mark-vector-guard (plot:mark-pin self) fallback-pin length-base x))
    (define-values (gap gap.rad) (plot-mark-vector-guard (plot:mark-gap self) fallback-gap length-base x))

    (cond [(and pin gap)
           (let* ([pin-end (+ start   pin)]
                  [gap-end (+ pin-end gap)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\L pin-end)
                           (gpp:point #\M gap-end))
                     gap-end
                     (or gap.rad pin.rad)))]
          [(or pin)
           (let ([pin-end (+ start pin)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\L pin-end))
                     pin-end
                     (or gap.rad pin.rad)))]
          [(or gap)
           (let ([gap-end (+ start gap)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\M gap-end))
                     gap-end
                     (or gap.rad pin.rad)))]
          [else (values (list (gpp:point #\M start))
                        start
                        (or gap.rad pin.rad))])))
