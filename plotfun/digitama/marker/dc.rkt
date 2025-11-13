#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/color)
(require geofun/digitama/markup)
(require geofun/digitama/path/label)
(require geofun/digitama/paint/self)

(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/merge)

(require geofun/digitama/geometry/footprint)

(require "self.rkt")
(require "guard.rkt")
(require "anchor.rkt")
(require "../axis/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:marker geo:path
  ([self : Plot:Mark])
  #:type-name Plot:Marker
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-marker
  (lambda [#:id [id : (Option Symbol) #false]
           #:font [font : (Option Font) #false] #:color [color : (Option Color) #false] #:pin-stroke [pin-stroke : Maybe-Stroke-Paint (void)]
           #:fallback-pin [fallback-pin : (Option Plot-Mark-Fallback-Vector) #false] #:fallback-gap [fallback-gap : (Option Plot-Mark-Fallback-Vector) #false]
           #:fallback-anchor [fallback-anchor : Plot-Mark-Auto-Anchor plot-mark-auto-anchor] #:length-base [length-base : Nonnegative-Flonum 100.0]
           #:rotate [rotate : Real 0.0] #:debug? [debug0? : Boolean #false]
           [self : Plot:Mark] [transform0 : (Option Plot-Position-Transform) #false]] : Plot:Marker
    (define transform : Plot-Position-Transform (or transform0 plot-mark-values))
    (define maybe-debugger : (U Boolean Pen) (plot:mark-debug? self))
    (define debug? : Boolean (or debug0? (and maybe-debugger #true)))
    (define labels : (Listof Geo-Sticker-Datum)
      (let sticker-filter ([desc (plot:mark-desc self)])
        (cond [(geo? desc) (list desc)]
              [(geo-sticker? desc) (list desc)]
              [(dc-markup-text? desc) (list (geo-path-label-text desc #false font color))]
              [(list? desc) (apply append (map sticker-filter desc))]
              [(not desc) null]
              [else (let ([desc.geo (desc (plot:mark-point self) (plot:mark-datum self)
                                          (or font (default-font)) (or color hilite)
                                          transform)])
                      (cond [(void? desc.geo) null]
                            [else (sticker-filter desc.geo)]))])))

    (define-values (prints location angle)
      (plot-mark->footprints self transform length-base
                             fallback-pin fallback-gap
                             (real->double-flonum rotate)))
    
    (define master : Geo:Path:Self
      (geo-path* #:id id
                 #:stroke (cond [(not debug?) pin-stroke]
                                [(pen? maybe-debugger) maybe-debugger]
                                [else plot-mark-debugger])
                 #:source-tip (or (plot:mark-shape self) (and debug? 'dot))
                 #:target-tip (and debug? 'arrow)
                 #:tip-placement 'center #:tip-color color
                 prints))

    (define label-layers
      (for/list : (Listof (GLayerof Geo)) ([label (in-list labels)])
        (geo-sticker->layer #:default-anchor (or (plot:mark-anchor self)
                                                 (fallback-anchor location angle))
                            label (- location (geo-path-self-pin-position master)))))
    
    (create-geometry-group plot:marker id #false #false
                           (cond [(null? label-layers) (geo-own-layers master)]
                                 [else (geo-layers-merge (geo-own-layers master) label-layers)])
                           master self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WARNING: It's better for callers to ensure valid inputs as specifications vary across visualizers. 
(define plot-mark->footprints : (-> Plot:Mark Plot-Position-Transform Nonnegative-Flonum
                                    (Option Plot-Mark-Fallback-Vector) (Option Plot-Mark-Fallback-Vector) Flonum
                                    (Values Geo-Path-Clean-Prints+ Float-Complex (Option Flonum)))
  (lambda [self transform length-base fallback-pin fallback-gap rotate]
    (define pt : Complex (plot:mark-point self))
    (define-values (x y)
      (values (real->double-flonum (real-part pt))
              (real->double-flonum (imag-part pt))))

    (define start : Float-Complex (transform x y))
    (define-values (pin pin.rad) (plot-mark-vector-guard (plot:mark-pin self) fallback-pin length-base x rotate))
    (define-values (gap gap.rad) (plot-mark-vector-guard (plot:mark-gap self) fallback-gap length-base x rotate))

    (cond [(and pin gap)
           (let* ([pin-end (+ start   pin)]
                  [gap-end (+ pin-end gap)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\L pin-end)
                           (gpp:point #\M gap-end))
                     gap-end gap.rad))]
          [(or pin)
           (let ([pin-end (+ start pin)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\L pin-end))
                     pin-end pin.rad))]
          [(or gap)
           (let ([gap-end (+ start gap)])
             (values (list (gpp:point #\M start)
                           (gpp:point #\M gap-end))
                     gap-end gap.rad))]
          [else (values (list (gpp:point #\M start))
                        start (or gap.rad pin.rad))])))

(define plot-mark-values : Plot-Position-Transform
  (case-lambda
    [(x y) (make-rectangular x y)]
    [(pt) pt]))

(define plot-mark-debugger : Pen
  (desc-stroke #:color 'firebrick))
