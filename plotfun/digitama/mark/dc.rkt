#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require racket/math)
(require racket/match)

(require geofun/paint)
(require geofun/font)

(require geofun/digitama/convert)
(require geofun/digitama/color)
(require geofun/digitama/markup)
(require geofun/digitama/edge/label)

(require geofun/digitama/dc/edge)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)

(require geofun/digitama/geometry/footprint)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:marker geo:group
  ([self : Plot:Mark])
  #:type-name Plot:Marker
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-marker : (->* (Plot:Mark)
                           (#:id (Option Symbol) #:pin-stroke Maybe-Stroke-Paint
                            #:font (Option Font) #:font-paint Option-Fill-Paint)
                           Plot:Marker)
  (lambda [self #:id [id #false] #:font [font #false] #:font-paint [font-paint #false] #:pin-stroke [pin-stroke (void)]]
    (define pos : Complex (plot:mark-dot self))
    (define-values (start end) (values (- pos 1.0+0.0i) (+ 20+20i 1.0+0.0i)))
    
    (define edge : Geo:Edge
      (geo-edge #:id id
                #:stroke pin-stroke
                #:source-tip (plot:mark-shape self)
                #:target-tip #false
                #:tip-placement 'center
                (list pos (+ pos 20+20i))))
    
    (define label : (Option Geo-Edge-Label)
      (and (plot:mark-desc self)
           (let* ([desc (plot:mark-desc self)]
                  [desc (cond [(geo? desc) desc] [(dc-markup-text? desc) desc] [else (desc pos)])]
                  [desc (if (geo? desc) desc (geo-edge-label-text desc #false font font-paint))])
             (make-geo-edge-label #:rotate? (plot:mark-rotate? self)
                                  start end desc))))

    (create-geometry-group plot:marker id #false #false
                           (if (or label)
                               (let ([label-layer (geo-edge-label-layer label (geo:edge-origin edge))])
                                 (geo-path-layers-merge (geo-own-layers edge) (list label-layer)))
                               (geo-own-layers edge))
                           self)))

(plot-marker (plot-dot 2+3i #:rotate? #true))
