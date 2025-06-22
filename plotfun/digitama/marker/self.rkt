#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/constant)

(require geofun/font)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/markup)

(require geofun/digitama/dc/text)
(require geofun/digitama/edge/tip/self)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/sticker)

(require "../axis/self.rkt")
(require "quirk.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Datum (U Plot:Mark Complex))
(define-type Plot-Mark->Description (-> Complex Any Font Color Plot-Position-Transform (U Geo-Sticker-Datum DC-Markup-Text False Void)))
(define-type Plot-Mark-Static-Description (U DC-Markup-Text Geo-Sticker-Datum))
(define-type Plot-Mark-Description (U Plot-Mark-Static-Description Plot-Mark->Description))

(define-struct plot:mark : Plot:Mark
  ([point : Complex 0]
   [desc : (Option Plot-Mark-Description) #false]
   [shape : (Option Geo-Tip) #false]
   [pin : (Option Float-Complex) #false]
   [gap : Float-Complex +nan.0+nan.0i]
   [rotate? : Boolean #false]
   [anchor : (Option Geo-Pin-Anchor) #false]
   [datum : Any #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-integer
  (lambda [#:datum [datum : Any #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-real]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-angle [p-angle : (Option Real) #false]
           #:pin-length [p-length : (Option Real) #false]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [x : Integer] [length : Real +nan.0] [angle : Real pi/2]] : Plot:Mark
    (make-plot:mark #:point x #:desc desc #:datum datum #:shape shape #:rotate? rotate?
                    #:gap (plot-mark-vector angle length)
                    #:pin (plot-mark-pin-vector pin? p-angle p-length))))

(define plot-real
  (lambda [#:datum [datum : Any #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-real]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-angle [p-angle : (Option Real) #false]
           #:pin-length [p-length : (Option Real) #false]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [x : Real] [length : Real +nan.0] [angle : Real pi/2]] : Plot:Mark
    (make-plot:mark #:point x #:desc desc #:datum datum #:shape shape #:rotate? rotate?
                    #:gap (plot-mark-vector angle length)
                    #:pin (plot-mark-pin-vector pin? p-angle p-length))))

(define plot-point
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-angle [p-angle : (Option Real) #false]
           #:pin-length [p-length : (Option Real) #false]
           [x : Complex] [length : Real 0.0] [angle : Real 0.0]] : Plot:Mark
    (make-plot:mark #:point x #:desc desc #:datum datum #:shape shape #:rotate? rotate?
                    #:gap (plot-mark-vector angle length)
                    #:pin (plot-mark-pin-vector pin? p-angle p-length))))

(define plot-label
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) #false]
           #:at [x : Complex]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-angle [p-angle : (Option Real) #false]
           #:pin-length [p-length : (Option Real) #false]
           [desc : Plot-Mark-Static-Description] [length : Real +nan.0] [angle : Real 0.0]] : Plot:Mark
    (make-plot:mark #:point x #:desc desc #:datum datum #:shape shape #:rotate? rotate?
                    #:gap (plot-mark-vector angle length)
                    #:pin (plot-mark-pin-vector pin? p-angle p-length))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-desc-real : Plot-Mark->Description
  (lambda [dot datum font color transform]
   (geo-text (real-part dot) font #:color color)))

(define plot-desc-point : Plot-Mark->Description
  (lambda [dot datum font color transform]
    (geo-text #:color color
              (format "(~a, ~a)" (real-part dot) (imag-part dot))
              font)))
