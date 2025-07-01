#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)
(require digimon/constant)

(require geofun/font)

(require geofun/digitama/base)
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
   [pin : (Option FlComplex+%) #false]
   [gap : FlComplex+% +nan.0+nan.0i]
   [rotate? : Boolean #false]
   [anchor : (Option Geo-Pin-Anchor) #false]
   [datum : Any #false])
  #:transparent)

(define plot-mark-sync-with-template : (-> Plot:Mark (U False Plot:Mark Plot-Mark-Description) Plot:Mark)
  (lambda [self template]
    (cond [(not template) self]
          [(plot:mark? template) (remake-plot:mark template #:point (plot:mark-point self) #:datum (plot:mark-datum self))]
          [(eq? (plot:mark-desc self) template) self]
          [else (remake-plot:mark self #:desc template)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-integer
  (lambda [#:datum [datum : Any #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-real]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) -pi/2]
           #:gap-length [length : Real+% +nan.0] #:gap-angle [angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [x : Integer]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape #:rotate? rotate?
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle))))

(define plot-real
  (lambda [#:datum [datum : Any #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-real]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) -pi/2]
           #:gap-length [length : Real+% +nan.0] #:gap-angle [angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [x : Real]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape #:rotate? rotate?
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle))))

(define plot-point
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) #false]
           #:gap-length [g-length : Real+% +nan.0] #:gap-angle [g-angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [x : Complex]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape #:rotate? rotate?
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector g-length g-angle))))

(define plot-label
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) #false]
           #:at [x : Complex]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) +nan.0]
           #:gap-length [g-length : Real+% +nan.0] #:gap-angle [g-angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           [desc : Plot-Mark-Static-Description]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape #:rotate? rotate?
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector g-length g-angle))))

(define plot-template
  (lambda [#:shape [shape : (Option Geo-Tip) 'dot]
           #:rotate? [rotate? : Boolean #false]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false]
           #:pin-angle [p-angle : (Option Real) #false]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           [length : Real+%] [angle : Real 0.0]] : Plot:Mark
    (make-plot:mark #:shape shape #:rotate? rotate?
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-desc-real : Plot-Mark->Description
  (lambda [dot datum font color transform]
   (geo-text #:color color
             (or datum (real-part dot)) font)))

(define plot-desc-point : Plot-Mark->Description
  (lambda [dot datum font color transform]
    (geo-text #:color color
              (or datum (format "(~a, ~a)" (real-part dot) (imag-part dot)))
              font)))
