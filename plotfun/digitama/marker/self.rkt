#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/metrics)
(require digimon/constant)

(require geofun/font)

(require geofun/digitama/base)
(require geofun/digitama/dc/text)
(require geofun/digitama/paint/self)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/path/tip/self)
(require geofun/digitama/richtext/self)

(require "../axis/self.rkt")
(require "guard.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark->Description
  (-> Complex Any Font Color Plot-Position-Transform
      (U Geo-Sticker-Datum Geo-Rich-Text (Listof (U Geo-Sticker-Datum Geo-Rich-Text)) False Void)))

(define-type Plot-Mark-Datum (U Plot:Mark Complex))
(define-type Plot-Mark-Static-Description (U Geo-Rich-Text Geo-Sticker-Datum (Listof (U Geo-Sticker-Datum Geo-Rich-Text))))
(define-type Plot-Mark-Description (U Plot-Mark-Static-Description Plot-Mark->Description))

(define-struct plot:mark : Plot:Mark
  ([point : Complex 0]
   [desc : (Option Plot-Mark-Description) #false]
   [shape : (Option Geo-Tip) #false]
   [pin : (Option Plot-Mark-Vector) #false]
   [gap : Plot-Mark-Vector plot-mark-null-vector]
   [anchor : (Option Geo-Pin-Anchor) #false]
   [datum : Any #false]
   [debug? : (U Boolean Pen) #false])
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
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) -pi/2]
           #:gap-length [length : Real+% +nan.0] #:gap-angle [angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:debug? [debug? : (U Boolean Pen) #false]
           [x : Integer]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle)
                    #:debug? debug?)))

(define plot-real
  (lambda [#:datum [datum : Any #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-real]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) -pi/2]
           #:gap-length [length : Real+% +nan.0] #:gap-angle [angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:debug? [debug? : (U Boolean Pen) #false]
           [x : Real]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle)
                    #:debug? debug?)))

(define plot-point
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) 'dot]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false] #:pin-angle [p-angle : (Option Real) #false]
           #:gap-length [g-length : Real+% +nan.0] #:gap-angle [g-angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:debug? [debug? : (U Boolean Pen) #false]
           [x : Complex]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector g-length g-angle)
                    #:debug? debug?)))

(define plot-label
  (lambda [#:datum [datum : Any #false]
           #:shape [shape : (Option Geo-Tip) #false]
           #:at [x : Complex]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) '(100 %)] #:pin-angle [p-angle : (Option Real) +nan.0]
           #:gap-length [g-length : Real+% '(42 %)] #:gap-angle [g-angle : Real +nan.0]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:debug? [debug? : (U Boolean Pen) #false]
           [desc : Plot-Mark-Static-Description]] : Plot:Mark
    (make-plot:mark #:point x #:datum datum #:shape shape
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector g-length g-angle)
                    #:debug? debug?)))

(define plot-template
  (lambda [#:shape [shape : (Option Geo-Tip) 'dot]
           #:pin? [pin? : Boolean #true]
           #:pin-length [p-length : (Option Real+%) #false]
           #:pin-angle [p-angle : (Option Real) #false]
           #:anchor [anchor : (Option Geo-Pin-Anchor) #false]
           #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           #:debug? [debug? : (U Boolean Pen) #false]
           [length : Real+%] [angle : Real 0.0]] : Plot:Mark
    (make-plot:mark #:shape shape
                    #:desc desc #:anchor anchor
                    #:pin (plot-mark-pin-vector pin? p-length p-angle)
                    #:gap (plot-mark-vector length angle)
                    #:debug? debug?)))

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
