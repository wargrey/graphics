#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/struct)

(require geofun/digitama/convert)
(require geofun/digitama/markup)
(require geofun/digitama/edge/tip/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Datum (U Plot:Mark Real))
(define-type Plot-Mark-Dot->Description (-> Complex (U DC-Markup-Text Geo)))
(define-type Plot-Mark-Description (U DC-Markup-Text Geo Plot-Mark-Dot->Description))

(define-struct plot:mark : Plot:Mark
  ([dot : Complex 0]
   [angle : (Option Real) #false]
   [distance : (Option Real) #false]
   [desc : (Option Plot-Mark-Description) #false]
   [shape : (Option Geo-Tip) #false]
   [pin? : Boolean #false]
   [rotate? : Boolean #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-dot
  (lambda [#:shape [shape : (Option Geo-Tip) 'dot] #:desc [desc : (Option Plot-Mark-Description) plot-desc-point]
           #:radian? [radian? : Boolean #false] #:rotate? [rotate? : Boolean #false]
           [x : Complex] [angle : Real 0.0]] : Plot:Mark
    (make-plot:mark #:dot x
                    #:angle (and angle (~radian angle radian?)) #:distance #false
                    #:desc desc #:shape shape
                    #:pin? #false #:rotate? rotate?)))

(define plot-label
  (lambda [#:shape [shape : (Option Geo-Tip) #false]
           #:radian? [radian? : Boolean #false]
           #:pin? [pin? : Boolean #false] #:rotate? [rotate? : Boolean #false]
           [x : Complex] [desc : (U DC-Markup-Text Geo)] [angle : Real 0.0] [distance : (Option Real) #false]] : Plot:Mark
    (make-plot:mark #:dot x #:angle (and angle (~radian angle radian?)) #:distance distance
                    #:desc desc #:shape shape
                    #:pin? pin? #:rotate? rotate?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-desc-point : (-> Complex DC-Markup-Text)
  (lambda [dot]
    (format "(~a, ~a)"
      (real-part dot)
      (imag-part dot))))
