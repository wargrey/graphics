#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../paint/self.rkt")
(require "../geometry/sides.rkt")

(require "../unsafe/dc/border.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Frame-Datum (U Geo-Box Maybe-Fill-Paint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter geo-box : Geo-Box
  ([border : Maybe-Stroke-Paint #false]
   [background : Maybe-Fill-Paint #false]
   [margin : (Option Geo-Insets-Datum) #false]
   [padding : (Option Geo-Insets-Datum) #false]
   [open-sides : (Option Geo-Open-Sides) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-frame-values : (case-> [Geo-Frame-Datum -> (Values Maybe-Stroke-Paint Maybe-Fill-Paint
                                                               (Option Geo-Insets-Datum) (Option Geo-Insets-Datum)
                                                               (Option Geo-Open-Sides))]
                                   [Geo-Frame-Datum Nonnegative-Flonum Nonnegative-Flonum
                                                    -> (Values Maybe-Stroke-Paint Maybe-Fill-Paint
                                                               (Option Geo-Insets-Datum) (Option Geo-Insets-Datum)
                                                               (Option Geo-Border-Open-Sides))])
  (case-lambda
    [(self)
     (if (geo-box? self)
         (values (geo-box-border self) (geo-box-background self)
                 (geo-box-margin self) (geo-box-padding self)
                 (geo-box-open-sides self))
         (values #false self #false #false #false))]
    [(self width height)
     (if (geo-box? self)
         (values (geo-box-border self) (geo-box-background self)
                 (geo-box-margin self) (geo-box-padding self)
                 (geo-open-sides->border-sides (geo-box-open-sides self) width height))
         (values #false self #false #false #false))]))
