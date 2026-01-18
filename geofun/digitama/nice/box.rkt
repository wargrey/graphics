#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../paint/self.rkt")
(require "../geometry/insets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Frame-Datum (U Geo-Box Maybe-Fill-Paint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter geo-box : Geo-Box
  ([border : Maybe-Stroke-Paint #false]
   [background : Maybe-Fill-Paint #false]
   [margin : (Option Geo-Insets-Datum) #false]
   [padding : (Option Geo-Insets-Datum) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-frame-values : (-> Geo-Frame-Datum
                               (Values Maybe-Stroke-Paint Maybe-Fill-Paint
                                       (Option Geo-Insets-Datum) (Option Geo-Insets-Datum)))
  (lambda [self]
    (if (geo-box? self)
        (values (geo-box-border self) (geo-box-background self)
                (geo-box-margin self) (geo-box-padding self))
        (values #false self #false #false))))
