#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/self.rkt")
(require "digitama/composite.rkt")
(require "digitama/dc/composite.rkt")
(require "digitama/layer/adapter.rkt")
(require "digitama/geometry/insets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dsfit-composite
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [op : (Option Geo-Pin-Operator) #false]
           #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
           #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
           #:padding [padding : Geo-Insets-Datum+% 0.0]
           [base : Geo] [bx% : Real] [by% : Real] [icon : (Option Geo)] [gx% : Real 0.5] [gy% : Real 0.5]] : Geo
    (define maybe-containted
      (geo-try-dsfit-layers* base icon
                             (real->double-flonum left%) (real->double-flonum top%)
                             (real->double-flonum hfit%) (real->double-flonum vfit%)
                             (real->double-flonum bx%) (real->double-flonum by%)
                             (real->double-flonum gx%) (real->double-flonum gy%)
                             padding))
    (or (and maybe-containted
             (make-geo:group id base-op op desc maybe-containted))
        base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (geo-lt-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.0 0.0 icon 0.0 0.0))

(define (geo-lc-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.0 0.5 icon 0.0 0.5))

(define (geo-lb-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.0 1.0 icon 0.0 1.0))

(define (geo-ct-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.5 0.0 icon 0.5 0.0))

(define (geo-cc-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.5 0.5 icon 0.5 0.5))

(define (geo-cb-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 0.5 1.0 icon 0.5 1.0))

(define (geo-rt-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 1.0 0.0 icon 1.0 0.0))

(define (geo-rc-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 1.0 0.5 icon 1.0 0.5))

(define (geo-rb-stamp #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                      #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                      #:hfit% [hfit% : Real 1.0] #:vfit% [vfit% : Real 1.0]
                      #:top% [top% : Real +nan.0] #:left% [left% : Real +nan.0]
                      #:padding [padding : Geo-Insets-Datum+% 0.0]
                      [base : Geo] [icon : Geo]) : Geo
  (geo-dsfit-composite #:id id #:desc desc #:base-operator base-op #:operator sibs-op
                       #:hfit% hfit% #:vfit% vfit% #:top% top% #:left% left% #:padding padding
                       base 1.0 1.0 icon 1.0 1.0))
