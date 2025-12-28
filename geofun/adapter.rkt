#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "digitama/self.rkt")
(require "digitama/composite.rkt")
(require "digitama/dc/composite.rkt")
(require "digitama/layer/combine.rkt")
(require "digitama/layer/adapter.rkt")
(require "digitama/geometry/spacing.rkt")

(require "resize.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-fit-composite
  (lambda [#:id [id : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [op : (Option Geo-Pin-Operator) #false]
           #:hfit% [hratio : Nonnegative-Real 1.0]
           #:vfit% [vratio : Nonnegative-Real 1.0]
           #:margin [margin : Geo-Spacing 0.0]
           [base : Geo] [bx% : Real] [by% : Real] [geo : (Option Geo)] [gx% : Real 0.5] [gy% : Real 0.5]] : Geo
    (cond [(not geo) base]
          [else (make-geo:group id base-op op
                                (geo-fit-layers base geo
                                                (real->double-flonum hratio) (real->double-flonum vratio)
                                                (real->double-flonum bx%) (real->double-flonum by%)
                                                (real->double-flonum gx%) (real->double-flonum gy%)
                                                margin))])))
