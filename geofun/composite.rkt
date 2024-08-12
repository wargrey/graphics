#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo-frame geo:frame? Geo:Frame)

(provide (rename-out [geo-pin-over geo-pin]))

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")

(require "digitama/composite.rkt")
(require "digitama/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo<%> Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo<%> Geo:Group #:as geo-composite #:with 'dest-over #:id)
