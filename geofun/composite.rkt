#lang typed/racket/base

(provide (all-defined-out))
(provide geo-composite geo:group? Geo:Group)

(require "digitama/dc/composite.rkt")
(require "digitama/composite.rkt")
(require "digitama/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo<%> Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo<%> Geo:Group #:as geo-composite #:with 'dest-over #:id)
