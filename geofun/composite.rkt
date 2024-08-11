#lang typed/racket/base

(provide (all-defined-out))
(provide geo-composite geo:group? Geo:Group)

(require "digitama/dc/composite.rkt")
(require "digitama/composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo:Group #:as geo-composite #:with 'over)
(define-pin geo-pin-under #:-> Geo:Group #:as geo-composite #:with 'dest-over)
