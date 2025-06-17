#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out Geo-Tip))
(provide Geo-Tip-Placement Option-Geo-Tip Maybe-Geo-Tip)

(provide make-geo:tip:diamond make-geo:tip:arrow)
(provide default-arrow-tip default-generalization-tip)
(provide default-aggregation-tip default-composition-tip)

(require "tip/self.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")
