#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out geo-tip))
(provide Geo-Tip-Placement Geo-Tip-Shape Option-Geo-Tip Maybe-Geo-Tip)

(provide make-geo:tip:arrow default-arrow-tip default-generalization-tip)
(provide make-geo:tip:diamond default-aggregation-tip default-composition-tip)
(provide make-geo:tip:dot default-dot-tip default-odot-tip default-pixel-tip default-bullet-tip default-circle-tip)

(provide (rename-out [default-dot-tip default-point-tip]))

(require racket/case)

(require "tip/self.rkt")

(require "tip/dot.rkt")
(require "tip/arrow.rkt")
(require "tip/diamond.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-tip-filter : (-> Option-Geo-Tip (Option geo-tip))
  (lambda [self]
    (and self
         (cond [(geo-tip? self) self]
               [else (geo-tip-from-name self)]))))

(define geo-tip-from-name : (case-> [Geo-Tip-Shape -> geo-tip]
                                    [Symbol -> (Option geo-tip)])
  (lambda [name]
    (case/eq name
     [(arrow) default-arrow-tip]
     [(diamond) default-arrow-tip]
     [(bullet) default-bullet-tip]
     [(circle) default-circle-tip]
     [(triangle) default-aggregation-tip]
     [(dot point) default-dot-tip]
     [(odot) default-odot-tip]
     [(pixel) default-pixel-tip]
     [else #false])))
