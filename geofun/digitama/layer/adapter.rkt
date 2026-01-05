#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "combine.rkt")

(require "../self.rkt")
(require "../geometry/spacing.rkt")
(require "../../resize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-try-dsfit-layers : (-> Geo (Option Geo)
                                   Flonum Flonum Flonum Flonum
                                   Flonum Flonum Flonum Flonum Geo-Spacing
                                   (Option (GLayer-Groupof Geo)))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% spacing]
    (and maybe-geo
         (>= hfit% 0.0) (<= hfit% 1.0)
         (>= vfit% 0.0) (<= vfit% 1.0)
         (let ([fit-label (geo-try-dsfit maybe-geo base hfit% vfit% spacing)])
           (and fit-label
                (let*-values ([(mtop mright mbottom mleft) (geo-spacing-values spacing)]
                              [(bwidth bheight) (geo-flsize base)]
                              [(ml% mr%) (values (/ mleft bwidth) (/ mright bwidth))]
                              [(mt% mb%) (values (/ mtop bheight) (/ mbottom bheight))]
                              [(l0%) (if (rational? lft%) (max (min (- 1.0 hfit%) lft%) 0.0) (* (- 1.0 hfit%) 0.5))]
                              [(t0%) (if (rational? top%) (max (min (- 1.0 vfit%) top%) 0.0) (* (- 1.0 vfit%) 0.5))])
                  (geo-composite-layers base fit-label
                                        (+ l0% ml% (* (- hfit% ml% mr%) fx%))
                                        (+ t0% mt% (* (- vfit% mt% mb%) fy%))
                                        gx% gy%)))))))

(define geo-dsfit-layers : (-> Geo (Option Geo)
                               Flonum Flonum Flonum Flonum
                               Flonum Flonum Flonum Flonum Geo-Spacing
                               (GLayer-Groupof Geo))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% spacing]
    (or (geo-try-dsfit-layers base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% spacing)
        (geo-own-layers base))))
