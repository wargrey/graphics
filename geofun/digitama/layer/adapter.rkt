#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "type.rkt")
(require "combine.rkt")

(require "../self.rkt")
(require "../geometry/insets.rkt")
(require "../../resize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-try-dsfit-layers : (-> Geo (Option Geo)
                                   Flonum Flonum Flonum Flonum
                                   Flonum Flonum Flonum Flonum Geo-Insets-Datum
                                   (Option (GLayer-Groupof Geo)))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding]
    (and maybe-geo
         (>= hfit% 0.0) (<= hfit% 1.0)
         (>= vfit% 0.0) (<= vfit% 1.0)
         (let ([fit-label (geo-try-dsfit maybe-geo base hfit% vfit% padding)])
           (and fit-label
                (let*-values ([(bwidth bheight) (geo-flsize base)]
                              [(mtop mright mbottom mleft) (geo-inset-values padding)])
                  (geo-dsfit-composite-layers base fit-label bwidth bheight
                                              lft% top% hfit% vfit% fx% fy% gx% gy%
                                              mtop mright mbottom mleft)))))))

(define geo-dsfit-layers : (-> Geo (Option Geo)
                               Flonum Flonum Flonum Flonum
                               Flonum Flonum Flonum Flonum Geo-Insets-Datum
                               (GLayer-Groupof Geo))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding]
    (or (geo-try-dsfit-layers base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding)
        (geo-own-layers base))))

(define geo-try-dsfit-layers* : (-> Geo (Option Geo)
                                   Flonum Flonum Flonum Flonum
                                   Flonum Flonum Flonum Flonum Geo-Insets-Datum+%
                                   (Option (GLayer-Groupof Geo)))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding]
    (and maybe-geo
         (>= hfit% 0.0) (<= hfit% 1.0)
         (>= vfit% 0.0) (<= vfit% 1.0)
         (let ([fit-label (geo-try-dsfit maybe-geo base hfit% vfit% padding)])
           (and fit-label
                (let*-values ([(bwidth bheight) (geo-flsize base)]
                              [(mtop mright mbottom mleft) (geo-inset*-values padding bwidth)])
                  (geo-dsfit-composite-layers base fit-label bwidth bheight
                                              lft% top% hfit% vfit% fx% fy% gx% gy%
                                              mtop mright mbottom mleft)))))))

(define geo-dsfit-layers* : (-> Geo (Option Geo)
                               Flonum Flonum Flonum Flonum
                               Flonum Flonum Flonum Flonum Geo-Insets-Datum+%
                               (GLayer-Groupof Geo))
  (lambda [base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding]
    (or (geo-try-dsfit-layers* base maybe-geo lft% top% hfit% vfit% fx% fy% gx% gy% padding)
        (geo-own-layers base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dsfit-composite-layers : (-> Geo Geo 
                                         Nonnegative-Flonum Nonnegative-Flonum
                                         Flonum Flonum Flonum Flonum
                                         Flonum Flonum Flonum Flonum
                                         Nonnegative-Flonum Nonnegative-Flonum
                                         Nonnegative-Flonum Nonnegative-Flonum
                                         (GLayer-Groupof Geo))
  (lambda [base fit-label bwidth bheight lft% top% hfit% vfit% fx% fy% gx% gy% top right bottom left]
    (define-values (ml% mr%) (values (/ left bwidth) (/ right bwidth)))
    (define-values (mt% mb%) (values (/ top bheight) (/ bottom bheight)))
    (define l0% (* (- 1.0 hfit%) (if (rational? lft%) (~clamp lft% 0.0 1.0) 0.5)))
    (define t0% (* (- 1.0 vfit%) (if (rational? top%) (~clamp top% 0.0 1.0) 0.5)))

    (geo-composite-layers base fit-label
                          (+ l0% ml% (* (- hfit% ml% mr%) fx%))
                          (+ t0% mt% (* (- vfit% mt% mb%) fy%))
                          gx% gy%)))
