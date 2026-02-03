#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/font)
(require geofun/digitama/self)
(require geofun/digitama/dc/dingbat)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require "self.rkt")
(require "../../presets.rkt")

(require "../../block/dc.rkt")
(require "../../block/dc/node.rkt")
(require "../../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-uml-note-build : Dia-Note-Builder
  (lambda [key body style padding direction sotype]
    (define stereotype (and sotype (dia-block-stereotype sotype style dia-preset-note-tag-font-tweak)))
    (define-values (stwidth stheight) (if (not stereotype) (values 0.0 0.0) (geo-size stereotype)))
    (define-values (bdwidth bdheight) (geo-size body))
    (define-values (top rgt bot lft) (geo-inset-values padding))
    (define dog-ear : Nonnegative-Flonum (+ top stheight))
    (define width : Nonnegative-Flonum (+ lft (max (+ stwidth dog-ear) bdwidth) rgt))
    (define body-top : Nonnegative-Flonum (if (not stereotype) (max top dog-ear) (+ top dog-ear)))
    (define height : Nonnegative-Flonum (+ body-top bdheight bot))

    (define node : Geo
      (geo-file #:dog-ear-size dog-ear
                #:stroke (dia-block-resolve-stroke-paint style)
                #:fill (dia-block-resolve-fill-paint style)
                width height))
    
    (create-dia-block #:block dia:block:note
                      #:id key sotype
                      #:with-group style
                      (geo-composite (cond [(not stereotype) node]
                                           [else (geo-composite node (* (- width dog-ear) 0.5) top
                                                                stereotype (* stwidth 0.5) 0.0)])
                                     lft body-top body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uml-note-factory : Dia-Note-Factory (make-dia-note-factory #:builder default-uml-note-build))
