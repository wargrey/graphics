#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require "../dc.rkt")
(require "../self.rkt")
(require "../style.rkt")
(require "../interface.rkt")

(require "../../block/dc.rkt")
(require "../../block/dc/node.rkt")
(require "../../block/style.rkt")

(require "../../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-uml-zone-build : (Dia-Zone-Builder S)
  (lambda [id type title style width height sotype mask]
    (define stereotype : (Option Geo)
      (and sotype (dia-block-stereotype sotype
                                        (dia-zone-resolve-font style)
                                        (dia-zone-resolve-font-paint style)
                                        dia-preset-note-tag-font-tweak +inf.0)))
    
    (define-values (bdwidth bdheight) (if (not title) (values 0.0 0.0) (geo-size title)))
    (define-values (stwidth stheight) (if (not stereotype) (values 0.0 0.0) (geo-size stereotype)))

    (define-values (zone offset)
      (create-dia-zone #:zone dia:zone
                       #:id id type sotype
                       #:create-with style width height mask
                       (geo-rectangle)))

    (cons zone offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uml-zone-factory : Dia-Zone-Factory (make-dia-zone-factory #:builder default-uml-zone-build))
