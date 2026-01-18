#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/composite)

(require geofun/digitama/self)
(require geofun/digitama/dc/text)

(require "../block/style.rkt")
(require "../block/interface.rkt")
(require "../block/dc/node.rkt")

(require "parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) cls-block-interface : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key
                                (or (cls-block-caption caption (or stereotype 'Interface) style height)
                                    caption)
                                style width height direction 'Interface stereotype)))

(define #:forall (S) cls-block-class : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle/cr:8th block-key
                                (or (cls-block-caption caption stereotype style height)
                                    caption)
                                style width height direction 'Class stereotype)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) cls-block-caption : (-> (Option Geo) (Option Symbol) (Dia-Block-Style-Spec S) Nonnegative-Flonum (Option Geo))
  (lambda [caption stereotype style height]
    (and caption
         (or (symbol? stereotype)
             (pair? stereotype))
         (geo-vc-append #:gapsize (~dimension (default-cls-stereotype-gapsize) height)
                        (geo-text #:color (dia-block-resolve-font-paint style)
                                  (format "«~a»" (if (symbol? stereotype) stereotype (car stereotype)))
                                  (or (default-cls-stereotype-font)
                                      (dia-block-resolve-font style)))
                        caption))))
