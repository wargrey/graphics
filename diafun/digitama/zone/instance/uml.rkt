#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)

(require "../dc.rkt")
(require "../self.rkt")
(require "../style.rkt")
(require "../interface.rkt")

(require "../../block/dc/node.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type UML-Zone-Theme-Adjuster (#%Dia-Zone-Theme-Adjuster Dia-Zone-Style Dia-Zone-Metadata))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct uml-system-zone-style : UML-System-Zone-Style #:-> dia-zone-style #:for #%dia-zone-style
  ([padding : Dia-Zone-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'solid]
   [fill-paint : Maybe-Fill-Paint (void)]
   [corner-radius : (Option Length+%) 0.0]))

(define-phantom-struct uml-region-zone-style : UML-Region-Zone-Style #:-> dia-zone-style #:for #%dia-zone-style
  ([padding : Dia-Zone-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'long-dash]
   [fill-paint : Maybe-Fill-Paint (void)]
   [corner-radius : (Option Length+%) (&L 0.618 'em)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-uml-zone-identify : (Dia-Zone-Identifier Dia-Zone-Style)
  (lambda [id type sotype]
    (cond [(eq? type 'system) (default-uml-system-zone-style)]
          [(eq? type 'region) (default-uml-region-zone-style)])))

(define #:forall (S) default-uml-zone-build : (Dia-Zone-Builder S)
  (lambda [id type title style width height options mask]
    (define sotype : (Option Keyword) (car options))
    (define caption : (Option Geo)
      (if (or sotype)
          (let ([stereotype (dia-block-stereotype sotype
                                                  (dia-zone-resolve-font style)
                                                  (dia-zone-resolve-font-paint style)
                                                  #false +inf.0)])
            (uml-title-attach-stereotype title stereotype))
          title))

    (define-values (zone offset)
      (create-dia-zone #:zone dia:zone
                       #:id id type
                       #:options options
                       #:create-with caption style width height mask
                       (geo-rounded-rectangle)))

    (cons zone offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uml-title-attach-stereotype : (-> (Option Geo) (Option Geo) (Option Geo))
  (lambda [title stereotype]
    (cond [(and title stereotype) (geo-vc-append stereotype title)]
          [(and title) title]
          [else stereotype])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uml-zone-factory : Dia-Zone-Factory
  (make-dia-zone-factory #:identifier default-uml-zone-identify
                         #:builder default-uml-zone-build))
