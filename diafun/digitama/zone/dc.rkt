#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/adapter)
(require geofun/digitama/layer/combine)

(require "style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-zone stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:zone make-zone) #:defaults ([make-zone #'dia:zone]))
              (~optional (~seq #:id id type stereotype) #:defaults ([id #'#false] [type #'#false] [stereotype #'#false]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false])))
        ...
        #:create-with style width height mask [make-shape shape-argl ...]
        zone-argl ...)
     (syntax/loc stx
       (let*-values ([(padding) (dia-zone-resolve-padding style width height mask #:padding margin)]
                     [(t r b l) (geo-inset-values padding)]
                     [(shape) (make-shape #:id (dia-zone-shape-id id type)
                                          #:stroke (dia-zone-resolve-stroke-paint style)
                                          #:fill (dia-zone-resolve-fill-paint style)
                                          (+ l width r) (+ t height b) shape-argl ...)])
         (values (create-geometry-group make-zone id #false #false
                                        #:bleed (geo<%>-bleed shape)
                                        #:desc (geo-group-desc-from-caption #false)
                                        (geo-dsfit-layers shape #false
                                                          lft% top% hfit% vfit%
                                                          sx% sy% (or tx% sx%) (or ty% sy%)
                                                          padding)
                                        type stereotype zone-argl ...)
                 (make-rectangular (- (geo-standard-insets-left padding))
                                   (- (geo-standard-insets-top padding))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Option-Zone (Option Dia:Zone))
(define-type Dia-Maybe-Zone (U Void Dia-Option-Zone))

(struct dia:zone geo:group
  ([type : (Option Symbol)]
   [stereotype : (Option Keyword)])
  #:type-name Dia:Zone
  #:transparent)
