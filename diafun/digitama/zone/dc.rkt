#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/self)
(require geofun/digitama/dc/resize)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require geofun/digitama/layer/combine)

(require "style.rkt")
(require "metadata.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-zone stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:zone make-zone) #:defaults ([make-zone #'dia:zone]))
              (~optional (~seq #:id id type) #:defaults ([id #'#false] [type #'#false]))
              (~optional (~seq #:options options) #:defaults ([options #'null]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false])))
        ...
        #:create-with caption style width height mask [make-shape shape-argl ...]
        zone-argl ...)
     (syntax/loc stx
       (let*-values ([(padding) (dia-zone-resolve-padding style width height mask #:padding margin)]
                     [(zwidth zheight offset x y rad) (dia-zone-stretch width height padding caption (cdr options))]
                     [(shape) (make-shape #:id (dia-zone-shape-id id type)
                                          #:stroke (dia-zone-resolve-stroke-paint style)
                                          #:fill (dia-zone-resolve-fill-paint style)
                                          zwidth zheight (dia-zone-resolve-corner-radius style)
                                          shape-argl ...)])
         (values (create-geometry-group make-zone id #false #false
                                        #:bleed (geo<%>-bleed shape)
                                        #:desc (geo-group-desc-from-caption #false)
                                        (cond [(not caption) (geo-own-layers shape)]
                                              [(not rad) (geo-composite-layers shape caption x y)]
                                              [else (geo-composite-layers shape (geo-rotate caption rad) x y)])
                                        type (car options) zone-argl ...)
                 offset)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Option-Zone (Option Dia:Zone))
(define-type Dia-Maybe-Zone (U Void Dia-Option-Zone))

(struct dia:zone geo:group
  ([type : (Option Symbol)]
   [stereotype : (Option Keyword)])
  #:type-name Dia:Zone
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-zone-stretch : (-> Nonnegative-Flonum Nonnegative-Flonum (Option Geo-Insets-Datum) (Option Geo) (Listof Any)
                               (Values Nonnegative-Flonum Nonnegative-Flonum Float-Complex Flonum Flonum (Option Flonum)))
  (lambda [content-width content-height padding caption options]
    (define-values (t r b l) (geo-inset-values padding))
    
    (if (or caption)
        (let-values ([(caption-width caption-height) (geo-flsize caption)])
          (define-values (Width Height) (values (max content-width caption-width) (max content-height caption-width))) ; yes, there is no typo here
          (define-values (dw dh) (values (- Width caption-width) (- Height caption-width))) ; yes, there is no typo here
          (define hcoff (min (* (-  content-width caption-width) 0.5) 0.0))
          (define vcoff (min (* (- content-height caption-width) 0.5) 0.0)) ; yes, there is no typo here
          
          (define-values (side pos sep stretch?)
            (let ([config (findf dz:dock? options)])
              (if (dz:dock? config)
                  (values (dz:dock-side config) (dz:dock-position config) (dz:dock-distance config) (dz:dock-stretch? config))
                  (values (default-dia-zone-side) (default-dia-zone-position) (default-dia-zone-distance) (default-dia-zone-stretch?)))))
          
          (define-values (width height xoff yoff wext hext cx cy rad)
            (cond [(eq? side 't)
                   (let ([headsep (~distance sep t)]
                         [cx (+ l (* dw pos))])
                     (if (>= headsep 0.0)
                         (if (or stretch?)
                             (values Width content-height hcoff (- headsep caption-height t) 0.0 (+ headsep caption-height) cx headsep #false)
                             (values Width content-height hcoff 0.0 0.0 0.0 cx headsep #false))
                         (values Width content-height hcoff (- headsep caption-height) 0.0 0.0 cx (- headsep caption-height) #false)))]
                  [(eq? side 'b)
                   (let ([tailsep (~distance sep b)]
                         [cx (+ l (* dw pos))]
                         [by (+ t content-height b)])
                     (if (>= tailsep 0.0)
                         (if (or stretch?)
                             (values Width content-height hcoff 0.0 0.0 (+ tailsep caption-height) cx by #false)
                             (values Width content-height hcoff 0.0 0.0 0.0 cx (- by tailsep caption-height) #false))
                         (values Width content-height hcoff 0.0 0.0 0.0 cx (- by tailsep) #false)))]
                  [(eq? side 'l)
                   (let ([headsep (~distance sep l)]
                         [cy (+ t (* dh pos))])
                     (if (>= headsep 0.0)
                         (if (or stretch?)
                             (values content-width Height (- headsep caption-height l) vcoff (+ headsep caption-height) 0.0 headsep cy -pi/2)
                             (values content-width Height 0.0 vcoff 0.0 0.0 headsep cy -pi/2))
                         (values content-width Height (- headsep caption-height) vcoff 0.0 0.0 (- headsep caption-height) cy -pi/2)))]
                  [(eq? side 'r)
                   (let ([tailsep (~distance sep r)]
                         [rx (+ l content-width r)]
                         [cy (+ t (* dh pos))])
                     (if (>= tailsep 0.0)
                         (if (or stretch?)
                             (values content-width Height 0.0 vcoff (+ tailsep caption-height) 0.0 rx cy pi/2)
                             (values content-width Height 0.0 vcoff 0.0 0.0 (- rx tailsep caption-height) cy pi/2))
                         (values content-width Height 0.0 vcoff 0.0 0.0 (- rx tailsep) cy pi/2)))]
                  [else '#:deadcode (values Width Height 0.0 0.0 0.0 0.0 0.0 0.0 #false)]))
          
          (values (+ l  width wext r)
                  (+ t height hext b)
                  (make-rectangular (- xoff l) (- yoff t))
                  cx cy rad))
        (values (+ l  content-width r)
                (+ t content-height b)
                (make-rectangular (- l) (- t))
                0.0 0.0 #false))))
