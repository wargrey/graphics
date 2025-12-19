#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require geofun/resize)

(require geofun/digitama/convert)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)

(require geofun/digitama/geometry/computation/line)

(require "style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-block stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:block Geo) #:defaults ([Geo #'dia:block]))
              (~optional (~seq #:id name) #:defaults ([name #'#false]))
              (~optional (~seq #:base-operator base-op) #:defaults ([base-op #'#false]))
              (~optional (~seq #:operator op) #:defaults ([op #'#false]))
              (~optional (~seq #:type type subtype) #:defaults ([type #''Customized] [subtype #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
              (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
              (~optional (~seq #:position wpos hpos (~optional (~seq lwpos lhpos)))
                         #:defaults ([wpos #'0.5] [hpos #'0.5] [lwpos #'0.5] [lhpos #'0.5]))) ...
        shape label argl ...)
     (syntax/loc stx
       (create-geometry-group Geo name base-op op #:outline (geo-outline shape)
                              (dia-block-layers label shape wratio hratio wpos hpos lwpos lhpos)
                              intersect type subtype
                              argl ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Block-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))

(struct dia:block geo:group
  ([intersect : Dia-Block-Intersect]
   [type : Symbol]
   [subtype : (Option Symbol)])
  #:type-name Dia:Block
  #:transparent)

(struct dia:block:label dia:block
  ([body : (Option String)])
  #:type-name Dia:Block:Label
  #:transparent)

(struct dia:block:polygon dia:block
  ([vertices : (List* Float-Complex Float-Complex (Listof Float-Complex))])
  #:type-name Dia:Block:Polygon
  #:transparent)

(struct dia:block:circle dia:block
  ([radius : Nonnegative-Flonum])
  #:type-name Dia:Block:Circle
  #:transparent)

(struct dia:block:ellipse dia:block
  ([a : Nonnegative-Flonum]
   [b : Nonnegative-Flonum])
  #:type-name Dia:Block:Ellipse
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-default-intersect : Dia-Block-Intersect
  (lambda [A B node-pos nlayer]
    (define-values (+ox +oy) (geo-layer-size nlayer 0.50))
    (define-values (-ox -oy) (values (- +ox) (- +oy)))

    (geo-line-polygon-intersect/first* #:translate node-pos A B
                                       (list (make-rectangular -ox -oy)
                                             (make-rectangular +ox -oy)
                                             (make-rectangular +ox +oy)
                                             (make-rectangular -ox +oy)))))

(define dia-polygon-intersect : Dia-Block-Intersect
  (lambda [A B node-pos nlayer]
    (define g (glayer-master nlayer))
    
    (and (dia:block:polygon? g)
         (let-values ([(ox oy) (geo-layer-size nlayer 0.5)])
           (define origin : Float-Complex (make-rectangular ox oy))
           
           (geo-line-polygon-intersect/first* #:translate (- node-pos origin) A B
                                              (dia:block:polygon-vertices g))))))

(define dia-circle-intersect : Dia-Block-Intersect
  (lambda [A B node-pos nlayer]
    (define g (glayer-master nlayer))
    
    (and (dia:block:circle? g)
         (let* ([V (- B A)]
                [d (magnitude V)]
                [r (dia:block:circle-radius g)])
           (and (>= d r)
                (if (= node-pos A)
                    (+ A (* V (/ r d)))
                    (- B (* V (/ r d)))))))))

(define dia-ellipse-intersect : Dia-Block-Intersect
  (lambda [A B node-pos nlayer]
    (define g (glayer-master nlayer))
    
    (and (dia:block:ellipse? g)
         (let* ([V (- B A)]
                [vx/a (/ (real-part V) (dia:block:ellipse-a g))]
                [vy/b (/ (imag-part V) (dia:block:ellipse-b g))]
                [t (/ 1.0 (magnitude (make-rectangular vx/a vy/b)))])
           (and (<= 0.0 t 1.0)
                (if (= node-pos A)
                    (+ A (* t V))
                    (- B (* t V))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-line-block-intersect : (-> (GLayerof Geo) Float-Complex Float-Complex Float-Complex (Option Float-Complex))
  (lambda [nlayer A B node-pos]
    (define g (glayer-master nlayer))

    (and (dia:block? g)
         ((dia:block-intersect g) A B node-pos nlayer))))

(define dia-block-layers : (-> (Option Geo) Geo Nonnegative-Flonum Nonnegative-Flonum
                               Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                               (GLayer-Groupof Geo))
  (lambda [label shape wratio hratio wpos hpos lwpos lhpos]
    (cond [(not label) (geo-own-layers shape)]
          [(or (nan? wratio) (nan? hratio)) (geo-composite-layers shape label wpos hpos lwpos lhpos)]
          [else (let ([fit-label (geo-fit label shape wratio hratio (default-dia-block-margin))])
                  (geo-composite-layers shape fit-label wpos hpos lwpos lhpos))])))
