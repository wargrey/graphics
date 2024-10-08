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
(define-syntax (create-dia-node stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~seq #:id name) (~seq #:type type subtype)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        (~optional (~seq #:position wpos hpos) #:defaults ([wpos #'0.5] [hpos #'0.5]))
        shape label argl ...)
     (syntax/loc stx
       (create-dia-node dia:node
                        #:id name #:type type subtype
                        #:intersect intersect
                        #:fit-ratio wratio hratio
                        #:position wpos hpos
                        shape label argl ...))]
    [(_ Geo (~seq #:id name) (~seq #:type type subtype)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        (~optional (~seq #:position wpos hpos) #:defaults ([wpos #'0.5] [hpos #'0.5]))
        shape label argl ...)
     (syntax/loc stx
       (let ([layers (dia-node-layers label shape wratio hratio wpos hpos)])
         (Geo geo-convert geo-group-surface (geo-group-extent layers)
              name #false layers #false
              intersect type subtype
              argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Node-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))

(struct dia:node geo:group
  ([intersect : Dia-Node-Intersect]
   [type : Symbol]
   [subtype : (Option Symbol)])
  #:type-name Dia:Node
  #:transparent)

(struct dia:node:label dia:node
  ([body : (Option String)])
  #:type-name Dia:Node:Label
  #:transparent)

(struct dia:node:polygon dia:node
  ([vertices : (List* Float-Complex Float-Complex (Listof Float-Complex))])
  #:type-name Dia:Node:Polygon
  #:transparent)

(struct dia:node:circle dia:node
  ([radius : Nonnegative-Flonum])
  #:type-name Dia:Node:Circle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-default-intersect : Dia-Node-Intersect
  (lambda [A B node-pos nlayer]
    (define-values (+ox +oy) (geo-layer-size nlayer 0.50))
    (define-values (-ox -oy) (values (- +ox) (- +oy)))

    (geo-line-polygon-intersect/first* #:translate node-pos A B
                                       (list (make-rectangular -ox -oy)
                                             (make-rectangular +ox -oy)
                                             (make-rectangular +ox +oy)
                                             (make-rectangular -ox +oy)))))

(define dia-polygon-intersect : Dia-Node-Intersect
  (lambda [A B node-pos nlayer]
    (define g (glayer-master nlayer))

    ;;; NOTE
    ; We don't deal with stroke here,
    ; the node size might be larger then the actual bounding box of the polygon,
    ; and decorative cap styles might cause some pixels of the arrow overlapping nodes
    ; when the node has transparent fill.
    
    (and (dia:node:polygon? g)
         (let-values ([(ox oy) (geo-layer-size nlayer 0.5)])
           (define origin : Float-Complex (make-rectangular ox oy))
           
           (geo-line-polygon-intersect/first* #:translate (- node-pos origin) A B
                                              (dia:node:polygon-vertices g))))))

(define dia-circle-intersect : Dia-Node-Intersect
  (lambda [A B node-pos nlayer]
    (define g (glayer-master nlayer))
    
    (and (dia:node:circle? g)
         (let* ([V (- B A)]
                [d (magnitude V)]
                [r (dia:node:circle-radius g)])
           (and (>= d r)
                (if (= node-pos A)
                    (+ A (* V (/ r d)))
                    (- B (* V (/ r d)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-line-node-intersect : (-> (GLayerof Geo) Float-Complex Float-Complex Float-Complex (Option Float-Complex))
  (lambda [nlayer A B node-pos]
    (define g (glayer-master nlayer))

    (and (dia:node? g)
         ((dia:node-intersect g) A B node-pos nlayer))))

(define dia-node-layers : (-> (Option Geo) Geo Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (GLayer-Groupof Geo))
  (lambda [label shape wratio hratio wpos hpos]
    (cond [(not label) (geo-own-layers shape)]
          [(or (nan? wratio) (nan? hratio)) (geo-composite-layers shape label wpos hpos 0.5 0.5)]
          [else (let ([fit-label (geo-fit label shape wratio hratio (default-dia-node-margin))])
                  (geo-composite-layers shape fit-label wpos hpos 0.5 0.5))])))
