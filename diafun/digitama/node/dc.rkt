#lang typed/racket/base

(provide (all-defined-out))

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
    [(_ (~seq #:id name)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        (~optional (~seq #:position wpos hpos) #:defaults ([wpos #'0.5] [hpos #'0.5]))
        shape label argl ...)
     (syntax/loc stx
       (create-dia-node dia:node #:id name #:intersect intersect
                        #:fit-ratio wratio hratio
                        #:position wpos hpos
                        shape label argl ...))]
    [(_ Geo (~seq #:id name)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        (~optional (~seq #:position wpos hpos) #:defaults ([wpos #'0.5] [hpos #'0.5]))
        shape label argl ...)
     (syntax/loc stx
       (let ([layers (cond [(not label) (geo-own-layers shape)]
                           [else (geo-composite-layers shape (geo-fit label shape wratio hratio (default-dia-node-margin))
                                                       wpos hpos 0.5 0.5)])])
         (Geo geo-convert geo-group-surface (geo-group-extent layers) name #false
              layers intersect argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Node-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))

(struct dia:node geo:group
  ([intersect : Dia-Node-Intersect])
  #:type-name Dia:Node
  #:transparent)

(struct dia:node:label dia:node ()
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
    (define g (vector-ref nlayer 0))

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
    (define g (vector-ref nlayer 0))
    
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
    (let ([g (vector-ref nlayer 0)])
      (and (dia:node? g)
           ((dia:node-intersect g) A B node-pos nlayer)))))
