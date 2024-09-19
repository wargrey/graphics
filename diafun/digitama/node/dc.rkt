#lang typed/racket/base

(provide (all-defined-out))

(require geofun/resize)

(require geofun/digitama/convert)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)

(require geofun/digitama/geometry/computation/line)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-node stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~seq #:id name)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        shape label argl ...)
     (syntax/loc stx
       (create-dia-node dia:node #:id name #:intersect intersect #:fit-ratio wratio hratio
                        shape label argl ...))]
    [(_ Geo (~seq #:id name)
        (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
        (~optional (~seq #:fit-ratio wratio hratio) #:defaults ([wratio #'1.0] [hratio #'1.0]))
        shape label argl ...)
     (syntax/loc stx
       (let ([layers (cond [(not label) (geo-own-layers shape)]
                           [else (geo-composite-layers shape (geo-fit label shape wratio hratio)
                                                       0.5 0.5 0.5 0.5)])])
         (Geo geo-convert geo-group-surface (geo-group-extent layers) name #false
              layers intersect argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Node-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))

(struct dia:node geo:group
  ([intersect : Dia-Node-Intersect])
  #:type-name Dia:Node
  #:transparent)

(struct dia:node:polygon dia:node
  ([vertices : (Listof Float-Complex)])
  #:type-name Dia:Node:Polygon
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-default-intersect : Dia-Node-Intersect
  (lambda [A B N nlayer]
    (define-values (+ox +oy) (geo-layer-size nlayer 0.50))
    (define-values (-ox -oy) (values (- +ox) (- +oy)))

    (geo-line-polygon-intersect/first #:translate N A B
                                      (list (make-rectangular -ox -oy)
                                            (make-rectangular +ox -oy)
                                            (make-rectangular +ox +oy)
                                            (make-rectangular -ox +oy)))))

(define dia-polygon-intersect : Dia-Node-Intersect
  (lambda [A B N nlayer]
    (define g (vector-ref nlayer 0))

    ;;; NOTE
    ; We don't deal with stroke here,
    ; the node size might be larger then the actual bounding box of the polygon,
    ; and decorative cap styles might cause some pixels of the arrow overlapping nodes
    ; when the node has transparent fill.
    
    (and (dia:node:polygon? g)
         (let-values ([(ox oy) (geo-layer-size nlayer 0.5)])
           (define origin : Float-Complex (make-rectangular ox oy))

           (geo-line-polygon-intersect/first #:translate (- N origin) A B
                                             (dia:node:polygon-vertices g))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-line-node-intersect : (-> (GLayerof Geo) Float-Complex Float-Complex Float-Complex (Option Float-Complex))
  (lambda [nlayer A B N]
    (let ([g (vector-ref nlayer 0)])
      (and (dia:node? g)
           ((dia:node-intersect g) A B N nlayer)))))
