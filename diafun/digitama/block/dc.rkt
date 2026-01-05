#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/adapter)

(require geofun/digitama/geometry/computation/line)

(require "style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-block stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:block make-block) #:defaults ([make-block #'dia:block]))
              (~optional (~seq #:id name) #:defaults ([name #'#false]))
              (~optional (~seq #:type type subtype) #:defaults ([type #''Customized] [subtype #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false])))
        ...
        #:create-with style [make-shape shape-argl ...]
        block-argl ...)
     (syntax/loc stx
       (let ([shape (make-shape #:id (dia-block-shape-id name)
                                #:stroke (dia-block-resolve-stroke-paint style)
                                #:fill (dia-block-resolve-fill-paint style)
                                shape-argl ...)])
         (create-dia-block #:block make-block #:id name
                           #:type type subtype #:intersect intersect
                           #:fit-region hfit% vfit% lft% top%
                           #:alignment sx% sy% tx% ty%
                           #:with shape block-argl ...)))]
    [(_ (~alt (~optional (~seq #:block make-block) #:defaults ([make-block #'dia:block]))
              (~optional (~seq #:id name) #:defaults ([name #'#false]))
              (~optional (~seq #:type type subtype) #:defaults ([type #''Customized] [subtype #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false])))
        ...
        #:with shape maybe-caption extra-argl ...)
     (syntax/loc stx
       (create-geometry-group make-block name #false #false
                              #:outline (geo-outline shape)
                              #:desc (dia-block-desc-from-caption maybe-caption)
                              (geo-dsfit-layers shape maybe-caption
                                                lft% top% hfit% vfit%
                                                sx% sy% (or tx% sx%) (or ty% sy%)
                                                (default-dia-block-margin))
                              (dia-block-tags type subtype)
                              intersect extra-argl ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Block-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))

(struct dia:block geo:group
  ([tags : (Pairof Symbol (Listof Symbol))]
   [intersect : Dia-Block-Intersect])
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
(define dia:block-type : (-> Dia:Block Symbol)
  (lambda [self]
    (car (dia:block-tags self))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-tags : (-> Symbol (Option Symbol) (Pairof Symbol (Listof Symbol)))
  (lambda [type subtype]
    (cond [(not subtype) (list type)]
          [else (list type subtype)])))

(define dia-block-desc-from-caption : (-> (Option Geo) (Option String))
  (lambda [maybe-caption]
    (and maybe-caption
         (cond [(geo:string? maybe-caption) (geo:string-body maybe-caption)]
               [(geo:group? maybe-caption) (geo:group-desc maybe-caption)]
               [else #false]))))
