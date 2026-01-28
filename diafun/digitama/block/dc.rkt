#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/richtext/self)

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
              (~optional (~seq #:id name tags) #:defaults ([name #'#false] [tags #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false])))
        ...
        #:create-with style [make-shape shape-argl ...]
        block-argl ...)
     (syntax/loc stx
       (let ([shape (make-shape #:id (dia-block-shape-id name)
                                #:stroke (dia-block-resolve-stroke-paint style)
                                #:fill (dia-block-resolve-fill-paint style)
                                shape-argl ...)])
         (create-dia-block #:block make-block #:id name tags
                           #:intersect intersect
                           #:fit-region hfit% vfit% lft% top%
                           #:alignment sx% sy% tx% ty%
                           #:margin margin
                           #:with style shape block-argl ...)))]
    [(_ (~alt (~optional (~seq #:block make-block) #:defaults ([make-block #'dia:block]))
              (~optional (~seq #:id name tags) #:defaults ([name #'#false] [tags #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false])))
        ...
        #:with style shape maybe-caption extra-argl ...)
     (syntax/loc stx
       (create-geometry-group make-block name #false #false
                              #:outline (geo-outline shape)
                              #:desc (dia-block-desc-from-caption maybe-caption)
                              (geo-dsfit-layers shape maybe-caption
                                                lft% top% hfit% vfit%
                                                sx% sy% (or tx% sx%) (or ty% sy%)
                                                (dia-block-resolve-padding style #:padding margin))
                              (dia-block-style-type-object style)
                              (dia-block-tags tags)
                              intersect extra-argl ...))]
    [(_ (~alt (~optional (~seq #:block make-block) #:defaults ([make-block #'dia:block]))
              (~optional (~seq #:id name tags) #:defaults ([name #'#false] [tags #'#false]))
              (~optional (~seq #:intersect intersect) #:defaults ([intersect #'dia-default-intersect])))
        ...
        #:with-group style shape:expr extra-argl ...)
     (syntax/loc stx
       (let ([g shape])
         (with-asserts ([g geo:group?])
           (create-geometry-group make-block (or name (geo-id g)) #false #false
                                  #:outline (geo-outline g)
                                  #:desc (geo:group-desc g)
                                  (geo:group-selves g)
                                  (dia-block-style-type-object style)
                                  (dia-block-tags tags)
                                  intersect extra-argl ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Block-Intersect (-> Float-Complex Float-Complex Float-Complex (GLayerof Geo) (Option Float-Complex)))
(define-type Dia-Option-Block (Option Dia:Block))
(define-type Dia-Maybe-Block (U Void Dia-Option-Block))

(struct dia:block geo:group
  ([phantom-type : Any]
   [tags : (Listof Symbol)]
   [intersect : Dia-Block-Intersect])
  #:type-name Dia:Block
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

(struct dia:block:note dia:block
  ([body : Geo-Option-Rich-Text])
  #:type-name Dia:Block:Note
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia:block-same-type? : (-> Dia:Block (Option Dia:Block) Boolean)
  (lambda [lgt rgt]
    (and rgt
         (eq? (object-name (dia:block-phantom-type lgt))
              (object-name (dia:block-phantom-type rgt))))))

(define dia:block-diff-type? : (-> Dia:Block (Option Dia:Block) Boolean)
  (lambda [lgt rgt]
    (if rgt
        (not (eq? (object-name (dia:block-phantom-type lgt))
                  (object-name (dia:block-phantom-type rgt))))
        #true)))

(define dia:block-typeof? : (-> Dia:Block (-> Any Boolean) Boolean)
  (lambda [self phantom-type?]
    (phantom-type? (dia:block-phantom-type self))))

(define dia:block*-typeof? : (-> (Option Dia:Block) (-> Any Boolean) Boolean : #:+ Dia:Block)
  (lambda [self phantom-type?]
    (and self (phantom-type? (dia:block-phantom-type self)))))

(define dia:block-has-tag? : (-> Dia:Block Symbol Boolean)
  (lambda [self tag]
    (and (memq tag (dia:block-tags self)) #true)))

(define dia:block*-has-tag? : (-> (Option Dia:Block) Symbol Boolean : #:+ Dia:Block)
  (lambda [self tag]
    (and self (dia:block-has-tag? self tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-apothem-intersect : (-> Nonnegative-Flonum Nonnegative-Flonum Dia-Block-Intersect)
  (lambda [horizon% vertical%]
    (λ [A B node-pos nlayer]
      (define-values (+ox +oy) (geo-layer-size nlayer horizon% vertical%))
      (define-values (-ox -oy) (values (- +ox) (- +oy)))
      
      (geo-line-polygon-intersect/first* #:translate node-pos A B
                                         (list (make-rectangular -ox -oy)
                                               (make-rectangular +ox -oy)
                                               (make-rectangular +ox +oy)
                                               (make-rectangular -ox +oy))))))

(define dia-default-intersect : Dia-Block-Intersect (dia-apothem-intersect 0.5 0.5))

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
(define dia-block-tags : (-> Any (Listof Symbol))
  (lambda [maybe-tags]
    (cond [(not maybe-tags) null]
          [(symbol? maybe-tags) (list maybe-tags)]
          [(list? maybe-tags) (filter symbol? maybe-tags)]
          [else null])))

(define dia-block-desc-from-caption : (-> (Option Geo) (Option String))
  (lambda [maybe-caption]
    (and maybe-caption
         (cond [(geo:string? maybe-caption) (geo:string-body maybe-caption)]
               [(geo:group? maybe-caption) (geo:group-desc maybe-caption)]
               [else #false]))))
