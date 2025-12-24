#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/self)
(require geofun/digitama/path/label)
(require geofun/digitama/geometry/anchor)

(require "style.rkt")
(require "block.rkt")

(require "../block/dc.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaCls-RelationShip-Type (U 'association 'aggregation 'composition 'bidirection 'dependency))
(define-type DiaCls-RelationShip-Identifier (U (HashTable (Pairof Symbol Symbol) DiaCls-RelationShip-Type)
                                               (-> Symbol Symbol (U False Void DiaCls-RelationShip-Type))))

(define default-diacls-relationship-identifier : (Parameterof (Option DiaCls-RelationShip-Identifier)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-block-identify : Dia-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (not (eq? (string-ref text 0) #\.))
         (if (symbol? anchor)
             (dia-block-info anchor text (default-diacls-class-style-make) make-diacls-class-style)
             (dia-block-info anchor text (default-diacls-interface-style-make) make-diacls-interface-style)))))

(define default-diacls-track-identify : Dia-Track-Identifier
  (lambda [source target labels extra-info]
    (define hints : (Listof Bytes) (geo-path-label-flatten labels))

    (cond
      [(or (eq? source target) (not target)) ; for self associated classes
       (dia-track-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
      [(geo-path-double-angle-bracketed-label? hints)
       (dia-track-style-construct source target labels (default-diacls-dependency-arrow-style-make)  make-diacls-dependency-arrow-style)]
      [else ; dependency and association
       (case/eq (diacls-association-identify (geo-id source) (geo-id target) (filter symbol? extra-info))
         [(composition) (dia-track-style-construct source target labels (default-diacls-composition-arrow-style-make) make-diacls-composition-arrow-style)]
         [(aggregation) (dia-track-style-construct source target labels (default-diacls-aggregation-arrow-style-make) make-diacls-aggregation-arrow-style)]
         [(association) (dia-track-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
         [(bidirection) (dia-track-style-construct source target labels (default-diacls-bidirection-arrow-style-make) make-diacls-bidirection-arrow-style)]
         [(dependency)  (dia-track-style-construct source target labels (default-diacls-dependency-arrow-style-make)  make-diacls-dependency-arrow-style)]
         [else (let ([stype (dia:block-type source)]
                     [ttype (dia:block-type target)])
                 (if (and (eq? stype 'Class) (eq? ttype 'Interface))
                     (dia-track-style-construct source target labels
                                                (default-diacls-realization-arrow-style-make)
                                                make-diacls-realization-arrow-style)
                     (dia-track-style-construct source target labels
                                                (default-diacls-generalization-arrow-style-make)
                                                make-diacls-generalization-arrow-style)))])])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diacls-association-identify : (-> Symbol Symbol (Listof Symbol) (Option DiaCls-RelationShip-Type))
  (lambda [source target extra-info]
    (if (pair? extra-info)
        (let-values ([(self rest) (values (car extra-info) (cdr extra-info))])
          (if (memq self '(composition aggregation association bidirection dependency))
              self
              (diacls-association-identify source target rest)))

        (let ([identifier (default-diacls-relationship-identifier)])
          (and identifier
               (cond [(hash? identifier) (hash-ref identifier (cons source target) (λ [] #false))]
                     [(procedure? identifier) (let ([type (identifier source target)]) (and (symbol? type) type))]
                     [else #false]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-block-fallback-construct : Dia-Anchor->Block
  (lambda [id brief style width height direction subtype]
    (cond [(diacls-interface-style? style) (diacls-block-interface id brief style width height direction subtype)]
          [(diacls-class-style? style)     (diacls-block-class id brief style width height direction subtype)])))
