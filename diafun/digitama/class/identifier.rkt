#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/convert)
(require geofun/digitama/path/self)
(require geofun/digitama/geometry/anchor)

(require "../node/dc.rkt")
(require "../edge/label.rkt")
(require "../path/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaCls-RelationShip-Type (U 'association 'aggregation 'composition 'bidirection 'dependency))
(define-type DiaCls-RelationShip-Identifier (U (HashTable (Pairof Symbol Symbol) DiaCls-RelationShip-Type)
                                               (-> Symbol Symbol (U False Void DiaCls-RelationShip-Type))))

(define default-diacls-relationship-identifier : (Parameterof (Option DiaCls-RelationShip-Identifier)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-block-identify : Dia-Path-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (not (eq? (string-ref text 0) #\.))
         (if (symbol? anchor)
             (dia-path-block-style-construct anchor text (default-diacls-class-style-make) make-diacls-class-style)
             (dia-path-block-style-construct anchor text (default-diacls-interface-style-make) make-diacls-interface-style)))))

(define default-diacls-arrow-identify : Dia-Path-Arrow-Identifier
  (lambda [source target labels extra-info]
    (define hints : (Listof Bytes) (dia-edge-label-flatten labels))

    (cond
      [(or (eq? source target) (not target)) ; for self associated classes
       (dia-edge-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
      [(dia-edge-label-double-angle-bracketed? hints)
       (dia-edge-style-construct source target labels (default-diacls-dependency-arrow-style-make)  make-diacls-dependency-arrow-style)]
      [else ; dependency and association
       (case/eq (diacls-association-identify (geo-id source) (geo-id target) (filter symbol? extra-info))
         [(composition) (dia-edge-style-construct source target labels (default-diacls-composition-arrow-style-make) make-diacls-composition-arrow-style)]
         [(aggregation) (dia-edge-style-construct source target labels (default-diacls-aggregation-arrow-style-make) make-diacls-aggregation-arrow-style)]
         [(association) (dia-edge-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
         [(bidirection) (dia-edge-style-construct source target labels (default-diacls-bidirection-arrow-style-make) make-diacls-bidirection-arrow-style)]
         [(dependency)  (dia-edge-style-construct source target labels (default-diacls-dependency-arrow-style-make)  make-diacls-dependency-arrow-style)]
         [else (let ([stype (dia:node-type source)]
                     [ttype (dia:node-type target)])
                 (if (and (eq? stype 'Class) (eq? ttype 'Interface))
                     (dia-edge-style-construct source target labels
                                               (default-diacls-realization-arrow-style-make)
                                               make-diacls-realization-arrow-style)
                     (dia-edge-style-construct source target labels
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
