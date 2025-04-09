#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/case)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)

(require "../node/dc.rkt")
(require "../edge/label.rkt")
(require "../path/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaCls-Association-Type (U 'association 'aggregation 'composition))
(define-type DiaCls-Association-Identifier (U (HashTable (Pairof Symbol Symbol) DiaCls-Association-Type)
                                              (-> Symbol Symbol (U False Void DiaCls-Association-Type))))

(define default-diacls-association-identifier : (Parameterof (Option DiaCls-Association-Identifier)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-block-identify : Dia-Path-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (diacls-block-text-identify anchor text size))))

(define default-diacls-arrow-identify : Dia-Path-Arrow-Identifier
  (lambda [source target labels]
    (define hints : (Listof Bytes) (dia-edge-label-flatten labels))

    (define edge-style : Dia-Edge-Style
      (cond [(not target) ; for self associated classes
             (dia-edge-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
            [(eq? source target)
             (dia-edge-style-construct source target labels (default-diacls-association-arrow-style-make) make-diacls-association-arrow-style)]
            [(dia-edge-label-match? hints #px"[Uu][Ss][Ee][Ss]?")
             (dia-edge-style-construct source target labels (default-diacls-dependency-arrow-style-make) make-diacls-dependency-arrow-style)]
            [else (case/eq (diacls-association-identify (geo-id source) (geo-id target))
                           [(composition)
                            (dia-edge-style-construct source target labels
                                                      (default-diacls-composition-arrow-style-make)
                                                      make-diacls-composition-arrow-style)]
                           [(aggregation)
                            (dia-edge-style-construct source target labels
                                                      (default-diacls-aggregation-arrow-style-make)
                                                      make-diacls-aggregation-arrow-style)]
                           [(association)
                            (dia-edge-style-construct source target labels
                                                      (default-diacls-association-arrow-style-make)
                                                      make-diacls-association-arrow-style)]
                           [else (let ([stype (dia:node-type source)]
                                       [ttype (dia:node-type target)])
                                   (cond [(eq? stype ttype)
                                          (dia-edge-style-construct source target labels
                                                                    (default-diacls-generalization-arrow-style-make)
                                                                    make-diacls-generalization-arrow-style)]
                                         [(eq? stype 'Class)
                                          (dia-edge-style-construct source target labels
                                                                    (default-diacls-realization-arrow-style-make)
                                                                    make-diacls-realization-arrow-style)]
                                         [else (dia-edge-style-construct source target labels
                                                                         (default-diacls-association-arrow-style-make)
                                                                         make-diacls-association-arrow-style)]))])]))

    edge-style))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diacls-block-text-identify : (-> Geo-Anchor-Name String Positive-Index (Option Dia-Path-Block-Datum))
  (lambda [anchor text size]
    (define ch0 (string-ref text 0))

    (cond [(eq? ch0 #\:)
           (dia-path-block-style-construct anchor (substring text 1 size) (default-diacls-interface-style-make) make-diacls-interface-style)]
          [(eq? ch0 #\<)
           (dia-path-block-style-construct anchor (string-trim text #px"(^<+)|(>+$)") (default-diacls-interface-style-make) make-diacls-interface-style)]
          [(not (eq? ch0 #\.))
           (dia-path-block-style-construct anchor text (default-diacls-class-style-make) make-diacls-class-style)]
          [else #false])))

(define diacls-association-identify : (-> Symbol Symbol (Option DiaCls-Association-Type))
  (lambda [source target]
    (define identifier (default-diacls-association-identifier))

    (and identifier
         (cond [(hash? identifier) (hash-ref identifier (cons source target) (λ [] #false))]
               [(procedure? identifier) (let ([type (identifier source target)]) (and (symbol? type) type))]
               [else #false]))))
