#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/self)
(require geofun/digitama/path/label)
(require geofun/digitama/track/anchor)

(require "../block/dc.rkt")
(require "../block/interface.rkt")
(require "../track/interface.rkt")

(require "style.rkt")
(require "relationship.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-cls-block-identify : (Dia-Block-Identifier Cls-Block-Style Cls-Block-Metadata)
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (not (eq? (string-ref text 0) #\.))
         (if (symbol? anchor)
             (let* ([tail (regexp-match #px"(.+)#(\\w+)$" text)]
                    [cname (and tail (cadr tail))]
                    [tag (and tail (caddr tail))])
               (if (and cname tag)
                   (cls-tagged-class-identify anchor cname (string->symbol tag))
                   (cls-named-class-identify anchor text)))
             (cls-interface-style-construct anchor text)))))

(define default-cls-track-identify : (Dia-Track-Identifier Cls-Track-Style)
  (lambda [source target labels extra-info]
    (cond
      [(or (eq? source target) (not target)) ; for self associated classes
       (cls-track-adjust source target labels default-cls~association~style)]
      [(geo-path-label-has-stereotype? labels)
       (cls-track-adjust source target labels default-cls~dependency~style)]
      [else ; dependency and association
       (case/eq (cls-association-identify (geo-id source) (geo-id target) (filter symbol? extra-info))
         [(composition) (cls-track-adjust source target labels default-cls~composition~style)]
         [(aggregation) (cls-track-adjust source target labels default-cls~aggregation~style)]
         [(association) (cls-track-adjust source target labels default-cls~association~style)]
         [(bidirection) (cls-track-adjust source target labels default-cls~bidirection~style)]
         [(dependency)  (cls-track-adjust  source target labels  default-cls~dependency~style)]
         [else (let ([stype (dia:block-type source)]
                     [ttype (dia:block-type target)])
                 (if (and (eq? stype 'Class) (eq? ttype 'Interface))
                     (cls-track-adjust source target labels default-cls~realization~style)
                     (cls-track-adjust source target labels default-cls~generalization~style)))])])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cls-tagged-class-identify : (-> Symbol String Symbol (Option (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata)))
  (lambda [anchor text tag]
    (case/eq tag
      [(T t Type type) (cls-plain-type-construct anchor text)]
      [(A a Abstract abstract) (cls-abstract-style-construct anchor text)]
      [(E e Enum enum Enumeration enumeration) (cls-block-info anchor text default-cls-enumeration-style)]
      [(λ F f Lambda lambda Function function) (cls-block-info anchor text default-cls-lambda-style)]
      [(Functor functor) (cls-block-info anchor text default-cls-lambda-style)]
      [else (cls-block-info anchor text default-cls-unrecognized-style tag)])))

(define cls-named-class-identify : (-> Symbol String (Option (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata)))
  (lambda [anchor text]
    (cond [(regexp-match? #px"(^I[A-Z]\\w*)|(<%>$)" text) (cls-interface-style-construct anchor text)]
          [(regexp-match? #px"^(Abstract|Base)[A-Z]\\w*" text) (cls-abstract-style-construct anchor text)]
          [else (cls-plain-style-construct anchor text)])))

(define cls-association-identify : (-> Symbol Symbol (Listof Symbol) (Option Cls-RelationShip-Type))
  (lambda [source target extra-info]
    (if (pair? extra-info)
        (let-values ([(self rest) (values (car extra-info) (cdr extra-info))])
          (if (memq self '(composition aggregation association bidirection dependency))
              self
              (cls-association-identify source target rest)))

        (let ([identifier (default-cls-relationship-identifier)])
          (and identifier
               (cond [(hash? identifier) (hash-ref identifier (cons source target) (λ [] #false))]
                     [(procedure? identifier) (let ([type (identifier source target)]) (and (symbol? type) type))]
                     [else #false]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cls-interface-style-construct : (-> Geo-Anchor-Name String (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata))
  (lambda [anchor text]
    (cls-block-info anchor text default-cls-interface-style 'Interface)))

(define cls-abstract-style-construct : (-> Geo-Anchor-Name String (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata))
  (lambda [anchor text]
    (cls-block-info anchor text default-cls-abstract-style 'Abstract)))

(define cls-plain-type-construct : (-> Geo-Anchor-Name String (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata))
  (lambda [anchor text]
    (cls-block-info anchor text default-cls-type-style 'Type)))

(define cls-plain-style-construct : (-> Geo-Anchor-Name String (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata))
  (lambda [anchor text]
    (cls-block-info anchor text default-cls-class-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cls-block-info : (->* (Geo-Anchor-Name String (-> (Dia-Block-Style Cls-Block-Style))) (Cls-Block-Metadata)
                              (Dia-Block-Info Cls-Block-Style Cls-Block-Metadata))
  (lambda [anchor text mk-style [datum #false]]
    ((inst dia-block-info Cls-Block-Style Cls-Block-Metadata) anchor text mk-style default-cls-block-theme-adjuster datum)))

(define cls-track-adjust : (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Label-Datum) (-> (Dia-Track-Style Cls-Track-Style))
                              (Option (Dia-Track-Style Cls-Track-Style)))
  (lambda [source target label mk-style]
    ((inst dia-track-theme-adjust Cls-Track-Style Dia:Block (Option Dia:Block)) source target label mk-style default-cls-track-theme-adjuster)))
