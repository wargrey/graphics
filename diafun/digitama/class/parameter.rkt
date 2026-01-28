#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cls-RelationShip-Type (U 'association 'aggregation 'composition 'bidirection 'dependency))
(define-type Cls-RelationShip-Identifier (U (HashTable (Pairof Symbol Symbol) Cls-RelationShip-Type)
                                            (-> Symbol Symbol (U False Void Cls-RelationShip-Type))))

(define default-cls-relationship-identifier : (Parameterof (Option Cls-RelationShip-Identifier)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-cls-stereotype-gapsize : (Parameterof Length+%) (make-parameter 4.0))
(define default-cls-stereotype-font : (Parameterof (Option Font)) (make-parameter dia-preset-block-tag-font))
