#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cls-RelationShip-Type (U 'association 'aggregation 'composition 'bidirection 'dependency))
(define-type Cls-RelationShip-Identifier (U (HashTable (Pairof Symbol Symbol) Cls-RelationShip-Type)
                                            (-> Symbol Symbol (U False Void Cls-RelationShip-Type))))

(define default-cls-relationship-identifier : (Parameterof (Option Cls-RelationShip-Identifier)) (make-parameter #false))
