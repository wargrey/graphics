#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../track/dc.rkt")
(require "../track/interface.rkt")
(require "../block/interface.rkt")

(require "style.rkt")
(require "block.rkt")
(require "identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:class dia:track ()
  #:type-name Dia:Class
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-cls-block-fallback-build : (Dia-Block-Builder Cls-Block-Style Cls-Block-Metadata)
  (lambda [id caption style width height direction stereotype]
    (dia-block-case
     style
     [(cls-interface-style?) (cls-block-interface id caption style width height direction stereotype)]
     [(cls-type-style?) (cls-block-class id caption style width height direction stereotype)]
     [(cls-class-style?) (cls-block-class id caption style width height direction stereotype)]
     [(cls-abstract-style?) (cls-block-class id caption style width height direction stereotype)]
     [(cls-enumeration-style?) (cls-block-class id caption style width height direction stereotype)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter #:specialized (Cls-Block-Style Cls-Block-Metadata) cls-block-factory : Cls-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Cls-Block-Style Cls-Block-Metadata) default-cls-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Cls-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Cls-Block-Style Cls-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder Cls-Block-Style Cls-Block-Metadata) default-cls-block-fallback-build]
   [λroot-style : (Option (Dia-Block-Link-Root-Style Cls-Block-Style)) #false]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-cls-block-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Cls-Track-Style) cls-track-factory : Cls-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Cls-Track-Style) default-cls-track-identify]
   [annotator : (Option (Dia-Track-Annotator Cls-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Cls-Track-Style)) #false]
   [λroot-style : (Option (Dia-Track-Link-Root-Style Cls-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-cls-track-backstop-style])
  #:transparent)
