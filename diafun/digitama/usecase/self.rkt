#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../track/dc.rkt")
(require "../track/interface.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/actor.rkt")

(require "style.rkt")
(require "identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:use-case dia:track ()
  #:type-name Dia:Use-Case
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-uc-block-fallback-build : (Dia-Block-Builder UC-Block-Style UC-Block-Metadata)
  (lambda [id caption style width height direction property]
    (dia-block-case
     style
     [(uc-case-style?)   (dia-block-ellipse id caption style width height direction property)]
     [(uc-actor-style?) (dia-actor-stickman id caption style width height property)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type UC-Block-Describer (Dia-Block-Describer UC-Block-Style UC-Block-Metadata))

(define-struct/parameter #:specialized (UC-Block-Style UC-Block-Metadata) uc-block-factory : UC-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier UC-Block-Style UC-Block-Metadata) default-uc-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter UC-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder UC-Block-Style UC-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder UC-Block-Style UC-Block-Metadata) default-uc-block-fallback-build]
   [λroot-style : (Option (Dia-Block-Link-Root-Style UC-Block-Style)) #false]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-uc-block-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (UC-Track-Style) uc-track-factory : UC-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier UC-Track-Style) default-uc-track-identify]
   [dangling-identifier : (Option (Dia-Dangling-Track-Identifier UC-Track-Style)) #false]
   [annotator : (Option (Dia-Track-Annotator UC-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder UC-Track-Style)) #false]
   [λroot-style : (Option (Dia-Track-Link-Root-Style UC-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-uc-track-backstop-style])
  #:transparent)
