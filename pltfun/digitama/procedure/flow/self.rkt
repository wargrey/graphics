#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require diafun/digitama/track/interface)
(require diafun/digitama/block/interface)

(require diafun/digitama/flowchart/self)
(require diafun/digitama/flowchart/style)
(require diafun/digitama/flowchart/identifier)

(require "style.rkt")
(require "customize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter #:specialized (Flow-Track-Style) plt-flow-track-factory : Plt-Flow-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Flow-Track-Style) plt-flow-track-identify]
   [annotator : (Option (Dia-Track-Annotator Flow-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Flow-Track-Style)) #false]
   [λroot-style : (Option (Dia-Track-Link-Root-Style Flow-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-plt-flow-track-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Flow-Block-Style Flow-Block-Metadata) plt-flow-block-factory : Plt-Flow-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Flow-Block-Style Flow-Block-Metadata) default-flow-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Flow-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)) plt-flow-block-build]
   [fallback-builder : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata) default-flow-block-fallback-build]
   [λroot-style : (Option (Dia-Block-Link-Root-Style Flow-Block-Style)) #false]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-plt-flow-block-backstop-style])
  #:transparent)
