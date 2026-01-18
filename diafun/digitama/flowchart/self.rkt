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
(struct dia:flowchart dia:track ()
  #:type-name Dia:FlowChart
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-flow-block-fallback-build : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)
  (lambda [id caption style width height direction subtype]
    (dia-block-case
     style
     [(flow-preparation-style?) (flow-block-preparation id caption style width height direction subtype)]
     [(flow-input-style?) (flow-block-input id caption style width height direction subtype)]
     [(flow-output-style?) (flow-block-output id caption style width height direction subtype)]
     [(flow-process-style?) (flow-block-process id caption style width height direction subtype)]
     [(flow-decision-style?) (flow-block-decision id caption style width height direction subtype)]
     [(flow-delay-style?) (flow-block-delay id caption style width height direction subtype)]
     [(flow-operation-style?) (flow-block-manual-operation id caption style width height direction subtype)]
     
     [(flow-start-style?) (flow-block-terminal id caption style width height direction subtype)]
     [(flow-stop-style?) (flow-block-terminal id caption style width height direction subtype)]
     [(flow-inspection-style?) (flow-block-inspection id caption style width height direction subtype)]
     [(flow-reference-style?) (flow-block-reference id caption style width height direction subtype)]
     
     [(flow-selection-style?) (flow-block-selection id caption style width height direction subtype)]
     [(flow-junction-style?) (flow-block-junction id caption style width height direction subtype)]
     [(flow-extract-style?) (flow-block-extract id caption style width height direction subtype)]
     [(flow-merge-style?) (flow-block-merge id caption style width height direction subtype)]
     
     [(flow-storage-style?) (flow-block-storage id caption style width height direction subtype)]
     [(flow-collation-style?) (flow-block-collation id caption style width height direction subtype)]
     [(flow-sort-style?) (flow-block-sort id caption style width height direction subtype)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter #:specialized (Flow-Track-Style) flow-track-factory : Flow-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Flow-Track-Style) default-flow-track-identify]
   [annotator : (Option (Dia-Track-Annotator Flow-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Flow-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-flow-track-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Flow-Block-Style Flow-Block-Metadata) flow-block-factory : Flow-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Flow-Block-Style Flow-Block-Metadata) default-flow-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Flow-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata) default-flow-block-fallback-build]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-flow-block-backstop-style])
  #:transparent)
