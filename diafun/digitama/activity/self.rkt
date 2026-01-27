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
(struct dia:activity dia:track ()
  #:type-name Dia:Activity
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-act-block-fallback-build : (Dia-Block-Builder Act-Block-Style Act-Block-Metadata)
  (lambda [id caption style width height direction subtype]
    (dia-block-case
     style
     [(act-action-style?) (act-block-action id caption style width height direction subtype)]
     [(act-decision-style?) (act-block-decision id caption style width height direction subtype)]
     [(act-merge-style?) (act-block-merge id caption style width height direction subtype)]
     [(act-time-event-style?) (act-block-time-event id caption style width height direction subtype)]

     [(act-initial-style?) (act-block-initial id caption style width height direction subtype)]
     [(act-final-style?) (act-block-final id caption style width height direction subtype)]
     [(act-flow-final-style?) (act-block-flow-final id caption style width height direction subtype)]
     [(act-connector-style?) (act-block-connector id caption style width height direction subtype)]
     
     [(act-fork-style?) (act-block-fork id caption style width height direction subtype)]
     [(act-join-style?) (act-block-join id caption style width height direction subtype)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Act-Block-Describer (Dia-Block-Describer Act-Block-Style Act-Block-Metadata))

(define-struct/parameter #:specialized (Act-Block-Style Act-Block-Metadata) act-block-factory : Act-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Act-Block-Style Act-Block-Metadata) default-act-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Act-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Act-Block-Style Act-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder Act-Block-Style Act-Block-Metadata) default-act-block-fallback-build]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-act-block-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Act-Track-Style) act-track-factory : Act-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Act-Track-Style) default-act-track-identify]
   [annotator : (Option (Dia-Track-Annotator Act-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Act-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-act-track-backstop-style])
  #:transparent)
