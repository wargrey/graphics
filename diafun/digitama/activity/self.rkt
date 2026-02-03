#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../track/dc.rkt")
(require "../track/interface.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/symbol.rkt")

(require "style.rkt")
(require "block.rkt")
(require "identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:activity dia:track ()
  #:type-name Dia:Activity
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-act-track-linker : (Dia-Track-Link-Root-Style Act-Track-Style)
  (lambda [self]
    (cond [(dia-track-style?? self act~control~flow~style?) (default-act~control~flow~style)]
          [(dia-track-style?? self act~object~flow~style?) (default-act~object~flow~style)]
          [else #false])))

(define default-act-block-linker : (Dia-Block-Link-Root-Style Act-Block-Style)
  (lambda [self]
    (cond [(dia-block-style?? self act-object-node-style?) (default-act-object-style)]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-act-block-fallback-build : (Dia-Block-Builder Act-Block-Style Act-Block-Metadata)
  (lambda [key caption style width height direction maybe-type]
    (dia-block-case
     style
     [(act-action-style?) (dia-block-rectangle/cr:4th key caption style width height direction maybe-type)]
     [(act-object-style?) (act-block-object key caption style width height direction maybe-type)]
     [(act-material-style?) (act-block-object key caption style width height direction (or maybe-type 'material))]
     [(act-central-buffer-style?) (act-block-central-buffer key caption style width height direction maybe-type)]
     
     [(act-decision-style?) (dia-symbol-diamond key style width height direction maybe-type)]
     [(act-merge-style?) (dia-symbol-diamond key style width height direction maybe-type)]
     [(act-fork-style?) (act-block-fork key #false style width height direction maybe-type)]
     [(act-join-style?) (act-block-join key #false style width height direction maybe-type)]

     [(act-time-event-style?) (act-block-time-event key caption style width height direction maybe-type)]

     [(act-initial-style?) (dia-symbol-circle key style height height null maybe-type)]
     [(act-final-style?) (act-block-final key caption style width height direction maybe-type)]
     [(act-flow-final-style?) (act-block-flow-final key caption style width height direction maybe-type)]
     [(act-connector-style?) (dia-block-circle key caption style width height maybe-type)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Act-Block-Describer (Dia-Block-Describer Act-Block-Style Act-Block-Metadata))

(define-struct/parameter #:specialized (Act-Block-Style Act-Block-Metadata) act-block-factory : Act-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Act-Block-Style Act-Block-Metadata) default-act-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Act-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Act-Block-Style Act-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder Act-Block-Style Act-Block-Metadata) default-act-block-fallback-build]
   [λroot-style : (Option (Dia-Block-Link-Root-Style Act-Block-Style)) default-act-block-linker]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-act-block-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Act-Track-Style) act-track-factory : Act-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Act-Track-Style) default-act-track-identify]
   [dangling-identifier : (Option (Dia-Dangling-Track-Identifier Act-Track-Style)) default-act-dangling-track-identify]
   [annotator : (Option (Dia-Track-Annotator Act-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Act-Track-Style)) #false]
   [λroot-style : (Option (Dia-Track-Link-Root-Style Act-Track-Style)) default-act-track-linker]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-act-track-backstop-style])
  #:transparent)
