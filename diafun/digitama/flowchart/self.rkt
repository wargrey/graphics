#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require "../track/dc.rkt")
(require "../track/interface.rkt")
(require "../block/interface.rkt")
(require "../decoration/note/self.rkt")

(require "../block/dc.rkt")
(require "../block/dc/node.rkt")

(require "style.rkt")
(require "block.rkt")
(require "identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flowchart dia:track ()
  #:type-name Dia:FlowChart
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-flow-block-fallback-build : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)
  (lambda [key caption style width height direction subtype]
    (dia-block-case
     style
     [(flow-input-style?) (flow-block-input key caption style width height direction subtype)]
     [(flow-output-style?) (flow-block-output key caption style width height direction subtype)]
     [(flow-process-style?) (flow-block-process key caption style width height direction subtype)]
     [(flow-decision-style?) (dia-block-diamond key caption style width height direction subtype)]
     [(flow-delay-style?) (flow-block-delay key caption style width height direction subtype)]
     [(flow-operation-style?) (flow-block-manual-operation key caption style width height direction subtype)]
     
     [(flow-start-style?) (flow-block-terminal key caption style width height direction subtype)]
     [(flow-preparation-style?) (dia-block-hexagon key caption style width height direction subtype)]
     [(flow-stop-style?) (flow-block-terminal key caption style width height direction subtype)]
     [(flow-connector-style?) (dia-block-circle key caption style width height subtype)]
     [(flow-reference-style?) (flow-block-reference key caption style width height direction subtype)]
     
     [(flow-selection-style?) (flow-block-selection key caption style width height direction subtype)]
     [(flow-junction-style?) (flow-block-junction key caption style width height direction subtype)]
     [(flow-extract-style?) (flow-block-extract key caption style width height direction subtype)]
     [(flow-merge-style?) (flow-block-merge key caption style width height direction subtype)]
     
     [(flow-storage-style?) (flow-block-storage key caption style width height direction subtype)]
     [(flow-collation-style?) (flow-block-collation key caption style width height direction subtype)]
     [(flow-sort-style?) (flow-block-sort key caption style width height direction subtype)])))

(define default-flow-note-build : Dia-Note-Builder
  (lambda [key body style padding direction whatever]
    (define-values (top rgt bot lft) (geo-inset-values padding))
    (define-values (w h) (geo-size body))
    (define-values (width height) (values (+ lft w rgt) (+ top h bot)))
    (define angle (or direction 0.0))

    (create-dia-block #:block dia:block:note
                      #:id key whatever
                      #:with-group style
                      (geo-composite (geo-open-rectangle #:open-sides (list (cond [(< (abs angle) pi/4) 'l]
                                                                                  [(< angle (+ 3pi/4)) 't]
                                                                                  [(< angle (- 3pi/4)) 'b]
                                                                                  [else 'r]))
                                                         #:stroke (dia-block-resolve-stroke-paint style)
                                                         #:fill (dia-block-resolve-fill-paint style)
                                                         width height)
                                     lft top body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Flow-Block-Describer (Dia-Block-Describer Flow-Block-Style Flow-Block-Metadata))

(define-struct/parameter #:specialized (Flow-Block-Style Flow-Block-Metadata) flow-block-factory : Flow-Block-Factory #:as dia-block-factory
  ([identifier : (Dia-Block-Identifier Flow-Block-Style Flow-Block-Metadata) default-flow-block-identify]
   [typesetter : (Option (Dia-Block-Typesetter Flow-Block-Style)) #false]
   [builder : (Option (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)) #false]
   [fallback-builder : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata) default-flow-block-fallback-build]
   [λroot-style : (Option (Dia-Block-Link-Root-Style Flow-Block-Style)) #false]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-flow-block-backstop-style])
  #:transparent)

(define-struct/parameter #:specialized (Flow-Track-Style) flow-track-factory : Flow-Track-Factory #:as dia-track-factory
  ([identifier : (Dia-Track-Identifier Flow-Track-Style) default-flow-track-identify]
   [dangling-identifier : (Option (Dia-Dangling-Track-Identifier Flow-Track-Style)) default-flow-dangling-track-identify]
   [annotator : (Option (Dia-Track-Annotator Flow-Track-Style)) #false]
   [builder : (Option (Dia-Track-Builder Flow-Track-Style)) #false]
   [λroot-style : (Option (Dia-Track-Link-Root-Style Flow-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-flow-track-backstop-style])
  #:transparent)

(define flow-note-factory : Dia-Note-Factory (make-dia-note-factory #:builder default-flow-note-build))
