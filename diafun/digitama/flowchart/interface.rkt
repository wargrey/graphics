#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "block.rkt")
(require "../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-block-fallback-construct : Dia-Anchor->Block
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diaflow-start-style) (diaflow-block-terminal id label style width height direction hint)]
             [(diaflow-stop-style) (diaflow-block-terminal id label style width height direction hint)]
             [(diaflow-inspection-style) (diaflow-block-inspection id label style width height direction hint)]
             [(diaflow-reference-style) (diaflow-block-reference id label style width height direction hint)]
             
             [(diaflow-preparation-style) (diaflow-block-preparation id label style width height direction hint)]
             [(diaflow-input-style) (diaflow-block-input id label style width height direction hint)]
             [(diaflow-output-style) (diaflow-block-output id label style width height direction hint)]
             [(diaflow-process-style) (diaflow-block-process id label style width height direction hint)]
             [(diaflow-decision-style) (diaflow-block-decision id label style width height direction hint)]
             [(diaflow-delay-style) (diaflow-block-delay id label style width height direction hint)]
             [(diaflow-operation-style) (diaflow-block-manual-operation id label style width height direction hint)]
             
             [(diaflow-selection-style) (diaflow-block-selection id label style width height direction hint)]
             [(diaflow-junction-style) (diaflow-block-junction id label style width height direction hint)]
             [(diaflow-extract-style) (diaflow-block-extract id label style width height direction hint)]
             [(diaflow-merge-style) (diaflow-block-merge id label style width height direction hint)]
             
             [(diaflow-storage-style) (diaflow-block-storage id label style width height direction hint)]
             [(diaflow-collation-style) (diaflow-block-collation id label style width height direction hint)]
             [(diaflow-sort-style) (diaflow-block-sort id label style width height direction hint)])))
