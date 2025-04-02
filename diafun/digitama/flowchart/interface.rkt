#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/convert)

(require "style.rkt")
(require "node.rkt")
(require "../path/interface.rkt")

(require "../node/dc.rkt")
(require "../edge/style.rkt")
(require "../edge/label.rkt")
(require "../edge/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-label-construct : Dia-Path-Id->Node-Label
  (lambda [id label style hint]
    (define maybe-label : (U String Void False)
      (let ([labels (default-diaflow-node-label-string)])
        (and labels
             (if (hash? labels)
                 (hash-ref labels id (λ [] #false))
                 (labels id label)))))
    
    (dia-node-text-label id (if (string? maybe-label) maybe-label label) style)))

(define default-diaflow-node-fallback-construct : Dia-Path-Id->Node-Shape
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

(define default-diaflow-edge-construct : Dia-Path-Arrow->Edge
  (lambda [source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge (geo-id source) (and target (geo-id target)) #true)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (and target (not (dia:node:label? target)) (dia-edge-select-target-shape style))
               tracks)
     labels)))

(define default-diaflow-edge-label-construct : Dia-Path-Arrow->Edge-Label
  (lambda [source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (and (dia-edge-select-label-inline? style) 0.0)
                          start end info)))

(define default-diaflow-free-edge-construct : Dia-Path-Free-Track->Edge
  (lambda [source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge source target #false)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (dia-edge-select-target-shape style)
               tracks)
     labels)))

(define default-diaflow-free-edge-label-construct : Dia-Path-Free-Track->Edge-Label
  (lambda [source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (and (dia-edge-select-label-inline? style) 0.0)
                          start end info)))
