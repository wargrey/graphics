#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../block/dc.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/symbol.rkt")

(require "parameter.rkt")

(require geofun/digitama/dc/gadget)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; controls
(define #:forall (S) act-block-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-bullseye block-key style width height (&% 72) 'Activity)))

(define #:forall (S) act-block-flow-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-circle block-key style width height (list pi/4 3pi/4) 'Flow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actions
(define #:forall (S) act-block-time-event : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignore-width height direction subtype]
    (create-dia-block #:id block-key '(Event Time)
                      #:fit-region 1.00 0.45 0.00 0.00
                      #:create-with style [geo-sandglass (* height 0.618)]
                      caption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects
(define #:forall (S) act-block-central-buffer : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define caption+stereotype
      (dia-caption+stereotype caption (if (or subtype) 'datastore 'CentralBuffer) style
                              (default-act-stereotype-font) (default-act-stereotype-gapsize)
                              height))
    
    (dia-block-rectangle block-key (or caption+stereotype caption)
                         style width height direction subtype)))
