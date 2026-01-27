#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../block/dc.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/symbol.rkt")

(require geofun/composite)

(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/gadget)
(require geofun/digitama/dc/dingbat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) act-block-initial : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-circle block-key #false style height height direction 'Initial subtype)))

(define #:forall (S) act-block-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-bullseye block-key style width height direction (&% 72) 'Final 'Activity)))

(define #:forall (S) act-block-flow-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-circle block-key style width height direction
                       (list pi/4 3pi/4)
                       'Final 'Flow)))

(define #:forall (S) act-block-decision : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-diamond block-key style width height direction 'Decision subtype)))

(define #:forall (S) act-block-merge : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-diamond block-key style width height direction 'Merge subtype)))

(define #:forall (S) act-block-fork : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-rectangle/cr:2nd block-key caption style width height direction 'Fork subtype)))

(define #:forall (S) act-block-join : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-rectangle/cr:2nd block-key caption style width height direction 'Join subtype)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) act-block-action : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-rectangle/cr:4th block-key caption style width height direction 'Action subtype)))

(define #:forall (S) act-block-time-event : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignore-width height direction subtype]
    (create-dia-block #:id block-key #:tag 'Action 'Time
                      #:fit-region 1.00 0.45 0.00 0.00
                      #:create-with style [geo-sandglass (* height 0.618)]
                      caption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) act-block-connector : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-circle block-key caption style width height direction 'Connector subtype)))
