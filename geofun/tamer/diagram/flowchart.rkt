#lang typed/racket/base

(require geofun/vector)
(require geofun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! flow-chart [64 64 #:anchor '#:Start] #:-
  (step-down 1 'initialize)
  (step-down 1 '>>cin)
  (step-down 1 'okay?)
  (step-down 1 'process)
  (step-down 1 'cout<<)
  (step-down 1 'End$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-path-flow flow-chart))
