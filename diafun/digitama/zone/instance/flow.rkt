#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/dingbat)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require "../self.rkt")
(require "../../presets.rkt")

(require "../../block/dc.rkt")
(require "../../block/dc/node.rkt")
(require "../../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-zone-factory : Dia-Zone-Factory (make-dia-zone-factory #:builder #false))
