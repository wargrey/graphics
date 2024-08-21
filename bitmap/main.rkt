#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/misc)
(require geofun/digitama/source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/provide colorspace)
(require/provide geofun/version)
(require/provide geofun/color geofun/font geofun/paint)
(require/provide geofun/digitama/geometry/dot geofun/digitama/geometry/constants)

(require/provide "base.rkt" "effect.rkt")

; reproviding geofun/constants causes name conflict
