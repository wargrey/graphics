#lang typed/racket/base

(provide (all-defined-out) Geo-Spacing Expr:Slot expr:slot?)
(provide (all-from-out digimon/constant digimon/complex digimon/flonum))
(provide (all-from-out geofun/digitama/base geofun/digitama/self geofun/digitama/paint/self))
(provide (all-from-out geofun/richtext geofun/composite geofun/resize geofun/adapter))
(provide (all-from-out geofun/paint geofun/stroke geofun/fill geofun/font geofun/color))
(provide (all-from-out "shared.rkt" "interface.rkt"))

(provide default-expr-slot-margin create-expr-slot)

(require geofun/paint)
(require geofun/stroke)
(require geofun/fill)
(require geofun/font)
(require geofun/color)
(require geofun/richtext)

(require geofun/resize)
(require geofun/adapter)
(require geofun/composite)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/spacing)

(require digimon/constant)
(require digimon/complex)
(require digimon/flonum)

(require "shared.rkt")
(require "interface.rkt")
(require "slot/dc.rkt")
(require "slot/style.rkt")
