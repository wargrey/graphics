#lang typed/racket/base

(provide (all-defined-out) Geo-Spacing Dia:Block dia:block?)
(provide (all-from-out digimon/constant digimon/complex digimon/flonum))
(provide (all-from-out geofun/digitama/base geofun/digitama/self geofun/digitama/paint/self))
(provide (all-from-out geofun/composite geofun/paint geofun/stroke geofun/fill geofun/font geofun/color))
(provide (all-from-out geofun/markup) dc-markup-text? DC-Markup-Text)
(provide (all-from-out "shared.rkt" "interface.rkt"))

(provide default-dia-block-margin create-dia-block)

(require geofun/paint)
(require geofun/stroke)
(require geofun/fill)
(require geofun/font)
(require geofun/color)

(require geofun/composite)
(require geofun/markup)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/markup)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/spacing)

(require digimon/constant)
(require digimon/complex)
(require digimon/flonum)

(require "shared.rkt")
(require "interface.rkt")
(require "block/dc.rkt")
(require "block/style.rkt")
