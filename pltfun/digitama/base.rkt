#lang typed/racket/base

(provide (all-defined-out) Geo-Insets-Datum)
(provide (all-from-out digimon/measure digimon/complex digimon/flonum))
(provide (all-from-out geofun/digitama/base geofun/digitama/self geofun/digitama/paint/self))
(provide (all-from-out geofun/composite geofun/resize geofun/adapter))
(provide (all-from-out geofun/paint geofun/stroke geofun/fill geofun/font geofun/color))

(require geofun/paint)
(require geofun/stroke)
(require geofun/fill)
(require geofun/font)
(require geofun/color)
(require geofun/richtext)

(require geofun/composite)
(require geofun/adapter)
(require geofun/resize)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/sides)

(require digimon/measure)
(require digimon/complex)
(require digimon/flonum)
