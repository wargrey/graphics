#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [geo-polyline geo-lines]))
(provide (rename-out [geo-sandglass geo-hourglass]))
(provide (rename-out [geo-dart geo-arrowhead]))
(provide (rename-out [geo-trapezium geo-trapezoid]))
(provide (rename-out [geo-regular-polygon geo-regular-convex-polygon]))
(provide (rename-out [geo-star-polygon geo-regular-star-polygon]))

(provide (all-from-out "digitama/edge/label.rkt"))
(provide (all-from-out "digitama/edge/marker/self.rkt" "digitama/edge/markers.rkt"))
(provide (all-from-out "digitama/skeleton/stickman/interface.rkt"))

(provide geo-blank geo-ghost geo-solid geo:blank? Geo:Blank Geo:Solid)
(provide geo-bitmap geo-rectangular geo:bitmap? Geo:Bitmap)
(provide geo-text geo:text? Geo:Text Geo-Text-Line geo-text-line?)
(provide geo-art-text geo:art-text? Geo:Art-Text)
(provide geo-paragraph geo:para? Geo:Paragraph)
(provide geo-markup geo:markup? Geo:Markup)
(provide geo-square geo-rectangle geo:rect? Geo:Rectangle)
(provide geo-vline geo-hline geo-diagonal geo-anti-diagonal geo:line? Geo:Line)
(provide geo-circle geo:circle? Geo:Circle)
(provide geo-ellipse geo:ellipse? Geo:Ellipse)
(provide geo-arc geo:arc? Geo:Arc)
(provide geo-sector geo:sector? Geo:Sector)
(provide geo-arrow geo:arrow? Geo:Arrow)
(provide geo-dart geo:dart? Geo:Dart)
(provide geo-gear geo:gear? Geo:Gear)
(provide geo-stickman geo:stickman? Geo:Stickman)

(provide geo-edge geo-edge* geo:edge? Geo:Edge)
(provide geo-edge-attach-label geo-edge-unlabel geo:labeled-edge? Geo:Labeled-Edge)
(provide geo-edge-self-pin-position geo-edge-endpoints geo-edge-endpoint-offsets)

(provide geo-stadium geo-lstadium geo-rstadium geo:stadium? Geo:Stadium)
(provide geo-bullet geo:bullet? Geo:Bullet)
(provide geo-sandglass geo:sandglass? Geo:Sandglass)

(provide geo-regular-polygon geo-star-polygon geo:regular-polygon? Geo:Regular-Polygon)
(provide geo-polygon geo:polygon? Geo:Polygon)
(provide geo-polyline geo:polyline? Geo:Polyline)
(provide geo-parallelogram geo-rhombus geo-trapezium geo-keyboard geo-house geo-star geo-hexagon-tile)

(provide geo-storage geo:storage? Geo:Storage)
(provide geo-document geo:document? Geo:Document)
(provide geo-database geo:database? Geo:Database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "digitama/dc/text.rkt")
(require "digitama/dc/rect.rkt")
(require "digitama/dc/arc.rkt")
(require "digitama/dc/plain.rkt")
(require "digitama/dc/raster.rkt")
(require "digitama/dc/line.rkt")
(require "digitama/dc/arrow.rkt")
(require "digitama/dc/edge.rkt")
(require "digitama/dc/gear.rkt")
(require "digitama/dc/stickman.rkt")
(require "digitama/dc/more.rkt")

(require "digitama/dc/polygon.rkt")
(require "digitama/dc/polygon/quadrilateral.rkt")
(require "digitama/dc/polygon/pentagon.rkt")
(require "digitama/dc/polygon/hexagon.rkt")

(require "digitama/edge/label.rkt")
(require "digitama/edge/markers.rkt")
(require "digitama/edge/marker/self.rkt")

(require "digitama/skeleton/stickman/interface.rkt")

(require "digitama/unsafe/dc/text-layout.rkt")

