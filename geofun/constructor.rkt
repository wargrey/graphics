#lang typed/racket/base

(provide (all-defined-out))
(provide geo-text geo:text? Geo:Text)
(provide geo-paragraph geo:para? Geo:Paragraph)
(provide geo-square geo-rectangle geo:rect? Geo:Rectangle)
(provide geo-circle geo:circle? Geo:Circle)
(provide geo-ellipse geo:ellipse? Geo:Ellipse)
(provide geo-arc geo:arc? Geo:Arc)
(provide geo-sector geo:sector? Geo:Sector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "digitama/dc/text.rkt")
(require "digitama/dc/rect.rkt")
(require "digitama/dc/circle.rkt")
