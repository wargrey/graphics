#lang typed/racket/base

(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define romedalen (read-bitmap (collection-file-path "romedalen.png" "geofun" "tamer" "pangocairo") #:backing-scale 2.0))

romedalen
(time (bitmap-grayscale/lightness romedalen))
(bitmap-grayscale/average romedalen)
(bitmap-grayscale/luminosity romedalen)

'decomposition
(bitmap-grayscale/decomposition romedalen 'max)
(bitmap-grayscale/decomposition romedalen 'min)

'channel
(bitmap-grayscale/channel romedalen 'red)
(bitmap-grayscale/channel romedalen 'green)

(collect-garbage)
(bitmap-grayscale/channel romedalen 'blue)
