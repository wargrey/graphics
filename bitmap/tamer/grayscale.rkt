#lang typed/racket/base

(require "../digitama/draw.rkt")
(require "../effect.rkt")

(define romedalen (read-bitmap (collection-file-path "romedalen.png" "bitmap" "tamer" "cairo") #:backing-scale 2.0))

romedalen
(bitmap-grayscale/lightness romedalen)
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
