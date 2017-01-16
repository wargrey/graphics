#lang typed/racket/base

(require typed/images/logos)
(require "../grayscale.rkt")

(define b (planet-logo))

b
(bitmap-grayscale/lightness b)
(bitmap-grayscale/average b)
(bitmap-grayscale/luminosity b)

'decomposition
(bitmap-grayscale/decomposition b 'max)
(bitmap-grayscale/decomposition b 'min)

'channel
(bitmap-grayscale/channel b 'red)
(bitmap-grayscale/channel b 'green)
(bitmap-grayscale/channel b 'blue)

