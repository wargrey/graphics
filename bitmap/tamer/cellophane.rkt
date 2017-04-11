#lang typed/racket/base

(require typed/images/logos)
(require "../transform.rkt")

(define b (planet-logo))

b
(bitmap-cellophane b 0.80)
(bitmap-cellophane b 0.64)
(bitmap-cellophane b 0.32)
(bitmap-cellophane b 0.16)
(bitmap-cellophane b 0.0)