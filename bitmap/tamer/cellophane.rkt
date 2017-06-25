#lang typed/racket/base

(require "../digitama/draw.rkt")
(require "../effect.rkt")

(define romedalen (read-bitmap (collection-file-path "romedalen.png" "bitmap" "tamer" "cairo") #:backing-scale 2.0))

(bitmap-cellophane romedalen 1.00)
(bitmap-cellophane romedalen 0.80)
(bitmap-cellophane romedalen 0.64)
(bitmap-cellophane romedalen 0.32)
(bitmap-cellophane romedalen 0.16)
(bitmap-cellophane romedalen 0.00)
