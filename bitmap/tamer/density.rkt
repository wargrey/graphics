#lang typed/racket/base

(require "../digitama/draw.rkt")
(require "../misc.rkt")

(define romedalen (read-bitmap (collection-file-path "romedalen.png" "bitmap" "tamer" "cairo") #:backing-scale 1.0))

romedalen
(bitmap-alter-density romedalen 2.0)
