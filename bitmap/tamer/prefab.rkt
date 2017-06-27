#lang typed/racket

(require "../digitama/draw.rkt")
(require "../prefab.rkt")

(require (for-syntax images/logos))

(define shape : Bitmap (time (prefab-bitmap (time (plt-logo)))))
shape
