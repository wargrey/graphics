#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Marker-Placement (U 'inside 'outside 'center))
(define-type Option-Geo-Marker (Option Geo-Marker))
(define-type Maybe-Geo-Marker (U Option-Geo-Marker Void))

(struct Geo-Marker ())
