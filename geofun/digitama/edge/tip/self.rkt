#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Placement (U 'inside 'outside 'center))
(define-type Option-Geo-Tip (Option Geo-Tip))
(define-type Maybe-Geo-Tip (U Option-Geo-Tip Void))

(struct Geo-Tip ())
