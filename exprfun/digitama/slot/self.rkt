#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct expr:slot geo:table ()
  #:type-name Expr:Slot
  #:transparent)
