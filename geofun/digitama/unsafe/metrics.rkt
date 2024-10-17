#lang racket/base

(provide (all-defined-out))

(require racket/unsafe/ops)
(require racket/draw/unsafe/pango)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~fx
  (lambda [fl]
    (unsafe-fl->fx (unsafe-flceiling fl))))

(define ~size
  (lambda [size]
    (unsafe-fx* (~fx size)
                PANGO_SCALE)))

(define ~metric
  (lambda [val]
    (unsafe-fl/ (unsafe-fx->fl val)
                (unsafe-fx->fl PANGO_SCALE))))
