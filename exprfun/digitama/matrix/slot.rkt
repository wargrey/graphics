#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/resize)
(require geofun/digitama/geometry/sides)

(require "../slot/style.rkt")
(require "../slot/dc.rkt")

(require "types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-slot-row-header : Mtx-Header-Slot-Create
  (lambda [id term0 style width height direction indices]
    (define-values (mt mright mb mleft) (geo-inset-values (default-expr-slot-margin)))
    (define term (mtx-rotate term0 direction))
    (define cell-width
      (cond [(or term) (max (+ (geo-width term) mright mleft) width)]
            [else width]))
    
    (mtx-slot-shape id term style cell-width height
                    1.0 0.5 1.0 0.5)))

(define mtx-slot-col-header : Mtx-Header-Slot-Create
  (lambda [id term0 style width height direction indices]
    (define-values (mtop mr mbottom ml) (geo-inset-values (default-expr-slot-margin)))
    (define term (mtx-rotate term0 direction))
    (define cell-height
      (cond [(or term) (max (+ (geo-height term) mtop mbottom) height)]
            [else height]))

    (case/eq (mtx-hdr-anchor indices)
      [(cb) (mtx-slot-shape id term style width cell-height 0.5 1.0 0.5 1.0)]
      [(ct) (mtx-slot-shape id term style width cell-height 0.5 0.0 0.5 0.0)]
      [else (mtx-slot-shape id term style width cell-height 0.5 0.5 0.5 0.5)])))

(define mtx-slot-corner : Mtx-Header-Slot-Create
  (lambda [id term style width height direction indices]
    (mtx-slot-shape id (mtx-rotate term direction) style width height)))

(define mtx-slot-entry : Mtx-Slot-Create
  (lambda [id term style width height direction indices]
    (mtx-slot-shape id (mtx-rotate term direction) style width height)))

(define mtx-slot-hole : Mtx-Slot-Create
  (lambda [id term style width height direction indices]
    (mtx-slot-shape id #false style width height)))

(define mtx-slot-mask : Mtx-Slot-Create
  (lambda [id term style width height direction indices]
    (mtx-slot-shape id #false style width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-rotate : (-> (Option Geo) (Option Flonum) (Option Geo))
  (lambda [term direction]
    (cond [(or (not direction) (zero? direction)) term]
          [else (and term (geo-rotate term direction))])))

(define mtx-slot-shape : (->* (Symbol (Option Geo) Expr-Slot-Style-Layers Nonnegative-Flonum Nonnegative-Flonum)
                              (Flonum Flonum Flonum Flonum)
                              Expr:Slot)
  (lambda [id term style width height [sx% 0.5] [sy% 0.5] [tx% 0.5] [ty% 0.5]]
    (create-expr-slot #:id id
                      #:alignment sx% sy% tx% ty%
                      #:create-with style [geo-rectangle width height]
                      term)))
