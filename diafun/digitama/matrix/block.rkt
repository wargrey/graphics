#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/resize)

(require "../block/style.rkt")
(require "../block/dc.rkt")

(require "types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-block-row-header : Mtx-Block-Create
  (lambda [block-key brief0 style width height direction indices]
    (define brief (mtx-rotate brief0 direction))
    (define cell-width
      (cond [(or brief) (max (+ (geo-width brief) (* (default-dia-block-margin) 2.0)) width)]
            [else width]))
    
    (mtx-block-shape block-key 'Header 'Row brief style cell-width height)))

(define mtx-block-col-header : Mtx-Block-Create
  (lambda [block-key brief0 style width height direction indices]
    (define brief (mtx-rotate brief0 direction))
    (define cell-height
      (cond [(or brief) (max (+ (geo-height brief) (* (default-dia-block-margin) 2.0)) height)]
            [else height]))
    
    (mtx-block-shape block-key 'Header 'Column brief style width cell-height)))

(define mtx-block-corner : Mtx-Block-Create
  (lambda [block-key brief style width height direction indices]
    (mtx-block-shape block-key 'Corner #false (mtx-rotate brief direction) style width height)))

(define mtx-block-entry : Mtx-Block-Create
  (lambda [block-key brief style width height direction indices]
    (mtx-block-shape block-key 'Data 'Normal (mtx-rotate brief direction) style width height)))

(define mtx-block-hole : Mtx-Block-Create
  (lambda [block-key brief style width height direction indices]
    (mtx-block-shape block-key 'Data 'Hole #false style width height)))

(define mtx-block-mask : Mtx-Block-Create
  (lambda [block-key brief style width height direction indices]
    (mtx-block-shape block-key 'Data 'Mask #false style width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-rotate : (-> (Option Geo) (Option Flonum) (Option Geo))
  (lambda [brief direction]
    (cond [(or (not direction) (zero? direction)) brief]
          [else (and brief (geo-rotate brief direction))])))

(define mtx-block-shape : (-> Symbol Symbol (Option Symbol) (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum Dia:Block)
  (lambda [block-key type subtype brief style width height]
    (create-dia-block #:id block-key #:type type subtype
                      (geo-rectangle #:id (dia-block-shape-id block-key)
                                     #:stroke (dia-block-resolve-stroke-paint style)
                                     #:fill (dia-block-resolve-fill-paint style)
                                     width height)
                      brief)))
