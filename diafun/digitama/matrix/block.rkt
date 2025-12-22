#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/resize)

(require "../block/style.rkt")
(require "../block/dc.rkt")
(require "../track/interface.rkt")

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diamtx-block-row-header : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief0 style width height direction indices]
    (define brief (diamtx-rotate brief0 direction))
    (define cell-width
      (cond [(or brief) (max (+ (geo-width brief) (* (default-dia-block-margin) 2.0)) width)]
            [else width]))
    
    (diamtx-block-shape block-key 'Header 'Row brief style cell-width height)))

(define diamtx-block-col-header : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief0 style width height direction indices]
    (define brief (diamtx-rotate brief0 direction))
    (define cell-height
      (cond [(or brief) (max (+ (geo-height brief) (* (default-dia-block-margin) 2.0)) height)]
            [else height]))
    
    (diamtx-block-shape block-key 'Header 'Column brief style width cell-height)))

(define diamtx-block-corner : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief style width height direction indices]
    (diamtx-block-shape block-key 'Corner #false (diamtx-rotate brief direction) style width height)))

(define diamtx-block-entry : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief style width height direction indices]
    (diamtx-block-shape block-key 'Data 'Normal (diamtx-rotate brief direction) style width height)))

(define diamtx-block-hole : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief style width height direction indices]
    (diamtx-block-shape block-key 'Data 'Hole #false style width height)))

(define diamtx-block-mask : (Dia-Block-Create* Dia-Matrix-Urgent-Datum)
  (lambda [block-key brief style width height direction indices]
    (diamtx-block-shape block-key 'Data 'Mask #false style width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diamtx-rotate : (-> (Option Geo) (Option Flonum) (Option Geo))
  (lambda [brief direction]
    (cond [(or (not direction) (zero? direction)) brief]
          [else (and brief (geo-rotate brief direction))])))

(define diamtx-block-shape : (-> Symbol Symbol (Option Symbol) (Option Geo) Dia-Block-Style Nonnegative-Flonum Nonnegative-Flonum Dia:Block)
  (lambda [block-key type subtype brief style width height]
    (create-dia-block #:id block-key #:type type subtype
                      (geo-rectangle #:id (dia-block-shape-id block-key)
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height)
                      brief)))
