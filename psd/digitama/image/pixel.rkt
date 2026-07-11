#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [parse-mfloat psd-fl32bpc-ref]))

(require bitmap/stdio)
(require digimon/packbits)

;;; Scanline Planar Data format
; L1: RRRRR
; Ln: RRRRR
; L1: GGGGG
; Ln: GGGGG
; L1: BBBBB
; Ln: BBBBB
; ...

;;; Raw Bytes format
; L1-Ln: RRRRRRRRRR
; L1-Ln: GGGGGGGGGG
; L1-Ln: BBBBBBBBBB
; L1-Ln: ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-scanline-unpack : (-> Bytes Positive-Index Positive-Index Positive-Byte Positive-Byte Positive-Byte Bytes)
  (lambda [planar-data width height channels depth ps-size]
    (define depth-size : Positive-Byte (psd-depth->size depth))
    (define width-span : Index (unsafe-idx* width depth-size))
    (define chnnl-span : Index (unsafe-idx* width-span height))

    (define rle-size : Byte (quotient ps-size 2))
    (define N : Index (unsafe-idx* height channels))
    (define raw-bytes : Bytes (make-bytes (unsafe-idx* width-span N)))

    (let fill-raw! ([rest : (Listof Index) (parse-mnsize-list planar-data 0 rle-size N)]
                    [sL-idx : Index (unsafe-idx* rle-size N)]
                    [row-idx : Index 0]
                    [chl-idx : Index 0])
      (when (pair? rest)
        (define rest-- : (Listof Index) (cdr rest))
        (define sL-idx++ : Index (unsafe-idx+ sL-idx (car rest)))
        (define rowoff++ : Index (unsafe-idx+ row-idx width-span))
        
        (unpackbits! raw-bytes (unsafe-idx+ row-idx chl-idx)
                     planar-data sL-idx sL-idx++)

        (if (= rowoff++ chnnl-span)
            (fill-raw! rest-- sL-idx++ 0 (unsafe-idx+ chl-idx chnnl-span))
            (fill-raw! rest-- sL-idx++ rowoff++ chl-idx))))

    raw-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-depth->size : (-> Positive-Byte Positive-Byte)
  (lambda [depth]
    (cond [(= depth 8) 1]
          [(= depth 16) 2]
          [(= depth 32) 4]
          [else 1])))

(define psd-lab-axis-adjust : (-> Flonum Flonum)
  (λ [v]
    (- (* v 255.0) 128.0)))

(define psd-fl8bpc-ref : (-> Bytes Fixnum Flonum)
  (lambda [planar col]
    (real->double-flonum
     (/ (parse-muint8 planar col)
        #xFF))))

;;; WARNING
; No matter how Adoble claimed that 16bit is actually 15bit + 1bit,
;   the values stored in file are full 16bit.
; Adobe's API provides 15bit values and uses them in memory,
;   but it has nothing to do with the way they store the values.
(define psd-fl16bpc-ref : (-> Bytes Fixnum Flonum)
  (lambda [planar col]
    (real->double-flonum
     (/ (parse-muint16 planar col)
        #xFFFF))))
