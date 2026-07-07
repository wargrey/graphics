#lang typed/racket/base

(provide (all-defined-out))

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

(define psd-16bpc-ref : (-> Bytes Fixnum Byte)
  (lambda [planar col]
    (define 16bits (parse-muint16 planar col))
    (define 8bits (unsafe-idxrshift 16bits 8))

    (if (> 8bits #xFF) #xFF 8bits)))

(define psd-32bpc-ref : (-> Bytes Fixnum Flonum)
  (lambda [planar col]
    (parse-mfloat planar col)))
