#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)

(require bitmap/stdio)
(require digimon/packbits)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-scanline-unpack : (-> Bytes Positive-Index Positive-Index Positive-Byte Positive-Byte (Listof Bytes))
  (lambda [planar-data width height channels ps-size]
    (define rle-size : Byte (quotient ps-size 2))
    (define scan-lines : Index (unsafe-idx* height channels))
    (define intervals : (Listof (Pairof Integer Integer))
      (nbytes-pairs (unsafe-fx* scan-lines rle-size)
                    (parse-mnsizes-list planar-data 0 rle-size scan-lines)))
    
    (let reverse-unpack ([rest : (Listof (Pairof Integer Integer)) intervals]
                         [senilnacs : (Listof Bytes) null])
      (if (pair? rest)
          (let-values ([(self rest--) (values (car rest) (cdr rest))])
            (reverse-unpack rest--
                            (cons (unpackbits width planar-data (car self) (cdr self))
                                  senilnacs)))
          
          ; Yes, we need the reversed planar-data for dealing with the alpha channel first
          senilnacs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-16bpc-ref : (-> Bytes Fixnum Byte)
  (lambda [planar col]
    (define 16bits (parse-muint16 planar col))
    (define 8bits (unsafe-idxrshift 16bits 8))

    (if (> 8bits #xFF) #xFF 8bits)))

(define psd-32bpc-ref : (-> Bytes Fixnum Flonum)
  (lambda [planar col]
    (parse-mfloat planar col)))
