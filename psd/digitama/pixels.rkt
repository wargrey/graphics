#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require digimon/packbits)
(require racket/unsafe/ops)

(require "image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte
                                PSD-Compression-Method
                                Bitmap-Body-Decoder)
  (lambda [func planar-data width height color-mode channels depth compression-method]
    (unless (eq? color-mode 'RGB)
      (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a" color-mode))
    (unless (= depth 8)
      (throw-unsupported-error (current-ioexn-input-port) func "depth: ~a-bpc" depth))
    (unless (or (= channels 3) (= channels 4))
      (throw-unsupported-error (current-ioexn-input-port) func "channel count: ~a" channels))
    
    (case compression-method
      [(Raw) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (psd-extract-pixels! pixels planar-data width height stride channels))]
      [(RLE) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (let* ([scan-lines (* height channels)]
                      [intervals (nbytes-pairs (unsafe-fx* scan-lines 2) (parse-nsizes-list planar-data 0 2 scan-lines))])
                 (psd-extract-pixels! pixels
                                      (for/list : (Listof Bytes) ([interval (in-list intervals)])
                                        (unpackbits width planar-data (car interval) (cdr interval)))
                                      width height stride channels)))]
      [else (throw-unsupported-error (current-ioexn-input-port) func "compression method: ~a" compression-method)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-extract-pixels! : (-> Bitmap-Pixels (U Bytes (Listof Bytes)) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels]
    (when (= channels 3)
      (pix-fill! pixels #xFF (* stride height)))
    
    (if (bytes? planar-data)
        (psd-fill-argb-from-bytes! pixels planar-data width height stride channels)
        (psd-fill-argb-from-scanlines! pixels planar-data width height stride channels))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-fill-argb-from-bytes! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels]
    (define total (unsafe-fx* width height))

    (let fill! ([idx : Nonnegative-Fixnum 0])
      (when (unsafe-fx< idx total)
        (define row (unsafe-fxquotient  idx width))
        (define col (unsafe-fxremainder idx width))
        (define r (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* total 0))))
        (define g (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* total 1))))
        (define b (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* total 2))))
        (define pixel-idx (unsafe-fx+ (unsafe-fx* row stride) (unsafe-fx* col 4)))
        
        (if (= channels 4)
            (let ([a (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* total 3)))])
              (pix-set-argb-bytes! pixels pixel-idx a r g b))
            (pix-set-rgb-bytes! pixels pixel-idx r g b))
        (fill! (unsafe-fx+ idx 1))))))

(define psd-fill-argb-from-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels]
    (let fill! ([rest : (Listof Bytes) planar-data]
                [row : Nonnegative-Fixnum 0]
                [channel : Byte 0])
      (when (unsafe-fx< channel channels)
        (define offset : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (unsafe-fx< col width)
            (define pixel-idx (unsafe-fx+ offset (unsafe-fx* col 4)))
            
            (pix-set-rgba-channel-byte! pixels pixel-idx channel (unsafe-bytes-ref planar col))
            (subfill! (unsafe-fx+ col 1))))

        (if (unsafe-fx= (unsafe-fx+ row 1) height)
            (fill! (unsafe-cdr rest) 0 (unsafe-b+ channel 1))
            (fill! (unsafe-cdr rest) (unsafe-fx+ row 1) channel))))))
  