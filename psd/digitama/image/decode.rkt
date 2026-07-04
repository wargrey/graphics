#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require digimon/packbits)
(require racket/unsafe/ops)

(require "enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte PSD-Compression-Method
                                Bitmap-Body-Decoder)
  (lambda [func planar-data width height color-mode channels depth compression-method]
    (unless (eq? color-mode 'RGB)
      (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a" color-mode))
    (unless (= depth 8)
      (throw-unsupported-error (current-ioexn-input-port) func "depth: ~a-bpc" depth))
    (unless (or (= channels 3) (= channels 4))
      (throw-unsupported-error (current-ioexn-input-port) func "channel count: ~a" channels))

    (: reverse-unpack (->* ((Listof (Pairof Integer Integer))) ((Listof Bytes)) (Listof Bytes)))
    (define (reverse-unpack rest [ranalp-data null])
      (if (pair? rest)
          (let-values ([(self rest--) (values (car rest) (cdr rest))])
            (reverse-unpack rest--
                            (cons (unpackbits width planar-data (car self) (cdr self))
                                  ranalp-data)))
          
          ; Yes, we need the reversed planar-data for dealing with the alpha channel first
          ranalp-data))
    
    (case compression-method
      [(Raw) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (psd-extract-pixels! pixels planar-data width height stride channels))]
      [(RLE) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (let* ([scan-lines (* height channels)]
                      [intervals (nbytes-pairs (unsafe-fx* scan-lines 2) (parse-nsizes-list planar-data 0 2 scan-lines))])
                 (psd-extract-pixels! pixels (reverse-unpack intervals) width height stride channels)))]
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
              (pix-set-straight-argb-bytes! pixels pixel-idx a r g b))
            (pix-set-rgb-bytes! pixels pixel-idx r g b))
        (fill! (unsafe-fx+ idx 1))))))

(define psd-fill-argb-from-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels ranalp-data width height stride channels]
    ;;; The planar-data is stored line by line and channel by channel
    ;     whoever the designed this deserve negative rewards forever. 
    ;   We have to fill all alpha values first for premultiplication if it exists,
    ;     so all the 3 aspects have to be dealed with in reverse order.
    
    (define height-- : Index (- height 1))
    
    (let fill! ([rest : (Listof Bytes) ranalp-data]
                [row : Index height--]
                [channel : Byte channels])
      (when (unsafe-fx> channel 0)
        (define channel-- : Byte (- channel 1))
        (define offset : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (unsafe-fx< col width)
            (define pixel-idx (unsafe-fx+ offset (unsafe-fx* col 4)))

            ;;; this function knows how to deal with the alpha channel
            (pix-set-straight-rgba-channel-byte! pixels pixel-idx channel-- (unsafe-bytes-ref planar col))
            (subfill! (unsafe-fx+ col 1))))

        (if (unsafe-fx= row 0)
            (fill! (unsafe-cdr rest) height-- channel--)
            (fill! (unsafe-cdr rest) (- row 1) channel))))))
