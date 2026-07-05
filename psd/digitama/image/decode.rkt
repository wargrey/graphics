#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require digimon/packbits)

(require racket/case)
(require racket/unsafe/ops)

(require "enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte PSD-Compression-Method Positive-Byte
                                Bitmap-Body-Decoder)
  (lambda [func planar-data width height color-mode channels depth compression-method ps-size]
    (define rle-size : Byte (quotient ps-size 2))
    (case compression-method
      [(Raw) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (psd-extract-pixels! func pixels planar-data width height stride color-mode channels depth))]
      [(RLE) (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
               (let* ([scan-lines (unsafe-idx* height channels)]
                      [intervals (nbytes-pairs (unsafe-fx* scan-lines rle-size) (parse-mnsizes-list planar-data 0 rle-size scan-lines))])
                 (psd-extract-pixels! func pixels (psd-unpack-scanlines planar-data width intervals)
                                      width height stride color-mode channels depth)))]
      [else (throw-unsupported-error (current-ioexn-input-port) func "compression method: ~a" compression-method)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-extract-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes)) Positive-Index Positive-Index Positive-Index
                                  PSD-Color-Mode Positive-Byte Positive-Byte Void)
  (lambda [func pixels planar-data width height stride color-mode channels depth]
    (case/eq color-mode
      [(RGB) (psd-extract-rgb-pixels! func pixels planar-data width height stride channels depth)]
      [(Grayscale) (psd-extract-grayscale-pixels! func pixels planar-data width height stride channels depth)]
      [else (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a; channels: ~a; depth: ~a." color-mode channels depth)])))

(define psd-extract-grayscale-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes))
                                            Positive-Index Positive-Index Positive-Index
                                            Positive-Byte Positive-Byte Void)
  (lambda [func pixels planar-data width height stride channels depth]
    (unless (= depth 8) (throw-unsupported-error (current-ioexn-input-port) func "depth: ~a-bpc" depth))

    (when (>= channels 1)
      (when (= channels 1) (pix-fill! pixels #xFF (* stride height)))
      (if (bytes? planar-data)
          (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels)
          (psd-fill-argb-from-scanlines! pixels planar-data width height stride channels)))))

(define psd-extract-rgb-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes))
                                      Positive-Index Positive-Index Positive-Index
                                      Positive-Byte Positive-Byte Void)
  (lambda [func pixels planar-data width height stride channels depth]
    (unless (= depth 8) (throw-unsupported-error (current-ioexn-input-port) func "depth: ~a-bpc" depth))

    (when (>= channels 3)
      (when (= channels 3) (pix-fill! pixels #xFF (* stride height)))
      (if (bytes? planar-data)
          (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels)
          (psd-fill-argb-from-rgb-scanlines! pixels planar-data width height stride channels)))))

(define psd-unpack-scanlines : (-> Bytes Positive-Index (Listof (Pairof Integer Integer)) (Listof Bytes))
  (lambda [planar-data width intervals]
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
(define psd-fill-argb-from-grayscale-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels]
    (define comp-span : Index (unsafe-idx* width height))

    (let fill! ([idx : Nonnegative-Fixnum 0])
      (when (< idx comp-span)
        (define row (unsafe-fxquotient  idx width))
        (define col (unsafe-fxremainder idx width))
        (define pixel-idx (unsafe-fx+ (unsafe-fx* row stride) (unsafe-fx* col 4)))
        (define gs (unsafe-bytes-ref planar-data idx))
        
        (if (= channels 2)
            (let ([a (unsafe-bytes-ref planar-data (unsafe-fx+ idx comp-span))])
              (pix-set-argb-bytes! pixels pixel-idx a gs gs gs))
            (pix-set-rgb-bytes! pixels pixel-idx gs gs gs))
        (fill! (+ idx 1))))))

(define psd-fill-argb-from-rgb-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels]
    (define comp-span : Index (unsafe-idx* width height))

    (let fill! ([idx : Nonnegative-Fixnum 0])
      (when (< idx comp-span)
        (define row (unsafe-fxquotient  idx width))
        (define col (unsafe-fxremainder idx width))
        (define pixel-idx (unsafe-fx+ (unsafe-fx* row stride) (unsafe-fx* col 4)))
        (define r (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* comp-span 0))))
        (define g (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* comp-span 1))))
        (define b (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* comp-span 2))))
        
        (if (= channels 4)
            (let ([a (unsafe-bytes-ref planar-data (unsafe-fx+ idx (unsafe-fx* comp-span 3)))])
              (pix-set-straight-argb-bytes! pixels pixel-idx a r g b))
            (pix-set-rgb-bytes! pixels pixel-idx r g b))
        (fill! (+ idx 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The planar-data is stored line by line and channel by channel
;     whoever designed this deserve negative rewards forever.
;   We have to fill all alpha values first for premultiplication if it exists,
;     so all the 3 aspects have to be dealed with in reverse order.    
(define psd-fill-argb-from-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels senilnacs width height stride channels]
    (define height-- : Index (- height 1))
    (define pix-set! (if (= channels 1) pix-set-monochromic-channel-byte! pix-set-straight-monochromic-channel-byte!))
    
    (let fill! ([rest : (Listof Bytes) senilnacs]
                [row : Index height--]
                [channel : Byte (min 2 channels)])
      (when (> channel 0)
        (define channel-- : Byte (- channel 1))
        (define offset : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define pixel-idx (unsafe-fx+ offset (unsafe-fx* col 4)))

            ;;; this function knows how to deal with the alpha channel
            (pix-set! pixels pixel-idx channel-- (unsafe-bytes-ref planar col))
            (subfill! (+ col 1))))

        (if (= row 0)
            (fill! (unsafe-cdr rest) height-- channel--)
            (fill! (unsafe-cdr rest) (- row 1) channel))))))

(define psd-fill-argb-from-rgb-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels senilnacs width height stride channels]
    (define height-- : Index (- height 1))
    (define pix-set! (if (= channels 3) pix-set-rgba-channel-byte! pix-set-straight-rgba-channel-byte!))
    
    (let fill! ([rest : (Listof Bytes) senilnacs]
                [row : Index height--]
                [channel : Byte (min 4 channels)])
      (when (> channel 0)
        (define channel-- : Byte (- channel 1))
        (define offset : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define pixel-idx (unsafe-fx+ offset (unsafe-fx* col 4)))

            ;;; this function knows how to deal with the alpha channel
            (pix-set! pixels pixel-idx channel-- (unsafe-bytes-ref planar col))
            (subfill! (+ col 1))))

        (if (= row 0)
            (fill! (unsafe-cdr rest) height-- channel--)
            (fill! (unsafe-cdr rest) (- row 1) channel))))))
