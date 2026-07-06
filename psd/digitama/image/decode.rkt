#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require colorspace/hdr)
(require colorspace/digitama/correction)

(require racket/case)
(require racket/unsafe/ops)

(require "enum.rkt")
(require "pixel.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte PSD-Compression-Method Positive-Byte
                                (U False Flonum (-> Flonum Flonum)) Bitmap-Body-Decoder)
  (lambda [func planar-data width height color-mode channels depth compression-method ps-size expose]
    (define gamma-expose
      (cond [(not expose) values]
            [(procedure? expose) (λ [[linear : Flonum]] (color-component-gamma-encode (expose linear)))]
            [(or (zero? expose) (not (rational? expose))) (λ [[linear : Flonum]] (color-component-gamma-encode linear))]
            [else (let ([λexpose (make-hdr-exposure expose)])
                    (λ [[linear : Flonum]] (color-component-gamma-encode (λexpose linear))))]))
    
    (case compression-method
      [(Raw) ; NOTE: compression is usually meaningless for higher depth image
       (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
         (psd-extract-pixels! func pixels planar-data width height stride color-mode channels depth gamma-expose))]
      [(RLE) ; NOTE: this path is hot for normal cases
       (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
         (let ([scanlines (psd-scanline-unpack planar-data width height channels ps-size)])
           (psd-extract-pixels! func pixels scanlines width height stride color-mode channels depth gamma-expose)))]
      [else (throw-unsupported-error (current-ioexn-input-port) func "compression method: ~a" compression-method)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-extract-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes)) Positive-Index Positive-Index Positive-Index
                                  PSD-Color-Mode Positive-Byte Positive-Byte (-> Flonum Flonum) Void)
  (lambda [func pixels planar-data width height stride color-mode channels depth expose]
    (or (case/eq color-mode
          [(RGB) (psd-extract-rgb-pixels! func pixels planar-data width height stride channels depth expose)]
          [(Grayscale) (psd-extract-grayscale-pixels! func pixels planar-data width height stride channels depth expose)]
          [else #false])
        (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a; channels: ~a; depth: ~abpc." color-mode channels depth))))

(define psd-extract-grayscale-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes))
                                            Positive-Index Positive-Index Positive-Index
                                            Positive-Byte Positive-Byte (-> Flonum Flonum) (Option Void))
  (lambda [func pixels planar-data width height stride channels depth expose]
    (when (>= channels 1)
      (when (= channels 1) (pix-fill! pixels #xFF (* stride height)))
      (if (bytes? planar-data)
          (case/eq depth
            [(08) (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                      pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                      unsafe-bytes-ref 1 values)]
            [(16) (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                      pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                      psd-16bpc-ref 2 values)]
            [(32) (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                      pix-set-straight-argb-flonums! pix-set-rgb-flonums!
                                                      psd-32bpc-ref 4 expose)]
            [else #false])
          (case/eq depth
            [(08) (psd-fill-argb-from-scanlines! pixels planar-data width height stride (min 2 channels)
                                                 (if (= channels 1) pix-set-grayscale-channel-byte! pix-set-straight-grayscale-channel-byte!)
                                                 unsafe-bytes-ref 1 values 2)]
            [(16) (psd-fill-argb-from-scanlines! pixels planar-data width height stride (min 2 channels)
                                                 (if (= channels 1) pix-set-grayscale-channel-byte! pix-set-straight-grayscale-channel-byte!)
                                                 psd-16bpc-ref 2 values 2)]
            [(32) (psd-fill-argb-from-scanlines! pixels planar-data width height stride (min 2 channels)
                                                 (if (= channels 1) pix-set-grayscale-channel-flonum! pix-set-straight-grayscale-channel-flonum!)
                                                 psd-32bpc-ref 4 expose 2)]
            [else #false])))))

(define psd-extract-rgb-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels (U Bytes (Listof Bytes))
                                      Positive-Index Positive-Index Positive-Index
                                      Positive-Byte Positive-Byte (-> Flonum Flonum) (Option Void))
  (lambda [func pixels planar-data width height stride channels depth expose]
    (when (>= channels 3)
      (when (= channels 3) (pix-fill! pixels #xFF (* stride height)))
      (if (bytes? planar-data)
          (case/eq depth
            [(08) (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                unsafe-bytes-ref 1 values)]
            [(16) (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                psd-16bpc-ref 2 values)]
            [(32) (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                pix-set-straight-argb-flonums! pix-set-rgb-flonums!
                                                psd-32bpc-ref 4 expose)]
            [else #false])
          (case/eq depth
            [(08) (psd-fill-argb-from-rgb-scanlines! pixels planar-data width height stride channels)]
            [(16) (psd-fill-argb-from-scanlines! pixels planar-data width height stride (min 4 channels)
                                                 (if (= channels 3) pix-set-rgba-channel-byte! pix-set-straight-rgba-channel-byte!)
                                                 psd-16bpc-ref 2 values 4)]
            [(32) (psd-fill-argb-from-scanlines! pixels planar-data width height stride (min 4 channels)
                                                 (if (= channels 3) pix-set-rgba-channel-flonum! pix-set-straight-rgba-channel-flonum!)
                                                 psd-32bpc-ref 4 expose 4)]
            [else #false])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) psd-fill-argb-from-rgb-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte
                                                        (-> Bitmap-Pixels Natural T T T T Void) (-> Bitmap-Pixels Natural T T T Void)
                                                        (-> Bytes Fixnum T) Positive-Byte (-> T T) Void)
  (lambda [pixels planar-data width height stride channels pix-straight-set! pix-set! channel-ref depth-size adjust]
    (define width-span : Index (unsafe-idx* width depth-size))
    (define channel-1span : Index (unsafe-idx* width-span height))
    (define channel-2span : Index (unsafe-idx* channel-1span 2))
    (define channel-3span : Index (unsafe-idx* channel-1span 3))
    
    (let fill-row! ([row : Nonnegative-Fixnum 0])
      (when (< row height)
        (let fill-col! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define rdata-idx (unsafe-fx+ (unsafe-fx* col depth-size) (unsafe-idx* row width-span)))
            (define pixel-idx (unsafe-fx+ (unsafe-fx* col 4) (unsafe-fx* row stride)))
            (define r (adjust (channel-ref planar-data rdata-idx)))
            (define g (adjust (channel-ref planar-data (unsafe-fx+ rdata-idx channel-1span))))
            (define b (adjust (channel-ref planar-data (unsafe-fx+ rdata-idx channel-2span))))
            
            (if (>= channels 4)
                (let ([a (channel-ref planar-data (unsafe-fx+ rdata-idx channel-3span))])
                  (pix-straight-set! pixels pixel-idx a r g b))
                (pix-set! pixels pixel-idx r g b))
            (fill-col! (+ col 1))))
        (fill-row! (+ row 1))))))

(define #:forall (T) psd-fill-argb-from-grayscale-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte
                                                              (-> Bitmap-Pixels Natural T T T T Void) (-> Bitmap-Pixels Natural T T T Void)
                                                              (-> Bytes Fixnum T) Positive-Byte (-> T T) Void)
  (lambda [pixels planar-data width height stride channels pix-straight-set! pix-set! channel-ref depth-size adjust]
    (define width-span : Index (unsafe-idx* width depth-size))
    (define channel-1span : Index (unsafe-idx* width-span height))

    (let fill-row! ([row : Nonnegative-Fixnum 0])
      (when (< row height)
        (let fill-col! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define rdata-idx (unsafe-fx+ (unsafe-fx* col depth-size) (unsafe-idx* row width-span)))
            (define pixel-idx (unsafe-fx+ (unsafe-fx* col 4) (unsafe-fx* row stride)))
            (define grey (adjust (channel-ref planar-data rdata-idx)))
            
            (if (>= channels 2)
                (let ([a (channel-ref planar-data (unsafe-fx+ rdata-idx channel-1span))])
                  (pix-straight-set! pixels pixel-idx a grey grey grey))
                (pix-set! pixels pixel-idx grey grey grey))
            (fill-col! (+ col 1))))
        (fill-row! (+ row 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The planar-data is stored line by line and channel by channel
;     whoever designed this deserve negative rewards forever.
;   We have to fill all alpha values first for premultiplication if it exists,
;     so all the 3 aspects have to be dealed with in reverse order.

(define psd-fill-argb-from-rgb-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte Void)
  (lambda [pixels senilnacs width height stride channels]
    (define height-- : Index (- height 1))
    (define pix-set! (if (= channels 3) pix-set-rgba-channel-byte! pix-set-straight-rgba-channel-byte!))
    
    (let fill! ([rest : (Listof Bytes) senilnacs]
                [row : Index height--]
                [channel : Byte (min 4 channels)])
      (when (> channel 0)
        (define channel-- : Byte (- channel 1))
        (define channel-idx0 : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define pixel-idx (unsafe-fx+ channel-idx0 (unsafe-fx* col 4)))

            ;;; this function knows how to deal with the alpha channel
            (pix-set! pixels pixel-idx channel-- (unsafe-bytes-ref planar col))
            (subfill! (+ col 1))))

        (if (= row 0)
            (fill! (unsafe-cdr rest) height-- channel--)
            (fill! (unsafe-cdr rest) (- row 1) channel))))))

(define #:forall (T) psd-fill-argb-from-scanlines! : (-> Bitmap-Pixels (Listof Bytes) Positive-Index Positive-Index Positive-Index Positive-Byte
                                                         (-> Bitmap-Pixels Nonnegative-Fixnum Byte T Void) (-> Bytes Fixnum T) Positive-Byte
                                                         (-> T T) Positive-Byte 
                                                         Void)
  (lambda [pixels senilnacs width height stride channels pix-set! channel-ref depth-size adjust alpha-channel-id]
    (define height-- : Index (- height 1))
    
    (let fill! ([rest : (Listof Bytes) senilnacs]
                [row : Index height--]
                [channel : Byte channels])
      (when (> channel 0)
        (define channel-- : Byte (- channel 1))
        (define channel-idx0 : Nonnegative-Fixnum (unsafe-fx* row stride))
        (define planar : Bytes (unsafe-car rest))

        (let subfill! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define pixel-idx (unsafe-fx+ channel-idx0 (unsafe-fx* col 4)))
            (define c (channel-ref planar (unsafe-idx* col depth-size)))

            ;;; this function knows how to deal with the alpha channel
            (pix-set! pixels pixel-idx channel-- (if (= channel alpha-channel-id) c (adjust c)))
            (subfill! (+ col 1))))

        (if (= row 0)
            (fill! (unsafe-cdr rest) height-- channel--)
            (fill! (unsafe-cdr rest) (- row 1) channel))))))
