#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require colorspace/hdr)
(require colorspace/digitama/correction)

(require racket/unsafe/ops)

(require "enum.rkt")
(require "pixel.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte PSD-Compression-Method Positive-Byte
                                (U False Flonum (-> Flonum Flonum)) Bitmap-Body-Decoder)
  (lambda [func image width height color-mode channels depth compression-method ps-size expose]
    (define gamma-expose : (-> Flonum Flonum)
      (cond [(not expose) values]
            [(procedure? expose) (λ [[linear : Flonum]] (color-component-gamma-encode (expose linear)))]
            [(or (zero? expose) (not (rational? expose))) (λ [[linear : Flonum]] (color-component-gamma-encode linear))]
            [else (let ([λexpose (make-hdr-exposure expose)])
                    (λ [[linear : Flonum]] (color-component-gamma-encode (λexpose linear))))]))
    
    (cond [(eq? compression-method 'RLE)
           (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
             (let ([scanlines (psd-scanline-unpack image width height channels depth ps-size)])
               (psd-extract-pixels! func pixels scanlines width height stride color-mode channels depth gamma-expose)))]
          [(eq? compression-method 'Raw)
           (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
             (psd-extract-pixels! func pixels image width height stride color-mode channels depth gamma-expose))]
          [else (throw-unsupported-error (current-ioexn-input-port) func "compression method: ~a" compression-method)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-extract-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index
                                  PSD-Color-Mode Positive-Byte Positive-Byte (-> Flonum Flonum) Void)
  (lambda [func pixels planar-data width height stride color-mode channels depth expose]
    (or (cond [(eq? color-mode 'RGB) (psd-extract-rgb-pixels! func pixels planar-data width height stride channels depth expose)]
              [(eq? color-mode 'Grayscale) (psd-extract-grayscale-pixels! func pixels planar-data width height stride channels depth expose)]
              [else #false])
        (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a; channels: ~a; depth: ~abpc." color-mode channels depth))))

(define psd-extract-grayscale-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index
                                            Positive-Byte Positive-Byte (-> Flonum Flonum) (Option Void))
  (lambda [func pixels planar-data width height stride channels depth expose]
    (when (>= channels 1)
      (when (= channels 1) (pix-fill! pixels #xFF (* stride height)))

      (cond [(= depth 08)
             (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                 pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                 unsafe-bytes-ref 1 values)]
            [(= depth 16)
             (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                 pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                                 psd-16bpc-ref 2 values)]
            [(= depth 32)
             (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                 pix-set-straight-argb-flonums! pix-set-rgb-flonums!
                                                 psd-32bpc-ref 4 expose)]
            [else #false]))))

(define psd-extract-rgb-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index
                                      Positive-Byte Positive-Byte (-> Flonum Flonum) (Option Void))
  (lambda [func pixels planar-data width height stride channels depth expose]
    (when (>= channels 3)
      (when (= channels 3) (pix-fill! pixels #xFF (* stride height)))

      (cond [(= depth 08)
             (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                           pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                           unsafe-bytes-ref 1 values)]
            [(= depth 16)
             (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                           pix-set-straight-argb-bytes! pix-set-rgb-bytes!
                                           psd-16bpc-ref 2 values)]
            [(= depth 32)
             (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                           pix-set-straight-argb-flonums! pix-set-rgb-flonums!
                                           psd-32bpc-ref 4 expose)]
            [else #false]))))

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
