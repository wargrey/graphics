#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require colorspace/hdr)
(require colorspace/cie)
(require colorspace/cmyk)
(require colorspace/digitama/correction)

(require racket/unsafe/ops)

(require "enum.rkt")
(require "pixel.rkt")
(require "../resource/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-decoder : (-> (U Symbol Procedure) Bytes Positive-Index Positive-Index
                                PSD-Color-Mode Positive-Byte Positive-Byte PSD-Compression-Method
                                Bytes PSD-Image-Resources Positive-Byte
                                (U False Flonum (-> Flonum Flonum)) (U CMYK-Ink-Config CMYK->RGB)
                                Bitmap-Body-Decoder)
  (lambda [func image width height color-mode channels depth compression-method color-mode-data images-resources ps-size expose cmyk-ink]
    (define gamma-expose : (-> Flonum Flonum)
      (cond [(not expose) values]
            [(procedure? expose) (λ [[linear : Flonum]] (color-component-gamma-encode (expose linear)))]
            [(or (zero? expose) (not (rational? expose))) (λ [[linear : Flonum]] (color-component-gamma-encode linear))]
            [else (let ([λexpose (make-hdr-exposure expose)])
                    (λ [[linear : Flonum]] (color-component-gamma-encode (λexpose linear))))]))

    (define cmyk->rgb : CMYK->RGB
      (cond [(procedure? cmyk-ink) cmyk-ink]
            [else (λ [[c : Flonum] [m : Flonum] [y : Flonum] [k : Flonum]] : (Values Flonum Flonum Flonum)
                    (device-cmyk->rgb c m y k cmyk-ink))]))
    
    (cond [(eq? compression-method 'RLE)
           (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
             (let ([scanlines (psd-scanline-unpack image width height channels depth ps-size)])
               (psd-extract-pixels! func pixels scanlines width height stride
                                    color-mode channels depth color-mode-data
                                    gamma-expose cmyk->rgb)))]
          [(eq? compression-method 'Raw)
           (λ [[pixels : Bitmap-Pixels] [fxwidth : Positive-Index] [fxheight : Positive-Index] [stride : Positive-Index]] : Void
             (psd-extract-pixels! func pixels image width height stride
                                  color-mode channels depth color-mode-data
                                  gamma-expose cmyk->rgb))]
          [else (throw-unsupported-error (current-ioexn-input-port) func "compression method: ~a" compression-method)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-extract-pixels! : (-> (U Symbol Procedure) Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index
                                  PSD-Color-Mode Positive-Byte Positive-Byte Bytes (-> Flonum Flonum) CMYK->RGB Void)
  (lambda [func pixels planar-data width height stride color-mode channels depth color-mode-data expose cmyk->rgb]
    (or (cond [(eq? color-mode 'RGB)
               (when (= channels 3) (pix-fill! pixels #xFF (* stride height)))
               (and (>= channels 3)
                    (cond [(= depth 08)
                           (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                         pix-set-straight-argb-bytes! pix-set-rgb-bytes! unsafe-bytes-ref 1 values)]
                          [(= depth 16)
                           (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                         pix-set-straight-argb-flonums! pix-set-rgb-flonums! psd-fl16bpc-ref 2 values)]
                          [(= depth 32)
                           (psd-fill-argb-from-rgb-data! pixels planar-data width height stride channels
                                                         pix-set-straight-argb-flonums! pix-set-rgb-flonums! psd-fl32bpc-ref 4 expose)]
                       [else #false]))]
              [(eq? color-mode 'L*a*b*)
               (when (= channels 3) (pix-fill! pixels #xFF (* stride height)))
               (and (>= channels 3)
                    (< depth 32)
                    (psd-fill-argb-from-lab-data! pixels planar-data width height stride channels psd-fl8bpc-ref (psd-depth->size depth)))]
              [(eq? color-mode 'CMYK)
               (when (= channels 4) (pix-fill! pixels #xFF (* stride height)))
               (and (>= channels 4)
                    (< depth 32)
                    (psd-fill-argb-from-cmyk-data! pixels planar-data width height stride channels
                                                   psd-fl8bpc-ref (psd-depth->size depth) cmyk->rgb))]
              [(eq? color-mode 'Indexed)
               (and (>= channels 1)
                    (= depth 08)
                    (psd-fill-argb-from-indexed-data! pixels planar-data width height stride channels color-mode-data))]
              [(or (eq? color-mode 'Grayscale)

                   ; TODO: these two need expert knownledge in printing
                   (eq? color-mode 'Duotone)
                   (eq? color-mode 'Multichannel))
               (and (>= channels 1)
                    (cond [(= depth 08)
                           (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                               pix-set-straight-argb-bytes! pix-set-rgb-bytes! unsafe-bytes-ref 1 values)]
                          [(= depth 16)
                           (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                               pix-set-straight-argb-flonums! pix-set-rgb-flonums! psd-fl16bpc-ref 2 values)]
                          [(= depth 32)
                           (psd-fill-argb-from-grayscale-data! pixels planar-data width height stride channels
                                                               pix-set-straight-argb-flonums! pix-set-rgb-flonums! psd-fl32bpc-ref 4 expose)]
                          [else #false]))]
              [else #false])
        (throw-unsupported-error (current-ioexn-input-port) func "color mode: ~a; channels: ~a; depth: ~abpc." color-mode channels depth))))

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

    (when (= channels 1)
      (pix-fill! pixels #xFF (* stride height)))

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

(define psd-fill-argb-from-lab-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte
                                           (-> Bytes Fixnum Flonum) Positive-Byte Void)
  (lambda [pixels planar-data width height stride channels channel-ref depth-size]
    (define width-span : Index (unsafe-idx* width depth-size))
    (define channel-1span : Index (unsafe-idx* width-span height))
    (define channel-2span : Index (unsafe-idx* channel-1span 2))
    (define channel-3span : Index (unsafe-idx* channel-1span 3))

    (when (= channels 3)
      (pix-fill! pixels #xFF (* stride height)))

    (let fill-row! ([row : Nonnegative-Fixnum 0])
      (when (< row height)
        (let fill-col! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define rdata-idx (unsafe-fx+ (unsafe-fx* col depth-size) (unsafe-idx* row width-span)))
            (define pixel-idx (unsafe-fx+ (unsafe-fx* col 4) (unsafe-fx* row stride)))
            (define l (channel-ref planar-data rdata-idx))
            (define a (psd-lab-axis-adjust (channel-ref planar-data (unsafe-fx+ rdata-idx channel-1span))))
            (define b (psd-lab-axis-adjust (channel-ref planar-data (unsafe-fx+ rdata-idx channel-2span))))
            (define-values (R G B) (lab->rgb l a b))
            
            (if (>= channels 4)
                (let ([alpha (channel-ref planar-data (unsafe-fx+ rdata-idx channel-3span))])
                  (pix-set-straight-argb-flonums! pixels pixel-idx alpha R G B))
                (pix-set-rgb-flonums! pixels pixel-idx R G B))
            (fill-col! (+ col 1))))
        (fill-row! (+ row 1))))))

(define psd-fill-argb-from-cmyk-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte
                                            (-> Bytes Fixnum Flonum) Positive-Byte CMYK->RGB Void)
  (lambda [pixels planar-data width height stride channels channel-ref depth-size cmyk->rgb]
    (define width-span : Index (unsafe-idx* width depth-size))
    (define channel-1span : Index (unsafe-idx* width-span height))
    (define channel-2span : Index (unsafe-idx* channel-1span 2))
    (define channel-3span : Index (unsafe-idx* channel-1span 3))
    (define channel-4span : Index (unsafe-idx* channel-1span 4))

    (when (= channels 4)
      (pix-fill! pixels #xFF (* stride height)))

    (let fill-row! ([row : Nonnegative-Fixnum 0])
      (when (< row height)
        (let fill-col! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define rdata-idx (unsafe-fx+ (unsafe-fx* col depth-size) (unsafe-idx* row width-span)))
            (define pixel-idx (unsafe-fx+ (unsafe-fx* col 4) (unsafe-fx* row stride)))
            (define c (- 1.0 (channel-ref planar-data rdata-idx)))
            (define m (- 1.0 (channel-ref planar-data (unsafe-fx+ rdata-idx channel-1span))))
            (define y (- 1.0 (channel-ref planar-data (unsafe-fx+ rdata-idx channel-2span))))
            (define k (- 1.0 (channel-ref planar-data (unsafe-fx+ rdata-idx channel-3span))))
            (define-values (r g b) (cmyk->rgb c m y k))
            
            (if (>= channels 5)
                (let ([alpha (channel-ref planar-data (unsafe-fx+ rdata-idx channel-4span))])
                  (pix-set-straight-argb-flonums! pixels pixel-idx alpha r g b))
                (pix-set-rgb-flonums! pixels pixel-idx r g b))
            (fill-col! (+ col 1))))
        (fill-row! (+ row 1))))))

(define psd-fill-argb-from-indexed-data! : (-> Bitmap-Pixels Bytes Positive-Index Positive-Index Positive-Index Positive-Byte Bytes Void)
  (lambda [pixels planar-data width height stride channels planar-palatte]
    (define channel-1span : Index (unsafe-idx* width height))

    (pix-fill! pixels #xFF (* stride height))

    (unless (= (bytes-length planar-palatte) 768)
      (throw-read-error (current-ioexn-input-port) psd-fill-argb-from-indexed-data!
                        "invalid palatte size\n  expected: 768\n  read: ~a\n  body: ~s."
                        (bytes-length planar-palatte) planar-palatte))
    
    (let fill-row! ([row : Nonnegative-Fixnum 0])
      (when (< row height)
        (let fill-col! ([col : Nonnegative-Fixnum 0])
          (when (< col width)
            (define rdata-idx (unsafe-fx+ col (unsafe-idx* row width)))
            (define pixel-idx (unsafe-fx+ (unsafe-fx* col 4) (unsafe-fx* row stride)))
            (define color-idx (unsafe-bytes-ref planar-data rdata-idx))
            (define r (unsafe-bytes-ref planar-palatte color-idx))
            (define g (unsafe-bytes-ref planar-palatte (unsafe-fx+ color-idx 256)))
            (define b (unsafe-bytes-ref planar-palatte (unsafe-fx+ color-idx 512)))

            (pix-set-rgb-bytes! pixels pixel-idx r g b)
            (fill-col! (+ col 1))))
        (fill-row! (+ row 1))))))
