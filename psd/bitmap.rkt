#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (all-from-out colorspace/hdr))

(require bitmap/stdio)
(require colorspace/hdr)
(require colorspace/cmyk)

(require "digitama/stdin.rkt")
(require "digitama/image/decode.rkt")
(require "digitama/resource/parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-read-bitmap psd-bitmap #:-> Bitmap
  (lambda [/dev/psdin density
                      #:hdr-expose [expose : (U False Flonum (-> Flonum Flonum)) #false]
                      #:cmyk-ink [cmyk-ink : (U CMYK->RGB CMYK-Ink-Config) 'US]]
    (define-values (ps-size channels height width depth color-mode) (read-psd-header /dev/psdin))
    (define-values (color-mode-data image-resource-data) (read-psd-image-resources /dev/psdin))
    (define-values (compression-method image-data) (read-psd-composite-image /dev/psdin (skip-psd-layer-section /dev/psdin ps-size)))
    
    (create-bitmap Bitmap /dev/psdin density width height
                   (psd-image-decoder 'read-psd-bitmap image-data width height
                                      color-mode channels depth compression-method
                                      color-mode-data (psd-image-resources-parse image-resource-data density)
                                      ps-size expose cmyk-ink))))
