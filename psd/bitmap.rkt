#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require bitmap/stdio)

(require "digitama/stdin.rkt")
(require "digitama/image/decode.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-read-bitmap psd-bitmap #:-> Bitmap
  (lambda [/dev/psdin density]
    (define-values (ps-size channels height width depth color-mode) (read-psd-header /dev/psdin))
    (define-values (compression-method image-data) (read-psd-composite-image /dev/psdin (skip-psd-subsection /dev/psdin ps-size)))
    
    (create-bitmap Bitmap /dev/psdin density width height
                   (psd-image-decoder read-psd-bitmap image-data
                                      width height color-mode channels depth
                                      compression-method ps-size))))
