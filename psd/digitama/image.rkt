#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-color-mode #:+> PSD-Color-Mode ; order matters
  color-mode->integer integer->color-mode
  [0 Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab])

(define-enumeration* psd-compression-method #:+> PSD-Compression-Method ; order matters
  compression-method->integer integer->compression-method
  [0 Raw RLE ZIP ZIP/prediction])

(define-enumeration psd-blend-mode : PSD-Blend-Mode
  ; http://photoblogstop.com/photoshop/photoshop-blend-modes-explained
  [pass norm diss dark mul idiv lbrn dkcl lite scrn
        div lddg lgcl over slit hlit vlit llit pLit
        hmix diff smud fsub fdiv hue sat colr lum])
