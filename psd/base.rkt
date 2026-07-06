#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (all-from-out colorspace/hdr))

(provide (rename-out [bitmap-intrinsic-size psd-size]
                     [bitmap-intrinsic-width psd-header-width]
                     [bitmap-intrinsic-height psd-header-height]
                     [bitmap-density psd-file-density]
                     [bitmap-source psd-file-name]))

(provide (rename-out [psd-body-resources psd-image-resources]
                     [psd-body-layers psd-layers]
                     [psd-body-global-mask psd-global-mask]
                     [psd-body-tagged-blocks psd-tagged-blocks]))

(require bitmap/stdio)
(require colorspace/hdr)

(require "digitama/self.rkt")
(require "digitama/stdin.rkt")

(require "digitama/image/enum.rkt")
(require "digitama/image/decode.rkt")

(require "digitama/resource/format.rkt")
(require "digitama/resource/parser.rkt")

(require "digitama/layer/parser.rkt")
(require "digitama/layer/block.rkt")
(require "digitama/layer/block/parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd psd-body () #:type-name PSD #:transparent)
(struct psb psd () #:type-name PSB #:transparent)

(define-read-bitmap psd #:-> PSD
  (lambda [/dev/psdin density #:hdr-expose [expose : (U False Flonum (-> Flonum Flonum)) #false]]
    (define-values (ps-size channels height width depth color-mode) (read-psd-header /dev/psdin))
    (define-values (color-mode-data image-resources layer-info gmask-info tagged-blocks image-pos) (read-psd-subsection /dev/psdin ps-size))
    (define-values (compression-method image-data) (read-psd-composite-image /dev/psdin image-pos))
    
    (create-bitmap (if (= ps-size 4) PSD PSB) /dev/psdin density width height channels depth color-mode compression-method ps-size
                   color-mode-data
                   (psd-image-resources-parse image-resources density)
                   (if (not layer-info) null (psd-layers-parse layer-info ps-size density))
                   (and gmask-info (parse-global-mask-info gmask-info))
                   (if (not tagged-blocks) psd-empty-tagged-blocks (psd-tagged-blocks-parse tagged-blocks ps-size))
                   (psd-image-decoder 'read-psd image-data
                                      width height color-mode channels depth
                                      compression-method ps-size
                                      expose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-depth : (-> PSD (Values Positive-Byte Positive-Byte))
  (lambda [self]
    (values (psd-header-depth self)
            (psd-header-channels self))))

(define psd-color-mode : (-> PSD PSD-Color-Mode)
  (lambda [self]
    (psd-header-color-mode self)))

(define psd-thumbnail-bitmap : (-> PSD False)
  (lambda [self]
    (define maybe-preview : (Option PSD:Res:Thumbnail) (psd-thumbnail self))
    (and maybe-preview (psd:res:thumbnail-image maybe-preview))

    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) psd-resource-ref : (case-> [PSD Integer -> (Option PSD-Resource)]
                                                [PSD Integer (-> Any Boolean : #:+ T) -> (Option T)])
  (case-lambda
    [(self id) (hash-ref (psd-body-resources self) id (λ [] #false))]
    [(self id pred?)
     (let ([res (psd-resource-ref self id)])
       (and res (pred? res) res))]))

(define psd-grid+guides : (-> PSD (Option PSD:Res:Grid+Guides)) (λ [self] (psd-resource-ref self #x408 psd:res:grid+guides?)))
(define psd-thumbnail : (-> PSD (Option PSD:Res:Thumbnail)) (λ [self] (psd-resource-ref self #x40C psd:res:thumbnail?)))
(define psd-file-info : (-> PSD (Option PSD:Res:File:Info)) (λ [self] (psd-resource-ref self #x424 psd:res:file:info?)))
