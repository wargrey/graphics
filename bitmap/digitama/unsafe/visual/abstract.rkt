#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer? abstract-surface?]
                       [cpointer? svg-surface?]
                       [cpointer? pdf-surface?]))

  (require "../pangocairo.rkt")
  (require "../surface/image.rkt")
  
  (require "../stream/pdf.rkt")
  (require "../stream/svg.rkt")
  (require "../stream/png.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (abstract-surface-content-bbox sfc)
    (define-values (scale? lx ty w h) (cairo_recording_surface_get_extents sfc))

    (if (not scale?)
        (cairo_recording_surface_ink_extents sfc)
        (values lx ty w h)))

  (define (abstract-surface->stream-bytes sfc format name density)
    (define /dev/sfcout (open-output-bytes name))
    (abstract-surface-save sfc /dev/sfcout format density)
    (get-output-bytes /dev/sfcout))

  (define (abstract-surface-save sfc /dev/sfcout format density)
    (case format
      [(svg) (abstract-surface-save-with cairo-svg-stream-write /dev/sfcout sfc)]
      [(pdf) (abstract-surface-save-with cairo-pdf-stream-write /dev/sfcout sfc)]
      [else  (cairo-png-stream-write /dev/sfcout (λ [] (abstract-surface->image-surface sfc density)))]))

  (define (abstract-surface->image-surface abs-sfc density)
    (define-values (lx ty flwidth flheight) (abstract-surface-content-bbox abs-sfc))
    (define-values (png-sfc _width _height) (cairo-create-image-surface flwidth flheight density))
    (define png-cr (cairo_create png-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale png-cr density density))

    (cairo_set_source_surface png-cr abs-sfc (unsafe-fl- 0.0 lx) (unsafe-fl- 0.0 ty))
    (cairo_paint png-cr)
    (cairo_destroy png-cr)

    (values png-sfc #true))

  (define (abstract-surface-save-with stream-write /dev/strout abs-sfc)
    (define-values (lx ty width height) (abstract-surface-content-bbox abs-sfc))

    (stream-write /dev/strout width height
                  (λ [cr flwidth flheight]
                    (cairo_set_source_surface cr abs-sfc (unsafe-fl- 0.0 lx) (unsafe-fl- 0.0 ty))
                    (cairo_paint cr)))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Abstract-Surface abstract-surface?]
 [#:opaque SVG-Surface svg-surface?]
 [#:opaque PDF-Surface pdf-surface?]
 [abstract-surface-content-bbox (-> Abstract-Surface (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface->stream-bytes (-> Abstract-Surface Symbol Symbol Positive-Flonum Bytes)]
 [abstract-surface-save (-> Abstract-Surface (U Output-Port Path-String) Symbol Positive-Flonum Void)])
