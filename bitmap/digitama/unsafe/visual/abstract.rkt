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
    (start-breakable-atomic)
    (case format
      [(svg) (abstract-surface-save-with cairo_svg_surface_create_for_stream sfc /dev/sfcout 4096)]
      [(pdf) (abstract-surface-save-with cairo_pdf_surface_create_for_stream sfc /dev/sfcout 256)]
      [else  (abstract-surface-save-as-png sfc /dev/sfcout density 8192)])
    (end-breakable-atomic))

  (define (abstract-surface-save-as-png abs-sfc /dev/pngout density pool-size)
    (define-values (lx ty flwidth flheight) (abstract-surface-content-bbox abs-sfc))
    (define-values (png-sfc _width _height) (cairo-create-image-surface flwidth flheight density))
    (define png-cr (cairo_create png-sfc))

    (unless (unsafe-fl= density 1.0)
      (cairo_scale png-cr density density))

    (cairo_set_source_surface png-cr abs-sfc (unsafe-fl- 0.0 lx) (unsafe-fl- 0.0 ty))
    (cairo_paint png-cr)
    (cairo_surface_flush png-sfc)
    (cairo_surface_write_to_png_stream png-sfc (make-cairo-image-surface-writer /dev/pngout pool-size))
    
    (cairo_surface_destroy png-sfc)
    (cairo_destroy png-cr))

  (define (abstract-surface-save-with create_for_stream abs-sfc /dev/strout pool-size)
    (define-values (lx ty width height) (abstract-surface-content-bbox abs-sfc))
    (define vec-sfc (create_for_stream (make-cairo-vector-surface-writer /dev/strout pool-size) width height))
    (define vec-cr (cairo_create vec-sfc))

    (cairo_set_source_surface vec-cr abs-sfc (unsafe-fl- 0.0 lx) (unsafe-fl- 0.0 ty))
    (cairo_paint vec-cr)
    (cairo_surface_flush vec-sfc)
    (cairo_surface_destroy vec-sfc)
    (cairo_destroy vec-cr)))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque Abstract-Surface abstract-surface?]
 [#:opaque SVG-Surface svg-surface?]
 [#:opaque PDF-Surface pdf-surface?]
 [abstract-surface-content-bbox (-> Abstract-Surface (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
 [abstract-surface->stream-bytes (-> Abstract-Surface Symbol Symbol Positive-Flonum Bytes)]
 [abstract-surface-save (-> Abstract-Surface Output-Port Symbol Positive-Flonum Void)])
