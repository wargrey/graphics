#lang typed/racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "ffi.rkt")

  (require racket/class)
  (require racket/draw)

  (require racket/flonum)
  (require racket/unsafe/ops)

  (define transparent-pen (make-pen #:style 'transparent))
  
  (define (cairo-create-argb-image flwidth flheight [density 1.0])
    (define surface
      (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                  (unsafe-fxmax (unsafe-fl->fx (unsafe-fl* flwidth density)) 1)
                                  (unsafe-fxmax (unsafe-fl->fx (unsafe-fl* flheight density)) 1)))
    
    (define cr (cairo_create surface))
    (unless (unsafe-fl= density 1.0) (cairo_scale cr density density))
    
    (cairo_surface_destroy surface) ; cairo will maintain the surface
    cr)

  (define (make-cairo-image flwidth flheight [density 1.0])
    (define width (unsafe-fxmax (unsafe-fl->fx flwidth) 1))
    (define height (unsafe-fxmax (unsafe-fl->fx flheight) 1))
    (define bmp (make-object bitmap% width height #false #true density))
    (define cr (cairo_create (send bmp get-handle)))
    (unless (unsafe-fl= density 1.0) (cairo_scale cr density density))
    (values bmp cr))

  (define (make-cairo-image* flwidth flheight background [density 1.0])
    (define-values (bmp cr) (make-cairo-image flwidth flheight density))
    (when (flvector? background)
      (cairo-set-rgba cr background)
      (cairo_paint cr))
    (values bmp cr))

  (define (cairo-set-rgba cr color)
    (cairo_set_source_rgba cr
                           (unsafe-flvector-ref color 0)
                           (unsafe-flvector-ref color 1)
                           (unsafe-flvector-ref color 2)
                           (unsafe-flvector-ref color 3)))
  
  (define (cairo-image->bitmap cr flwidth flheight [density 1.0])
    (define-values (width height) (values (unsafe-fxmax (unsafe-fl->fx flwidth) 1) (unsafe-fxmax (unsafe-fl->fx flheight) 1)))
    (define bmp (make-object bitmap% width height #false #true density))
    (define-values (src-surface dest-surface) (values (cairo_get_target cr) (send bmp get-handle)))
    (define-values (src dest) (values (cairo_image_surface_get_data src-surface) (cairo_image_surface_get_data dest-surface)))
    (define-values (src-step dest-step) (values (cairo_image_surface_get_stride src-surface) (cairo_image_surface_get_stride dest-surface)))
    (define-values (total safe-step) (values (unsafe-bytes-length dest) (unsafe-fxmin src-step dest-step)))
    (cond [(eq? src-step dest-step) (memcpy dest src total _byte)]
          [else (let rowcpy ([srcoff 0] [destoff 0])
                  (define destnext (unsafe-fx+ destoff dest-step))
                  (memcpy dest destoff src srcoff safe-step _byte)
                  (when (< destnext total) (rowcpy (unsafe-fx+ srcoff src-step) destnext)))])
    (cairo_surface_mark_dirty dest-surface)
    bmp))
