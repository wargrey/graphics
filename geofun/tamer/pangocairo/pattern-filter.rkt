#lang racket

(require racket/draw/unsafe/cairo)
(require racket/draw/private/bitmap)
(require bitmap/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cario/text/pixman-downscale.c

(define quadcolor (read-bitmap (collection-file-path "quadcolor.png" "geofun" "tamer" "pangocairo")))
(define romedalen (read-bitmap (collection-file-path "romedalen.png" "geofun" "tamer" "pangocairo")))

(define (cairo-filtered-image png filter density)
  (define image (send png get-handle))
  (define w (cairo_image_surface_get_width image))
  (define h (cairo_image_surface_get_height image))
  (define-values (bmp cr) (create-argb-bitmap (* (exact->inexact w) density) (* (exact->inexact h) density) 1.0 #true))
  (define pattern (cairo_pattern_create_for_surface image))
  (define src (cairo_get_source cr))

  (cairo_scale cr density density)
  (cairo_pattern_set_filter pattern filter)
  (cairo_set_source cr pattern)
  (cairo_paint cr)
  
  bmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  quadcolor
  (list (cairo-filtered-image quadcolor CAIRO_FILTER_FAST 2.0)
        (cairo-filtered-image quadcolor CAIRO_FILTER_GOOD 2.0)
        (cairo-filtered-image quadcolor CAIRO_FILTER_BEST 2.0))

  (list (cairo-filtered-image quadcolor CAIRO_FILTER_NEAREST 2.0)
        (cairo-filtered-image quadcolor CAIRO_FILTER_BILINEAR 2.0)
        (cairo-filtered-image quadcolor CAIRO_FILTER_GAUSSIAN 2.0))

  romedalen
  (list (cairo-filtered-image romedalen CAIRO_FILTER_FAST 1.618)
        (cairo-filtered-image romedalen CAIRO_FILTER_GOOD 1.618)
        (cairo-filtered-image romedalen CAIRO_FILTER_BEST 1.618))

  (list (cairo-filtered-image romedalen CAIRO_FILTER_NEAREST 1.618)
        (cairo-filtered-image romedalen CAIRO_FILTER_BILINEAR 1.618)
        (cairo-filtered-image romedalen CAIRO_FILTER_GAUSSIAN 1.618)))
