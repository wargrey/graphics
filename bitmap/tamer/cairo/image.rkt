#lang racket

(require racket/draw/private/bitmap)
(require "../../digitama/unsafe/pangocairo.rkt")

(define-cstruct _cairo_matrix_t
  ([xx _double*]
   [yx _double*]
   [xy _double*]
   [yy _double*]
   [x0 _double*]
   [y0 _double*])
  #:malloc-mode 'atomic-interior)

(define-cairo cairo_matrix_init_scale (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))

;;; https://www.cairographics.org/samples/clip_image/

(define density 2.0)
(define image (send (read-bitmap (collection-file-path "romedalen.png" "bitmap" "tamer" "cairo")) get-handle))
(define w (cairo_image_surface_get_width image))
(define h (cairo_image_surface_get_height image))
(define &matrix (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0))

(define (cairo-image xc yc)
  (define-values (width height) (values (* xc 2.0) (* yc 2.0)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density #true))
  
  (cairo_translate cr xc yc)
  (cairo_rotate cr (degrees->radians 45.0))
  (cairo_scale cr (/ width w) (/ height h))
  (cairo_translate cr (* -0.5 w) (* -0.5 h))
  (cairo_set_source_surface cr image 0.0 0.0)
  (cairo_paint cr)

  bmp)

(define (cairo-image-pattern xc yc)
  (define-values (width height) (values (* xc 2.0) (* yc 2.0)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density #true))
  (define pattern (cairo_pattern_create_for_surface image))

  (cairo_pattern_set_extend pattern CAIRO_EXTEND_REPEAT)
  (cairo_translate cr xc yc)
  (cairo_rotate cr (degrees->radians 45.0))
  (cairo_scale cr (/ 1.0 (sqrt 2.0)) (/ 1.0 (sqrt 2.0)))
  (cairo_translate cr (- xc) (- yc))

  (cairo_matrix_init_scale &matrix (* (/ w width) 5.0) (* (/ h height) 5.0))
  (cairo_pattern_set_matrix pattern &matrix)
  (cairo_set_source cr pattern)
  (cairo_rectangle cr 0 0 w h)
  (cairo_fill cr)

  (cairo_pattern_destroy pattern)
  bmp)

(cairo-image 128.0 128.0)
(cairo-image-pattern 128.0 128.0)
