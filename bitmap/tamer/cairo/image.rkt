#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")

(require ffi/unsafe/define)

(require racket/draw)
(require racket/draw/unsafe/cairo-lib)
(require racket/draw/private/utils)

(define-ffi-definer define-cairo cairo-lib #:provide provide-protected)
(define-syntax-rule (_cfun spec ...) (_fun #:lock-name "cairo-pango-lock" spec ...))

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
(define romedalen.png (read-bitmap "romedalen.png"))
(define image (send romedalen.png get-handle))
(define w (cairo_image_surface_get_width image))
(define h (cairo_image_surface_get_height image))
(define &matrix (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0))

(define (cairo-clip-image xc yc radius)
  (define-values (x y) (values (max xc radius) (max yc radius)))
  (define-values (width height) (values (* x 2.0) (* y 2.0)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density))
  
  (cairo_arc cr x y radius 0.0 (degrees->radians 360.0))
  (cairo_clip cr)
  (cairo_new_path cr)

  (cairo_scale cr (/ width w) (/ height h))
  (cairo_set_source_surface cr image 0.0 0.0)
  (cairo_paint cr)

  bmp)

(define (cairo-image xc yc)
  (define-values (width height) (values (* xc 2.0) (* yc 2.0)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density))
  
  (cairo_translate cr xc yc)
  (cairo_rotate cr (degrees->radians 45.0))
  (cairo_scale cr (/ width w) (/ height h))
  (cairo_translate cr (* -0.5 w) (* -0.5 h))
  (cairo_set_source_surface cr image 0.0 0.0)
  (cairo_paint cr)

  bmp)

(define (cairo-image-pattern xc yc)
  (define-values (width height) (values (* xc 2.0) (* yc 2.0)))
  (define-values (bmp cr _w _h) (make-cairo-image width height density))
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

(cairo-clip-image 128.0 128.0 76.0)
(cairo-image 128.0 128.0)
(cairo-image-pattern 128.0 128.0)
