#lang racket/gui

(require racket/math)
(require racket/flonum)

(require "../digitama/unsafe/ffi.rkt")
(require (submod "../digitama/unsafe/font.rkt" unsafe))
(require (submod "../digitama/unsafe/image.rkt" unsafe))

(define density 2.0)
(define radius 150)
(define words (string-split "Using Pango with Cairo to Draw Colorful Text Circle"))

(define (cairo-circle)
  (define-values (bmp cr) (make-cairo-image (* 2.0 radius) (* 2.0 radius) density))

  (define (draw-text-circle cr font-face font-size font-weight font-style)
    ; Center coordinates on the middle of the region we are drawing
    (cairo_translate cr radius radius)

    ; Create a PangoLayout, set the font and text
    (define layout (pango_cairo_create_layout cr))
    (define desc (pango-create-font-desc font-face font-size font-weight font-style))
    (pango_layout_set_font_description layout desc)
    (pango_font_description_free desc)

    ; Draw the layout N_WORDS times in a circle
    (define n (length words))
    (for ([i (in-range n)])
      (define angle (/ (* 360.0 i) n))
      (cairo_save cr)

      ; Gradient from red at angle == 60 to blue at angle == 240
      (define red (/ (+ 1 (cos (degrees->radians (- angle 60)))) 2))
      (pango_layout_set_text layout (list-ref words i))
      (cairo_set_source_rgb cr red 0 (- 1.0 red))
      (cairo_rotate cr (degrees->radians angle))

      ; Inform Pango to re-layout the text with the new transformation
      (pango_cairo_update_layout cr layout)

      (define-values (width height) (pango_layout_get_size layout))
      (cairo_move_to cr (/ (~metric width) -2) (- radius))
      (pango_cairo_show_layout cr layout)

      (cairo_restore cr)))
  
  (draw-text-circle cr "Symbol" 24 'bold 'normal)
  (cairo_destroy cr)
  bmp)

(define (cairo-paragraph)
  (pango_paragraph (string-append "Pango Layout Test:\n"
                                  "Here is some text that should wrap suitably to demonstrate PangoLayout's features.\n"
                                  "This paragraph should be ellipsized")
                   256.0 128.0 32.0 2.0 'PANGO_WRAP_WORD_CHAR 'PANGO_ELLIPSIZE_END
                   (make-color (random 255) (random 255) (random 255) 1.00)
                   (make-brush #:color (make-color (random 255) (random 255) (random 255) 0.08))
                   (pango-create-font-desc "Symbol" 16.0 'normal 'normal) 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (benchmark make-image)
  (printf "~a " (object-name make-image))
  (collect-garbage)
  (time (make-image)))

(benchmark cairo-circle)
(benchmark cairo-paragraph)
