#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/unsafe/draw.rkt")
(require "digitama/unsafe/image.rkt")
(require "digitama/unsafe/shape.rkt")
(require "digitama/unsafe/text.rkt")

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/source.rkt")
(require "font.rkt")

(require racket/string)

(define bitmap-rectangular : (-> Nonnegative-Real Nonnegative-Real XYWH->ARGB [#:density Positive-Flonum] Bitmap)
  (lambda [width height λargb #:density [density (default-bitmap-density)]]
    (λbitmap (real->double-flonum width) (real->double-flonum height)
             density λargb)))

(define bitmap-blank : (->* () (Real (Option Real) #:density Positive-Flonum) Bitmap)
  (lambda [[width 0.0] [height #false] #:density [density (default-bitmap-density)]]
    (bitmap_blank (real->double-flonum width)
                  (real->double-flonum (or height width))
                  density)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (define-values (w h) (bitmap-flsize bmp))
    (bitmap_blank w h (bitmap-density bmp))))

(define bitmap-solid : (->* () (Color Real #:density Positive-Flonum) Bitmap)
  (lambda [[color 'transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Flonum (real->double-flonum size))
    (bitmap_pattern side side (rgb* color) density)))

(define bitmap-text : (->* (String)
                           (Font #:color Stroke-Paint #:background Fill-Paint #:lines (Listof Text-Decoration-Line)
                                 #:baseline (Option Stroke-Paint) #:capline (Option Stroke-Paint) #:meanline (Option Stroke-Paint)
                                 #:ascent (Option Stroke-Paint) #:descent (Option Stroke-Paint)
                                 #:density Positive-Flonum)
                           Bitmap)
  (lambda [text [font (default-font)] #:color [fgsource black] #:background [bgsource transparent] #:lines [lines null]
                #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false]
                #:baseline [blsource #false] #:density [density (default-bitmap-density)]]
    (bitmap_text text (font-description font) lines fgsource bgsource
                 alsource clsource mlsource blsource dlsource density)))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                (Font #:color Stroke-Paint #:background Fill-Paint #:lines (Listof Text-Decoration-Line)
                                      #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                      #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode
                                      #:density Positive-Flonum)
                                Bitmap)
  (lambda [texts [font (default-font)] #:color [fgsource black] #:background [bgsource transparent] #:lines [lines null]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]
                 #:density [density (default-bitmap-density)]]
    (bitmap_paragraph (if (list? texts) (string-join texts "\n") texts) (font-description font) lines
                      (if (or (infinite? max-width) (nan? max-width)) -1 (real->double-flonum max-width))
                      (cond [(or (infinite? max-height) (nan? max-height)) -1]
                            [(negative? max-height) (exact-round max-height)]
                            [else (real->double-flonum max-height)])
                      (real->double-flonum indent) (real->double-flonum spacing) ; +nan.0 and +inf.0 are 0s
                      (paragraph-wrap-mode->integer wrap-mode) (paragraph-ellipsize-mode->integer ellipsize-mode)
                      fgsource bgsource density)))

(define bitmap-frame : (-> Bitmap [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)]
                           [#:margin Nonnegative-Real] [#:padding Nonnegative-Real] Bitmap)
  (lambda [bmp #:margin [margin 0.0] #:padding [inset 0.0] #:border [stroke (default-border)] #:fill [fill #false]]
    (bitmap_frame (bitmap-surface bmp) margin margin margin margin inset inset inset inset
                  (stroke-paint->source* stroke) (fill-paint->source* fill) (bitmap-density bmp))))

(define bitmap-square : (->* (Real)
                             ((Option Real) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum)
                             Bitmap)
  (lambda [w [radius #false] #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (if (and radius (positive? radius))
        (bitmap_rounded_rectangle width width radius (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_rectangle width width (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-rectangle : (->* (Real)
                                (Real (Option Real) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum)
                                Bitmap)
  (lambda [w [h #false] [radius #false] #:border [border (default-stroke)] #:fill [pattern #false]
             #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (define height : Flonum (if (not h) width (real->double-flonum h)))
    (if (and radius (positive? radius))
        (bitmap_rounded_rectangle width height radius (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_rectangle width height (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-stadium : (-> Real Real [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (bitmap_stadium (real->double-flonum length) radius (stroke-paint->source* border) (fill-paint->source* pattern) density)))

(define bitmap-circle : (-> Real [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)] [#:density Positive-Flonum] Bitmap)
  (lambda [radius #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (bitmap_arc (real->double-flonum radius) (stroke-paint->source* border) (fill-paint->source* pattern) density)))

(define bitmap-ellipse : (->* (Real) (Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum) Bitmap)
  (lambda [width [height #false] #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (if (or (not height) (= width height))
        (bitmap_arc (fl/ (real->double-flonum width) 2.0) (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_elliptical_arc (real->double-flonum width) (real->double-flonum height)
                               (stroke-paint->source* border) (fill-paint->source* pattern) density))))

#;(define bitmap-polygon : (->* ((Pairof (Pairof Real Real) (Listof (Pairof Real Real))))
                              (Real Real (U 'odd-even 'winding) #:color Brush+Color #:border Pen+Color #:density Positive-Flonum)
                              Bitmap)
  (let ([path : (Instance DC-Path%) (make-object dc-path%)])
    (lambda [vertices [xoffset 0.0] [yoffset 0.0] [style 'odd-even] #:color [color black] #:border [border-color 'transparent]
                      #:density [density (default-bitmap-density)]]
      (define count : Integer (length vertices))
      (send path reset)
      (send path move-to (caar vertices) (cdar vertices))
      (for ([p (in-list (cdr vertices))]) (send path line-to (car p) (cdr p)))
      (send path close)
      (define border-pen : Pen (select-pen border-color))
      (define brush : Brush (select-brush color))
      (define lw : Real (send border-pen get-width))
      (define-values (xoff yoff) (values (+ xoffset (/ lw 2)) (+ yoffset (/ lw 2))))
      (define-values (left top width height) (send path get-bounding-box))
      (define bmp : Bitmap (bitmap-blank (+ (max left width) lw) (+ (max top height) lw) density))
      (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
      (send dc set-smoothing 'aligned)
      (send dc set-pen border-pen)
      (send dc set-brush brush)
      (when (< count 3)
        (send dc set-pen (make-css-pen border-pen #:color (send brush get-color)))
        (when (= count 1) (send dc draw-point (+ xoff (caar vertices)) (+ yoff (cdar vertices)))))
      (send dc draw-path path xoff yoff style)
      bmp)))
