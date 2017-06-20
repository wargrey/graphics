#lang typed/racket

(provide (all-defined-out))

(require "digitama/unsafe/source.rkt")
(require "digitama/unsafe/bitmap.rkt")
(require "digitama/unsafe/shape.rkt")
(require "digitama/unsafe/text.rkt")

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "paint.rkt")
(require "color.rkt")
(require "font.rkt")

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
    (bitmap_blank (exact->inexact (send bmp get-width))
                  (exact->inexact (send bmp get-height))
                  (real->double-flonum (send bmp get-backing-scale)))))

(define bitmap-solid : (->* () (Color Real #:density Positive-Flonum) Bitmap)
  (lambda [[color 'transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Flonum (real->double-flonum size))
    (bitmap_pattern side side (rgb* color) density)))

(define bitmap-text : (->* (String)
                           (Font #:color Bitmap-Source #:background-color Bitmap-Source #:lines (Listof Text-Decoration-Line)
                                 #:baseline (Option Bitmap-Source) #:capline (Option Bitmap-Source) #:meanline (Option Bitmap-Source)
                                 #:ascent (Option Bitmap-Source) #:descent (Option Bitmap-Source)
                                 #:density Positive-Flonum)
                           Bitmap)
  (lambda [content [font (default-font)] #:color [fgcolor black] #:background-color [bgcolor transparent] #:lines [lines null]
                   #:ascent [alcolor #false] #:descent [dlcolor #false] #:capline [clcolor #false] #:meanline [mlcolor #false]
                   #:baseline [blcolor #false] #:density [density (default-bitmap-density)]]
    (bitmap_text content (font-description font) lines fgcolor bgcolor
                 alcolor clcolor mlcolor blcolor dlcolor density)))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                (Font #:color Bitmap-Source #:background-color Bitmap-Source
                                      #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                      #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode
                                      #:lines (Listof Text-Decoration-Line) #:density Positive-Flonum)
                                Bitmap)
  (lambda [words [font (default-font)] #:color [fgsource black] #:background-color [bgsource transparent]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]
                 #:lines [lines null] #:density [density (default-bitmap-density)]]
    (define smart-width : (U Flonum -1) (if (or (infinite? max-width) (nan? max-width)) -1 (real->double-flonum max-width)))
    (define-values (smart-height smart-emode)
      (cond [(eq? ellipsize-mode 'none) (values -1 ellipsize-mode)]
            [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (bitmap_paragraph (if (list? words) (string-join words "\n") words) smart-width smart-height
                      (real->double-flonum indent) (real->double-flonum spacing) ; +nan.0 and +inf.0 are 0s
                      (paragraph-wrap-mode->integer wrap-mode) (paragraph-ellipsize-mode->integer smart-emode)
                      (font-description font) lines fgsource bgsource density)))

#;(define bitmap-frame : (-> Bitmap [#:border (U Pen+Color (Listof Pen+Color))] [#:background Brush+Color]
                           [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:density Positive-Flonum] Bitmap)
  (let ()
    (define (normalize [v : (U Nonnegative-Real (Listof Nonnegative-Real))])
      : (Values Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real)
      (if (list? v)
          (match v
            [(list t) (values t t t t)]
            [(list t r) (values t r t r)]
            [(list t r b) (values t r b r)]
            [(list t r b l rest ...) (values t r b l)]
            [else (values 0 0 0 0)])
          (values v v v v)))
    (define (draw-border [dc : (Instance Bitmap-DC%)] [pen : (Instance Pen%)] [w : Real] [x1 : Real] [y1 : Real] [x2 : Real] [y2 : Real])
      (when (> w 0) (send* dc (set-pen pen) (draw-line x1 y1 x2 y2))))
    (lambda [bmp #:margin [margin 0] #:padding [inset 0] #:density [density (default-bitmap-density)]
                 #:border [pen-hint "Silver"] #:background [brush-hint (default-css-brush)]]
      (define-values (margin-top margin-right margin-bottom margin-left) (normalize margin))
      (define-values (padding-top padding-right padding-bottom padding-left) (normalize inset))
      (define-values (pen-top pen-right pen-bottom pen-left)
        (if (list? pen-hint)
            (match pen-hint
              [(list vt) (let ([t (select-pen vt)]) (values t t t t))]
              [(list vt vr) (let ([t (select-pen vt)] [r (select-pen vr)]) (values t r t r))]
              [(list vt vr b) (let ([t (select-pen vt)] [r (select-pen vr)]) (values t r (select-pen b) r))]
              [(list t r b l) (values (select-pen t) (select-pen r) (select-pen b) (select-pen l))]
              [else (let ([deadcode (default-css-pen)]) (values deadcode deadcode deadcode deadcode))])
            (let ([p (select-pen pen-hint)]) (values p p p p))))
      (define-values (border-top border-right border-bottom border-left)
        (values (send pen-top get-width) (send pen-right get-width)
                (send pen-bottom get-width) (send pen-left get-width)))
      (define-values (bgx bgy bgw bgh)
        (values (+ margin-left border-left) (+ margin-top border-top)
                (+ (send bmp get-width) padding-left padding-right)
                (+ (send bmp get-height) padding-top padding-bottom)))
      (define-values (bdx0 bdy0 bdx1 bdy1)
        (values margin-left margin-top
                (+ margin-left border-left bgw border-right)
                (+ margin-top border-top bgh border-bottom)))
      (define frame : Bitmap (bitmap-blank (+ bdx1 margin-right) (+ bdy1 margin-bottom) (send bmp get-backing-scale)))
      (define dc : (Instance Bitmap-DC%) (send frame make-dc))
      (send dc set-smoothing 'aligned)
      (send dc set-pen (select-pen 'transparent))
      (send dc set-brush (select-brush brush-hint))
      (send dc draw-rectangle bgx bgy bgw bgh)
      ;;; TODO: deal with corner overlaps correctly
      (let ([bdy (+ bdy0 (* border-top 1/2))]) (draw-border dc pen-top border-top bdx0 bdy bdx1 bdy))
      (let ([bdy (- bdy1 (* border-bottom 1/2))]) (draw-border dc pen-bottom border-bottom bdx0 bdy bdx1 bdy))
      (let ([bdx (+ bdx0 (* border-left 1/2))]) (draw-border dc pen-left border-left bdx bdy0 bdx bdy1))
      (let ([bdx (- bdx1 (* border-right 1/2))]) (draw-border dc pen-right border-right bdx bdy0 bdx bdy1))
      (send dc draw-bitmap bmp (+ bgx padding-left) (+ bgy padding-top))
      frame)))

(define bitmap-square : (->* (Real)
                             ((Option Real) #:border (Option Stroke) #:fill (Option Bitmap-Source) #:density Positive-Flonum)
                             Bitmap)
  (lambda [w [radius #false] #:border [border (default-stroke)] #:fill [pattern black] #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (if (and radius (positive? radius))
        (bitmap_rounded_rectangle width width radius border pattern density)
        (bitmap_rectangle width width border pattern density))))

(define bitmap-rectangle : (->* (Real)
                                (Real (Option Real) #:border (Option Stroke) #:fill (Option Bitmap-Source) #:density Positive-Flonum)
                                Bitmap)
  (lambda [w [h #false] [radius #false] #:border [border (default-stroke)] #:fill [pattern black]
             #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (define height : Flonum (if (not h) width (real->double-flonum h)))
    (if (and radius (positive? radius))
        (bitmap_rounded_rectangle width height radius border pattern density)
        (bitmap_rectangle width height border pattern density))))

(define bitmap-stadium : (-> Real Real [#:border (Option Stroke)] [#:fill (Option Bitmap-Source)] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:border [border (default-stroke)] #:fill [pattern black] #:density [density (default-bitmap-density)]]
    (bitmap_stadium (real->double-flonum length) radius border pattern density)))

(define bitmap-circle : (-> Real [#:border (Option Stroke)] [#:fill (Option Bitmap-Source)] [#:density Positive-Flonum] Bitmap)
  (lambda [radius #:border [border (default-stroke)] #:fill [pattern black] #:density [density (default-bitmap-density)]]
    (bitmap_arc (real->double-flonum radius) border pattern density)))

(define bitmap-ellipse : (->* (Real) (Real #:border (Option Stroke) #:fill (Option Bitmap-Source) #:density Positive-Flonum) Bitmap)
  (lambda [width [height #false] #:border [border (default-stroke)] #:fill [pattern black] #:density [density (default-bitmap-density)]]
    (if (or (not height) (= width height))
        (bitmap_arc (fl/ (real->double-flonum width) 2.0) border pattern density)
        (bitmap_elliptical_arc (real->double-flonum width) (real->double-flonum height)
                               border pattern density))))

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
