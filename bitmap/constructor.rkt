#lang typed/racket

(provide (all-defined-out))
(provide (rename-out [bitmap-ellipse bitmap-circle]
                     [bitmap-ellipse bitmap-disk]))

(require "digitama/digicore.rkt")
(require "background.rkt")
(require "color.rkt")
(require "font.rkt")

(require "digitama/unsafe/image.rkt")

(define bitmap-dc : (->* ((-> (Instance Bitmap-DC%) Nonnegative-Flonum Nonnegative-Flonum Any))
                         (Nonnegative-Real (Option Nonnegative-Real) Positive-Real)
                         Bitmap)
  (lambda [draw-with [w 0] [h #false] [density (default-bitmap-density)]]
    (define bmp : Bitmap (bitmap-blank w h density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    ;;; let the device deal with non-integer size
    (draw-with dc (real->double-flonum w) (real->double-flonum (or h w)))
    bmp))

(define bitmap-blank : (->* () (Real (Option Real) Positive-Real) Bitmap)
  (lambda [[width 0.0] [height #false] [density (default-bitmap-density)]]
    (bitmap_blank (real->double-flonum width)
                  (real->double-flonum (or height width))
                  (real->double-flonum density))))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap_blank (exact->inexact (send bmp get-width))
                  (exact->inexact (send bmp get-height))
                  (real->double-flonum (send bmp get-backing-scale)))))

(define bitmap-solid : (->* () (Color+sRGB Real Positive-Real) Bitmap)
  (lambda [[color 'transparent] [size 1] [density (default-bitmap-density)]]
    (define solid : Bitmap (bitmap-blank size size density))
    (define dc : (Instance Bitmap-DC%) (send solid make-dc))
    (send dc set-background (select-color color))
    (send dc clear)
    solid))

(define bitmap-text : (->* (String) ((Instance Font%) #:color (Option Color+sRGB)
                                                      #:background-color (Option Color+sRGB) #:baseline (Option Pen+Color)
                                                      #:capline (Option Pen+Color) #:meanline (Option Pen+Color)
                                                      #:ascent (Option Pen+Color) #:descent (Option Pen+Color)
                                                      #:density Positive-Real) Bitmap)
  (lambda [content [font (default-css-font)] #:color [fgcolor #false] #:background-color [bgcolor #false]
                   #:ascent [alcolor #false] #:descent [dlcolor #false] #:capline [clcolor #false] #:meanline [mlcolor #false]
                   #:baseline [blcolor #false] #:density [density (default-bitmap-density)]]
    (define-values (width height _ zero) (send the-dc get-text-extent content font #true))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height density)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-color fgcolor)))
    (when bgcolor (send* dc (set-text-background (select-color bgcolor)) (set-text-mode 'solid)))
    (send dc draw-text content 0 0 #true)
    (when (or alcolor clcolor mlcolor blcolor dlcolor)
      (define-values (ascent capline meanline baseline descent) (get-font-metrics-lines font content))
      (unless (false? alcolor)
        (send dc set-pen (select-pen alcolor))
        (send dc draw-line 0 ascent width ascent))
      (unless (false? clcolor)
        (send dc set-pen (select-pen clcolor))
        (send dc draw-line 0 capline width capline))
      (unless (false? mlcolor)
        (send dc set-pen (select-pen mlcolor))
        (send dc draw-line 0 meanline width meanline))
      (unless (false? blcolor)
        (send dc set-pen (select-pen blcolor))
        (send dc draw-line 0 baseline width baseline))
      (unless (false? dlcolor)
        (send dc set-pen (select-pen dlcolor))
        (send dc draw-line 0 descent width descent)))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                ((Instance Font%) #:color Color+sRGB #:background Brush+Color
                                                  #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                                  ;#:wrap-mode Symbol #:ellipsize-mode Symbol
                                                  #:density Positive-Real)
                                Bitmap)
  (lambda [words [font (default-css-font)] #:color [fgcolor #x000000] #:background [bgcolor 'transparent]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 ;#:wrap-mode [wrap 'PANGO_WRAP_WORD_CHAR] #:ellipsize-mode [ellipsize 'PANGO_ELLIPSIZE_END]
                 #:density [density (default-bitmap-density)]]
    (bitmap_paragraph (if (list? words) (string-join words "\n") words)
                      (real->double-flonum max-width) (real->double-flonum max-height)
                      (real->double-flonum indent) (real->double-flonum spacing) 2 3
                      (font->font-description font)
                      (font->decoration-lines font)
                      (color->source (select-color fgcolor))
                      (brush->source (select-brush bgcolor))
                      (real->double-flonum density))))

(define bitmap-frame : (-> Bitmap [#:border (U Pen+Color (Listof Pen+Color))] [#:background Brush+Color]
                           [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:density Positive-Real] Bitmap)
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

(define bitmap-rectangle : (->* (Real) (Real (Option Real) #:color Brush+Color #:border Pen+Color #:density Positive-Real) Bitmap)
  (lambda [w [h #false] [radius #false] #:color [color #x000000] #:border [border-color 'transparent]
             #:density [density (default-bitmap-density)]]
    (define width : Nonnegative-Real (max w 0.0))
    (define height : Nonnegative-Real (max (or h w) 0.0))
    (define bmp : Bitmap (bitmap-blank width height density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-pen border-color))
    (send dc set-brush (select-brush color))
    (cond [(false? radius) (send dc draw-rectangle 0 0 width height)]
          [else (send dc draw-rounded-rectangle 0 0 width height radius)])
    bmp))

(define bitmap-ellipse : (->* (Real) (Real #:color Brush+Color #:border Pen+Color #:density Positive-Real) Bitmap)
  (lambda [w [h #false] #:color [color #x000000] #:border [border-color 'transparent]
             #:density [density (default-bitmap-density)]]
    (define width : Nonnegative-Real (max w 0.0))
    (define height : Nonnegative-Real (max (or h w) 0.0))
    (define bmp : Bitmap (bitmap-blank width height density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-pen border-color))
    (send dc set-brush (select-brush color))
    (send dc draw-ellipse 0 0 width height)
    bmp))

(define bitmap-polygon : (->* ((Pairof (Pairof Real Real) (Listof (Pairof Real Real))))
                              (Real Real (U 'odd-even 'winding) #:color Brush+Color #:border Pen+Color #:density Positive-Real)
                              Bitmap)
  (let ([path : (Instance DC-Path%) (make-object dc-path%)])
    (lambda [vertices [xoffset 0.0] [yoffset 0.0] [style 'odd-even] #:color [color #x000000] #:border [border-color 'transparent]
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
