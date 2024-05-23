#lang typed/racket/base

(provide (all-defined-out))
(provide Bitmap XYWH->ARGB XYWH->ARGB* ARGB-Step)
(provide (rename-out [bitmap-polyline bitmap-lines]
                     [bitmap-sandglass bitmap-hourglass]))

(require "font.rkt")

(require "digitama/dot.rkt")
(require "digitama/base.rkt")
(require "digitama/font.rkt")
(require "digitama/source.rkt")

(require "digitama/unsafe/convert.rkt")
(require "digitama/unsafe/image.rkt")
(require "digitama/unsafe/shape.rkt")
(require "digitama/unsafe/text.rkt")

(require racket/math)
(require racket/string)
(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-rectangular : (-> Nonnegative-Real Nonnegative-Real XYWH->ARGB [#:density Positive-Flonum] Bitmap)
  (lambda [width height λargb #:density [density (default-bitmap-density)]]
    (λbitmap (real->double-flonum width) (real->double-flonum height)
             density λargb)))

(define bitmap-rectangular* : (All (t) (-> Nonnegative-Real Nonnegative-Real (XYWH->ARGB* t) t [#:density Positive-Flonum]
                                     (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (λbitmap* (real->double-flonum width) (real->double-flonum height)
              density λargb initial)))

(define bitmap-irregular : (All (t) (-> Nonnegative-Real Nonnegative-Real (ARGB-Step t) t [#:density Positive-Flonum] Bitmap))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (bmp _) (bitmap-irregular* width height λargb initial #:density density))
    bmp))

(define bitmap-irregular* : (All (t) (-> Nonnegative-Real Nonnegative-Real (ARGB-Step t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (λbitmap_step (real->double-flonum width) (real->double-flonum height) density λargb initial)))

(define bitmap-blank : (->* () (Real (Option Real) #:density Positive-Flonum) Bitmap)
  (lambda [[width 0.0] [height #false] #:density [density (default-bitmap-density)]]
    (bitmap_blank (real->double-flonum width)
                  (real->double-flonum (or height width))
                  density)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (define-values (flw flh density) (bitmap-flsize+density bmp))
    (bitmap_blank (/ flw density) (/ flh density)
                  density)))

(define bitmap-solid : (->* () (Color Real #:density Positive-Flonum) Bitmap)
  (lambda [[color 'transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Flonum (real->double-flonum size))
    (bitmap_pattern side side (rgb* color) density)))

(define bitmap-text : (->* (Any)
                           (Font #:color Fill-Paint #:background (Option Fill-Paint) #:lines (Listof Symbol)
                                 #:baseline (Option Color) #:capline (Option Color) #:meanline (Option Color)
                                 #:ascent (Option Color) #:descent (Option Color)
                                 #:density Positive-Flonum)
                           Bitmap)
  (lambda [text [font (default-font)] #:color [fgsource black] #:background [bgsource #false] #:lines [lines null]
                #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false]
                #:baseline [blsource #false] #:density [density (default-bitmap-density)]]
    (bitmap_text (~a text) (font-description font) lines (fill-paint->source fgsource) (fill-paint->source* bgsource)
                 (and alsource (rgb* alsource)) (and clsource (rgb* clsource)) (and mlsource (rgb* mlsource))
                 (and blsource (rgb* blsource)) (and dlsource (rgb* dlsource)) density)))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                (Font #:color Fill-Paint #:background (Option Fill-Paint) #:lines (Listof Symbol)
                                      #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                      #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode
                                      #:density Positive-Flonum)
                                Bitmap)
  (lambda [texts [font (default-font)] #:color [fgsource black] #:background [bgsource #false] #:lines [lines null]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]
                 #:density [density (default-bitmap-density)]]
    (define-values (smart-height smart-emode)
      (cond [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (bitmap_paragraph (if (list? texts) (string-join texts "\n") texts) (font-description font) lines
                      (if (or (infinite? max-width) (nan? max-width)) -1 (real->double-flonum max-width)) smart-height
                      (real->double-flonum indent) (real->double-flonum spacing)
                      (paragraph-wrap-mode->integer wrap-mode raise-argument-error)
                      (paragraph-ellipsize-mode->integer smart-emode raise-argument-error)
                      (fill-paint->source fgsource) (fill-paint->source* bgsource) density)))

(define bitmap-frame : (-> Bitmap [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)]
                           [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                           Bitmap)
  (lambda [bmp #:margin [margin 0.0] #:padding [inset 0.0] #:border [stroke (default-border)] #:fill [fill #false]]
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [else (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]))
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [else (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]))
    (bitmap_frame (bitmap-surface bmp) mtop mright mbottom mleft ptop pright pbottom pleft
                  (stroke-paint->source* stroke) (fill-paint->source* fill) (bitmap-density bmp))))

(define bitmap-square : (->* (Real)
                             ((Option Real) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum)
                             Bitmap)
  (lambda [w [corner-radius #false] #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (if (or (not corner-radius) (zero? corner-radius))
        (bitmap_rectangle width width (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_rounded_rectangle width width (real->double-flonum corner-radius) (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-rectangle : (->* (Real)
                                (Real (Option Real) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum)
                                Bitmap)
  (lambda [w [h #false] [corner-radius #false] #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (define width : Flonum (real->double-flonum w))
    (define height : Flonum (if (not h) width (real->double-flonum h)))
    (if (or (not corner-radius) (zero? corner-radius))
        (bitmap_rectangle width height (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_rounded_rectangle width height (real->double-flonum corner-radius) (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-polyline : (->* ((U Point2D (Listof Point2D)))
                               (Real Real #:scale Point2D #:window Point2D #:stroke (Option Stroke-Paint) #:density Positive-Flonum)
                               Bitmap)
  (lambda [pts [dx 0.0] [dy 0.0] #:scale [scale 1.0] #:stroke [stroke (default-stroke)] #:density [density (default-bitmap-density)] #:window [window 0]]
    (define-values (xs ys lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))

    (bitmap_polyline width height xs ys xoff yoff
                     (stroke-paint->source* stroke) #false #false
                     density)))

(define bitmap-polygon : (->* ((U Point2D (Listof Point2D)))
                              (Real Real #:scale Point2D #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:fill-style Symbol
                                    #:density Positive-Flonum #:window Point2D)
                              Bitmap)
  (lambda [pts [dx 0.0] [dy 0.0] #:scale [scale 1.0] #:border [border (default-stroke)] #:fill [pattern #false] #:fill-style [fstyle 'winding]
               #:density [density (default-bitmap-density)] #:window [window 0]]
    (define-values (xs ys lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))

    (bitmap_polyline width height xs ys xoff yoff
                     (stroke-paint->source* border) (fill-paint->source* pattern) fstyle
                     density)))

(define bitmap-regular-polygon : (->* (Integer Real)
                                      (Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum
                                            #:radian? Boolean #:inscribed? Boolean)
                                      Bitmap)
  (lambda [n radius [rotation 0.0]
             #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]
             #:radian? [radian? #true] #:inscribed? [inscribed? #false]]
    (if (and (index? n) (> n 0))
        (bitmap_regular_polygon n (real->double-flonum radius) (real->double-flonum rotation)
                                (stroke-paint->source* border) (fill-paint->source* pattern)
                                radian? inscribed? density)
        (bitmap_circle (real->double-flonum radius) (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-stadium : (-> Real Real [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (bitmap_stadium (real->double-flonum length) (real->double-flonum radius)
                    (stroke-paint->source* border) (fill-paint->source* pattern) density)))

(define bitmap-circle : (-> Real [#:border (Option Stroke-Paint)] [#:fill (Option Fill-Paint)] [#:density Positive-Flonum] Bitmap)
  (lambda [radius #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (bitmap_circle (real->double-flonum radius) (stroke-paint->source* border) (fill-paint->source* pattern) density)))

(define bitmap-sector : (->* (Real Real Real)
                             (#:ratio Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:radian? Boolean #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:ratio [ratio 1.0] #:border [border (default-stroke)] #:fill [pattern #false] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define r (real->double-flonum radius))
    (bitmap_sector r (if (> ratio 0.0) (/ r ratio) r) (real->double-flonum start) (real->double-flonum end)
                   (stroke-paint->source* border) (fill-paint->source* pattern) density radian?)))

(define bitmap-arc : (->* (Real Real Real) (#:ratio Real #:stroke (Option Stroke-Paint) #:radian? Boolean #:density Positive-Flonum) Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [stroke (default-stroke)] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define r (real->double-flonum radius))
    (bitmap_arc r (if (> ratio 0.0) (/ r ratio) r) (real->double-flonum start) (real->double-flonum end)
                (stroke-paint->source* stroke) density radian?)))

(define bitmap-ellipse : (->* (Real) (Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum) Bitmap)
  (lambda [width [height #false] #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]]
    (if (or (not height) (= width height))
        (bitmap_circle (* (real->double-flonum width) 0.5) (stroke-paint->source* border) (fill-paint->source* pattern) density)
        (bitmap_ellipse (real->double-flonum width) (real->double-flonum height)
                        (stroke-paint->source* border) (fill-paint->source* pattern) density))))

(define bitmap-arrow : (->* (Real Real)
                            (Real #:shaft-thickness Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint)
                                  #:radian? Boolean #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:shaft-thickness [shaft-thickness -0.3] #:border [border #false] #:fill [pattern 'black]
           #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           head-radius shaft-length [start 0.0]]
    (bitmap_arrow (real->double-flonum head-radius) (real->double-flonum start)
                  (stroke-paint->source* border) (fill-paint->source* pattern) density radian?
                  (real->double-flonum shaft-thickness) (real->double-flonum shaft-length))))

(define bitmap-arrowhead : (->* (Real)
                                (Real #:shaft-thickness Real #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:radian? Boolean #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:shaft-thickness [shaft-thickness 0.0] #:border [border (default-stroke)] #:fill [pattern #false]
           #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius [start 0.0]]
    (bitmap_arrow (real->double-flonum radius) (real->double-flonum start)
                  (stroke-paint->source* border) (fill-paint->source* pattern) density radian?
                  (real->double-flonum shaft-thickness) -1.0)))

(define bitmap-hline : (->* (Real Real) (#:stroke (Option Stroke-Paint) #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke)] #:density [density (default-bitmap-density)]]
    (define flheight (real->double-flonum height))
    (bitmap_line 0.0 (* flheight 0.5) (real->double-flonum width) 0.0
                 (real->double-flonum width) flheight (stroke-paint->source* stroke)
                 density)))

(define bitmap-vline : (->* (Real Real) (#:stroke (Option Stroke-Paint) #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke)] #:density [density (default-bitmap-density)]]
    (define flwidth (real->double-flonum width))
    (bitmap_line (* flwidth 0.5) 0.0 0.0 (real->double-flonum height)
                 flwidth (real->double-flonum height) (stroke-paint->source* stroke)
                 density)))

(define bitmap-sandglass : (->* (Real)
                                (Real #:neck-width Real #:neck-height Real #:tube-height Real
                                      #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height -0.0618]
           #:border [border (default-stroke)] #:fill [pattern #false] #:density [density (default-bitmap-density)]
           width [height #false]]
    (bitmap_sandglass (real->double-flonum width) (real->double-flonum (if (not height) (* width 1.618) height))
                      (real->double-flonum neck-width) (real->double-flonum neck-height) (real->double-flonum tube-height)
                      (stroke-paint->source* border) (fill-paint->source* pattern) density)))
