#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitmap.rkt")
(require "digitama/color.rkt")

(require racket/math)
(require racket/string)
(require racket/bool)

(require typed/images/icons)

(define bitmap-blank : (->* () (Nonnegative-Real (Option Nonnegative-Real) Positive-Real) Bitmap)
  (lambda [[w 0] [h #false] [density (default-icon-backing-scale)]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale density)))

(define bitmap-dc : (->* ((-> (Instance Bitmap-DC%) Any)) (Nonnegative-Real (Option Nonnegative-Real) Positive-Real) Bitmap)
  (lambda [draw-with [w 0] [h #false] [density (default-icon-backing-scale)]]
    (define bmp : Bitmap (bitmap-blank w h density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (draw-with dc)
    bmp))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-blank (send bmp get-width)
                  (send bmp get-height)
                  (send bmp get-backing-scale))))

(define bitmap-solid : (->* () (Color+sRGB Nonnegative-Real Positive-Real) Bitmap)
  (lambda [[color 'transparent] [size 1] [density (default-icon-backing-scale)]]
    (define solid : Bitmap (bitmap-blank size size density))
    (define dc : (Instance Bitmap-DC%) (send solid make-dc))
    (send dc set-background (select-color color))
    (send dc clear)
    solid))

(define bitmap-text : (->* (String) (Font #:combine? Boolean #:color (Option Color+sRGB)
                                          #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [content [font (default-css-font)] #:combine? [combine? #true] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define-values (width height descent ascent) (send the-dc get-text-extent content font combine?))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-color fgcolor)))
    (when bgcolor (send dc set-text-background (select-color bgcolor)))
    (when bgcolor (send dc set-text-mode 'solid))
    (send dc draw-text content 0 0 combine?)
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-desc : (->* (String Nonnegative-Real)
                           (Font #:max-height Nonnegative-Real #:combine? Boolean
                                 #:color (Option Color+sRGB) #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [description max-width.0 [font (default-css-font)] #:max-height [max-height.0 +inf.0]
           #:combine? [combine? #true] #:color [fgcolor #false] #:background-color [bgcolor #false]]
    (define-values (max-width max-height) (values (real->double-flonum max-width.0) (real->double-flonum max-height.0)))
    (define desc-extent : (-> String Integer Integer (Values String Nonnegative-Flonum Nonnegative-Flonum))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send the-dc get-text-extent subdesc font combine?))
        (values subdesc (real->double-flonum width) (real->double-flonum height))))

    (define-values (_ _w phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Flonum) null] [width : Nonnegative-Flonum 0.0] [height : Flonum 0.0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row ([idx0 : Nonnegative-Fixnum 0] [descs : (Listof String) descs] [ys : (Listof Flonum) ys]
                                                     [Widthn : Nonnegative-Flonum width] [yn : Flonum height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Nonnegative-Flonum Flonum Nonnegative-Fixnum)
              ([interval : Nonnegative-Fixnum 1] [open : Nonnegative-Fixnum idx0] [close : Nonnegative-Fixnum terminal]
                                                 [backtracking : String ""] [back-width : Nonnegative-Flonum max-width]
                                                 [back-height : Nonnegative-Flonum phantom-height])
              (define idx : Nonnegative-Fixnum (fxmin close (fx+ open interval)))
              (define next-open : Nonnegative-Fixnum (fx+ open (fxquotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (fl> width max-width) (fx= interval 1) (fl<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (max 0 (sub1 idx))))]
                    [(fl> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(fx= close idx) (values line width height idx)]
                    [else (desc-col-expt (fxlshift interval 1) open close line width height)])))
          (cond [(fl> (fl+ yn heightn) max-height) (values descs ys (max widthn Widthn) yn)]
                [(fx= idx terminal) (values (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]
                [else (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]))))

    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width (max phantom-height height))))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-color fgcolor)))
    (unless (false? bgcolor)
      (define color : Color (select-color bgcolor))
      (send dc set-smoothing 'aligned)
      (send dc set-pen color 0 'transparent)
      (send dc set-brush color 'solid)
      (send dc draw-rectangle 0 0 (max 1 width) (max 1 phantom-height height)))
    (for ([desc (in-list (reverse descs))] [y (in-list (reverse ys))])
      (send dc draw-text desc 0 y combine?))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-frame : (-> Bitmap [#:margin Nonnegative-Real] [#:border Nonnegative-Real] [#:inset Nonnegative-Real]
                           [#:color Color+sRGB] [#:style Pen-Style] [#:background-color Color+sRGB] [#:background-style Brush-Style]
                           Bitmap)
  (lambda [bmp #:margin [margin 0] #:border [border 1] #:inset [inset 0] #:color [pen-color #x000000] #:style [pen-style 'solid]
               #:background-color [brush-color #false] #:background-style [brush-style 'solid]]
    ;;; TODO:
    ; In this aligned bitmap context, border with 0 behaves the same as 1.
    ; It seems that, Racket also deal with 0 width line the "as thin as possible" algorithm.
    (define offset : Nonnegative-Real (+ (max border 1) inset))
    (define width : Positive-Real (+ (send bmp get-width) offset offset))
    (define height : Positive-Real (+ (send bmp get-height) offset offset))
    (define full-width : Positive-Real (+ width margin margin))
    (define full-height : Positive-Real (+ height margin margin))
    (define frame : Bitmap (bitmap-blank full-width full-height (send bmp get-backing-scale)))
    (define dc : (Instance Bitmap-DC%) (send frame make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-color pen-color) border pen-style)
    (cond [(not brush-color) (send dc set-brush (select-color #x000000) 'transparent)]
          [else (send dc set-brush (select-color brush-color) brush-style)])
    (send dc draw-rectangle margin margin width height)
    (send dc draw-bitmap bmp (+ offset margin) (+ offset margin))
    frame))
